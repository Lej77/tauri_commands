use std::{collections::HashMap, iter::once, mem};

use proc_macro2::{Ident, Span, TokenStream as TokenStream2};
use quote::{format_ident, quote, quote_spanned, ToTokens};
use syn::{
    meta::ParseNestedMeta, parse::Parser, punctuated::Punctuated, spanned::Spanned, token,
    AngleBracketedGenericArguments, AttrStyle, Attribute, BareFnArg, Expr, ExprArray, ExprLit,
    Field, FnArg, GenericArgument, ImplItem, ImplItemFn, ImplItemType, Item, ItemFn, ItemImpl,
    ItemTrait, Lit, LitBool, LitStr, Meta, Pat, PatIdent, PatType, Path, PathArguments,
    PathSegment, Receiver, ReturnType, Token, TraitItem, TraitItemFn, Type, TypeImplTrait,
    TypeParamBound, TypePath, TypeTraitObject, TypeTuple, VisRestricted, Visibility,
};

#[derive(Default)]
pub struct TauriCommandsAttributes {
    /// Override the path to the `tauri_commands` crate.
    _crate: Option<TokenStream2>,
    /// Generate an implementation for WebAssembly that calls the Tauri host.
    wasm_client_impl_for: Option<Type>,
    /// Path to a type that also implements the trait, call its methods when a
    /// method has an empty body.
    delegate_empty_methods_to: Option<Type>,
    /// Path to a macro to use for converting arguments when delegating.
    delegate_args_using: Option<Path>,
    /// Path to the module where the macro was invoked.
    module_path: Option<Path>,
    /// Don't rewrite associated types in arguments that refer to the `Self` type.
    skip_rewrite_self: bool,
    /// Ensure async commands return `Result` types.
    fix_async_command_results: bool,
    /// Attributes on methods that should be forwarded to the generated methods
    /// (either command methods when on a trait impl or wasm client methods when
    /// on a trait).
    method_attributes_to_keep: Option<Vec<String>>,
}
impl TauriCommandsAttributes {
    pub fn parser(&mut self) -> impl Parser<Output = ()> + '_ {
        syn::meta::parser(move |meta| self.parse(meta))
    }
    /// Parse attribute macro arguments.
    ///
    /// See
    /// [syn::meta::parser](https://docs.rs/syn/2.0.13/syn/meta/fn.parser.html)
    /// for more info.
    fn parse(&mut self, meta: ParseNestedMeta) -> syn::parse::Result<()> {
        if meta.path.is_ident("crate") {
            self._crate = Some(meta.value()?.parse::<LitStr>()?.value().parse()?);
        } else if meta.path.is_ident("wasm_client_impl_for") {
            self.wasm_client_impl_for = Some(meta.value()?.parse::<Type>()?);
        } else if meta.path.is_ident("delegate_empty_methods_to") {
            self.delegate_empty_methods_to = Some(meta.value()?.parse::<Type>()?);
        } else if meta.path.is_ident("delegate_args_using") {
            self.delegate_args_using = Some(meta.value()?.parse()?);
        } else if meta.path.is_ident("module_path") {
            self.module_path = Some(meta.value()?.parse()?);
        } else if meta.path.is_ident("rewrite_self") {
            self.skip_rewrite_self = !meta.value()?.parse::<LitBool>()?.value();
        } else if meta.path.is_ident("fix_async_command_results") {
            self.fix_async_command_results = meta.value()?.parse::<LitBool>()?.value();
        } else if meta.path.is_ident("method_attributes_to_keep") {
            let array = meta.value()?.parse::<ExprArray>()?;

            // Ensure we set this field to `Some` even if the array was empty:
            let method_attributes_to_keep =
                self.method_attributes_to_keep.get_or_insert_with(Vec::new);

            for item in array.elems {
                let Expr::Lit(ExprLit {
                    lit: Lit::Str(literal),
                    ..
                }) = item
                else {
                    return Err(syn::Error::new(
                        item.span(),
                        "only string literals is supported for \"method_attributes_to_keep\" property",
                    ));
                };
                method_attributes_to_keep.push(literal.value());
            }
        } else {
            return Err(meta.error("unsupported property in tauri_commands attribute macro"));
        }

        Ok(())
    }
    pub fn expand(&self, item: Item) -> (bool, TokenStream2) {
        let mut errors = Vec::new();
        let (keep_original, mut out) = self.try_expand(item, &mut errors);
        let mut errors = errors.into_iter();
        if let Some(mut error) = errors.next() {
            errors.for_each(|e| error.combine(e));
            out.extend(error.to_compile_error());
        }
        (keep_original, out)
    }
    pub fn try_expand(&self, item: Item, errors: &mut Vec<syn::Error>) -> (bool, TokenStream2) {
        match item {
            Item::Impl(item) => (false, self.try_expand_host_commands(item, errors)),
            Item::Trait(mut item) => {
                let wasm_client = self.try_expand_wasm_client(&mut item, errors);
                let helper_macro = self.try_expand_docs_helper_macro(&item);
                let mut out = item.into_token_stream();
                out.extend(helper_macro);
                out.extend(wasm_client);
                (false, out)
            }
            _ => {
                errors.push(syn::Error::new(item.span(), "tauri_commands attribute can only be applied to traits and trait implementations"));
                (true, TokenStream2::new())
            }
        }
    }
    pub fn try_expand_docs_helper_macro(&self, item: &ItemTrait) -> TokenStream2 {
        let mut body = TokenStream2::new();
        for item in &item.items {
            let TraitItem::Fn(item) = item else { continue };
            let docs_attrs = item.attrs.iter().filter(|attr| {
                if let Meta::NameValue(p) = &attr.meta {
                    p.path.is_ident("doc")
                } else {
                    false
                }
            });

            let fn_name = &item.sig.ident;
            let literal: LitStr = LitStr::new(&fn_name.to_string(), fn_name.span());
            body.extend(quote!(
                (#literal, $($token:tt)*) => {
                    #(#docs_attrs)*
                    $($token)*
                };
            ))
        }

        let name = &item.ident;
        let private_name = format_ident!("__private_docs_helper_{}", item.ident);
        let export = matches!(item.vis, Visibility::Public(_)).then(|| quote!(#[macro_export]));
        let vis = &item.vis;
        quote!(
            #[doc(hidden)]
            #[allow(unused_macros)]
            #export // <- Need #[macro_export] if "pub"
            macro_rules! #private_name {
                #body
                // Fallback with nicer error message:
                ($name:literal, $($rest:tt)*) => {
                    ::core::compile_error! {::core::concat!{ "Can't find docs for method with the name: ", $name } }
                }
            }
            // Export with same name as the trait (hopefully it will be imported in the same places):
            #[doc(hidden)]
            #vis use #private_name as #name;
        )
    }

    /// Detect if a type is using an associated type on the `Self` type.
    fn associated_type<'a>(ty: &'a Type, trait_ident: &Ident) -> Option<&'a Path> {
        let mut ty: &Type = ty;
        loop {
            match ty {
                Type::Group(syn::TypeGroup { elem, .. }) => ty = elem,
                Type::Paren(syn::TypeParen { elem, .. }) => ty = elem,
                Type::Path(syn::TypePath { qself, path }) if path.leading_colon.is_none() => {
                    if matches!(path.segments.first(), Some(seg) if seg.ident == *trait_ident) {
                        // Path start with the trait's name so it is an item in
                        // the current trait.
                        return Some(path);
                    }
                    if let Some(qself) = qself {
                        // Check the self qualifier:
                        let Type::Path(TypePath { qself: None, path }) = &*qself.ty else {
                            break;
                        };
                        if path.is_ident(&Ident::from(<Token![Self]>::default())) {
                            // Qualified as item of `Self`
                            return Some(path);
                        }
                    } else if matches!(path.segments.first(), Some(seg) if seg.ident == Ident::from(<Token![Self]>::default()))
                    {
                        // Path start with Self so it is likely an item in the current
                        // trait.
                        return Some(path);
                    }
                    break;
                }
                // Not a type path so can't be an associated type.
                _ => break,
            }
        }
        None
    }

    pub fn try_expand_wasm_client(
        &self,
        item: &mut ItemTrait,
        errors: &mut Vec<syn::Error>,
    ) -> TokenStream2 {
        if !cfg!(feature = "tauri-import") {
            return TokenStream2::new();
        }
        let Some(client_ty_path) = &self.wasm_client_impl_for else {
            return TokenStream2::new();
        };
        let ItemTrait {
            attrs: original_attrs,
            vis: _,
            unsafety,
            auto_token,
            restriction: _,
            trait_token: _,
            ident,
            generics,
            colon_token: _,
            supertraits: _,
            brace_token: _,
            items,
        } = item;

        // Check trait signature:
        if let Some(unsafety) = &unsafety {
            errors.push(syn::Error::new(
                unsafety.span(),
                "can't generate WASM client for unsafe trait",
            ));
        }
        if let Some(auto_token) = &auto_token {
            errors.push(syn::Error::new(
                auto_token.span(),
                "can't generate WASM client for auto trait",
            ));
        }

        // Tokens:
        let _crate = self
            ._crate
            .clone()
            .unwrap_or_else(|| quote!(::tauri_commands));

        // The generated trait impl:
        let mut generated = ItemImpl {
            // Forward `async_trait` attribute if it is present:
            attrs: original_attrs
                .iter()
                .filter(|attr| {
                    let path = match &attr.meta {
                        // #[async_trait::async_trait]
                        Meta::Path(p) => p,

                        // #[async_trait::async_trait(?Send)]
                        Meta::List(list) => &list.path,

                        _ => return false,
                    };
                    let Some(seg) = path.segments.last() else {
                        return false;
                    };
                    seg.ident == "async_trait"
                })
                .cloned()
                .collect(),
            defaultness: None,
            unsafety: None,
            impl_token: Default::default(),
            generics: generics.clone(),
            trait_: Some((None, ident.clone().into(), Default::default())),
            self_ty: Box::new(client_ty_path.clone()),
            brace_token: Default::default(),
            items: Vec::new(),
        };
        // Some arguments might not be used by the generated methods:
        generated
            .attrs
            .push(syn::parse_quote!(#[allow(unused_variables)]));

        // When targeting WebAssembly emit code to invoke Tauri command:

        let mut wasm_items = Vec::new();
        for item in &mut *items {
            match item {
                TraitItem::Fn(item) => {
                    // Keep some attributes:
                    let attrs = {
                        let mut attrs = Vec::new();
                        let allowed_attributes =
                            self.method_attributes_to_keep.clone().unwrap_or_else(|| {
                                vec![
                                    // https://docs.rs/tracing/latest/tracing/attr.instrument.html
                                    "instrument".to_owned(),
                                ]
                            });
                        attrs.extend(
                            original_attrs
                                .iter()
                                .filter(|attr| {
                                    matches!(attr.meta.path().segments.last(), Some(seg) if allowed_attributes.iter().any(|text| seg.ident == text))
                                })
                                .cloned(),
                        );
                        attrs
                    };

                    if item.sig.asyncness.is_none() {
                        // Calling Tauri is always async so change the trait when targeting WebAssembly:

                        // Async method for wasm target:
                        let mut wasm_item = item.clone();
                        wasm_item.sig.asyncness = Some(Default::default());
                        wasm_item
                            .attrs
                            .push(syn::parse_quote!(#[cfg(target_family = "wasm")]));
                        wasm_items.push(TraitItem::Fn(wasm_item));

                        // Original method only for non wasm (modify after we cloned):
                        item.attrs
                            .push(syn::parse_quote!(#[cfg(not(target_family = "wasm"))]));
                    }

                    let TraitItemFn {
                        attrs: _,
                        sig,
                        default: _,
                        semi_token: _,
                    } = item;

                    let sig_name = LitStr::new(&sig.ident.to_string(), Span::call_site());
                    let ret_ty = if let ReturnType::Type(_, ty) = &sig.output {
                        (**ty).clone()
                    } else {
                        Type::Tuple(TypeTuple {
                            paren_token: syn::token::Paren::default(),
                            elems: Default::default(),
                        })
                    };
                    let err_msg = LitStr::new(
                        &format!("failed to invoke Tauri command \"{}\"", sig_name.value()),
                        Span::call_site(),
                    );
                    let serde_path =
                        LitStr::new(&format!("{_crate}::__private::serde"), Span::call_site());

                    let mut args_expr = Punctuated::<Ident, Token![,]>::new();
                    let mut args_def = Punctuated::<Field, Token![,]>::new();
                    'args: for arg in &sig.inputs {
                        match arg {
                            FnArg::Receiver(_) => {
                                // Ignore receiver types (they can be useful to
                                // maybe allow dynamic dispatch).
                                continue 'args;
                            }
                            FnArg::Typed(PatType { pat, ty, .. }) => {
                                // Detect arguments that use associated types
                                // and ignore them:
                                if Self::associated_type(ty, ident).is_some() {
                                    // Skip this argument since its an
                                    // associated type to the trait:
                                    continue 'args;
                                }
                                if let syn::Pat::Ident(PatIdent { ident, .. }) = &**pat {
                                    args_expr.push(ident.clone());
                                    args_def.push(Field {
                                        attrs: Vec::new(),
                                        vis: Visibility::Inherited,
                                        mutability: syn::FieldMutability::None,
                                        ident: Some(ident.clone()),
                                        colon_token: Some(Token![:](Span::call_site())),
                                        ty: (**ty).clone(),
                                    });
                                    continue 'args;
                                }
                            }
                        }
                        errors.push(syn::Error::new_spanned(
                            arg.clone(),
                            "only normal arguments without patterns is supported",
                        ));
                    }

                    let generics = &sig.generics;
                    let (_, ty_generics, _) = generics.split_for_impl();
                    let ty_generics_turbo = ty_generics.as_turbofish();

                    let wasm_body: syn::Block = syn::parse2(quote!({
                        // Store function arguments inside deserializable struct:
                        #[::core::prelude::v1::derive(#_crate::__private::serde::Serialize)]
                        #[serde(crate = #serde_path)]
                        // Note: Tauri renames the arguments to camelCase to better
                        // support Javascript, see:
                        // https://tauri.app/v1/guides/features/command/#passing-arguments
                        #[serde(rename_all = "camelCase")]
                        struct __TauriArguments #generics {
                            // Ensure there aren't any unused generic parameters:
                            #[serde(skip)]
                            __marker: ::core::marker::PhantomData<fn() -> __TauriArguments #ty_generics>,
                            #args_def
                        }

                        ::core::result::Result::expect(
                            #_crate::__private::TauriCommandSpecialization::<#ret_ty>::new()
                                .__invoke_tauri_command(
                                    #sig_name,
                                    // Specify generics in case they can't be inferred.
                                    &__TauriArguments #ty_generics_turbo {
                                        __marker: ::core::marker::PhantomData,
                                        #args_expr
                                    }
                                )
                                .await,
                            #err_msg
                        )
                    })).expect("failed to generate body for WebAssembly implementation");

                    generated.items.push(ImplItem::Fn(ImplItemFn {
                        attrs,
                        vis: Visibility::Inherited,
                        defaultness: None,
                        sig: sig.clone(),
                        block: wasm_body,
                    }));
                }
                // Set all associated types to the unit type:
                TraitItem::Type(item) => {
                    generated.items.push(ImplItem::Type(ImplItemType {
                        attrs: Vec::new(),
                        vis: Visibility::Inherited,
                        defaultness: None,
                        type_token: Default::default(),
                        ident: item.ident.clone(),
                        generics: item.generics.clone(),
                        eq_token: Default::default(),
                        // Unit type:
                        ty: Type::Tuple(TypeTuple {
                            elems: Default::default(),
                            paren_token: Default::default(),
                        }),
                        semi_token: Default::default(),
                    }));
                }
                TraitItem::Const(item) => {
                    errors.push(syn::Error::new(
                        item.span(),
                        "can't generate WASM client for trait with constant",
                    ));
                }
                TraitItem::Macro(item) => {
                    errors.push(syn::Error::new(
                        item.span(),
                        "can't generate WASM client for trait with macro inside its body",
                    ));
                }
                _ => {
                    errors.push(syn::Error::new(
                        item.span(),
                        "could not parse part of the trait definition's body",
                    ));
                }
            }
        }

        items.extend(wasm_items);

        quote!(
            #[cfg(target_family = "wasm")]
            #generated
        )
    }
    pub fn try_expand_host_commands(
        &self,
        mut item: ItemImpl,
        errors: &mut Vec<syn::Error>,
    ) -> TokenStream2 {
        if !cfg!(feature = "tauri-export") {
            return item.into_token_stream();
        }
        let ItemImpl {
            attrs: _,
            defaultness: _,
            unsafety: _,
            impl_token,
            generics: _,
            trait_,
            self_ty,
            brace_token: _,
            items,
        } = &mut item;

        if trait_.is_none() {
            errors.push(syn::Error::new(
                impl_token.span(),
                "should be trait implementation",
            ));
        }

        // Tokens:
        let _crate = self
            ._crate
            .clone()
            .unwrap_or_else(|| quote!(::tauri_commands));

        // The generated trait impl:
        let mut generated = Vec::<TokenStream2>::new();

        let mut generated_cmd_names: Punctuated<TokenStream2, Token![,]> = Punctuated::new();

        /*
        let mut associated_types = HashMap::new();
        for item in &*items {
            let ImplItem::Type(item) = item else { continue };
            associated_types.insert(item.ident.to_string(), item.ty.clone());
        }
        */

        for item in items {
            let ImplItem::Fn(item) = item else { continue };
            let ImplItemFn {
                attrs: original_attrs,
                vis: _,
                defaultness: _,
                sig,
                block,
            } = item;
            generated_cmd_names.push(sig.ident.to_token_stream());

            let mut attrs = Vec::new();
            attrs.push(syn::parse_quote!(#[cfg(not(target_family = "wasm"))]));

            // Allow forwarding arguments to Tauri's command attribute by
            // checking for an existing attribute that ends with "command":
            let command_attr = original_attrs.iter().find(
                |a| matches!(a.meta.path().segments.last(), Some(seg) if seg.ident == "command"),
            ).cloned();
            attrs.push(command_attr.unwrap_or_else(|| {
                Attribute {
                    bracket_token: Default::default(),
                    meta: Meta::Path(
                        // Note: we assume the invoker has tauri as a direct
                        // dependency (this allows us to work with both version
                        // 1 and 2 of Tauri.)
                        syn::parse2(quote!(tauri::command))
                            .expect("failed to generate tauri::command attribute"),
                    ),
                    pound_token: Default::default(),
                    style: AttrStyle::Outer,
                }
            }));

            // Allow manual usage of specta even if `specta` feature is disabled:
            let specta_attr = original_attrs
                .iter()
                .find(
                    |a| matches!(a.meta.path().segments.last(), Some(seg) if seg.ident == "specta"),
                )
                .cloned();
            attrs.extend(specta_attr.or_else(|| {
                if cfg!(feature = "specta") {
                    Some(Attribute {
                        bracket_token: Default::default(),
                        meta: Meta::Path(
                            // Note: assumes invoker has specta as a direct dependency.
                            syn::parse2(quote!(specta::specta))
                                .expect("failed to generate specta attribute"),
                        ),
                        pound_token: Default::default(),
                        style: AttrStyle::Outer,
                    })
                } else {
                    None
                }
            }));

            // Keep docs (and some other attributes):
            let allowed_attributes = self.method_attributes_to_keep.clone().unwrap_or_else(|| {
                vec![
                    // https://docs.rs/tracing/latest/tracing/attr.instrument.html
                    "instrument".to_owned(),
                ]
            });
            attrs.extend(
                original_attrs
                    .iter()
                    .filter(|attr| {
                        if let Meta::NameValue(p) = &attr.meta {
                            p.path.is_ident("doc")
                        } else {
                            matches!(attr.meta.path().segments.last(), Some(seg) if allowed_attributes.iter().any(|text| seg.ident == text))
                        }
                    })
                    .cloned(),
            );

            let mut gen_sig = sig.clone();

            let changed_return_type = 'fix_return_type: {
                if !self.fix_async_command_results || gen_sig.asyncness.is_none() {
                    break 'fix_return_type false;
                }
                let ok_type = match &mut gen_sig.output {
                    // Even functions that don't return anything need to return
                    // results (we return `Result<(), ()>`):
                    ReturnType::Default => Type::Tuple(TypeTuple {
                        elems: Punctuated::new(),
                        paren_token: Default::default(),
                    }),
                    ReturnType::Type(_, ty) => {
                        if let Type::Path(TypePath { path, .. }) = &**ty {
                            if let Some(last) = path.segments.last() {
                                if last.ident == "Result" {
                                    // This is likely already a result type
                                    break 'fix_return_type false;
                                }
                            }
                        }
                        std::mem::replace(&mut **ty, Type::Verbatim(TokenStream2::new()))
                    }
                };

                // Path to the result type in core:
                let mut result_path = Path {
                    leading_colon: Some(Default::default()),
                    segments: ["core", "result", "Result"]
                        .into_iter()
                        .map(|name| Ident::new(name, Span::call_site()))
                        .map(|ident| PathSegment {
                            ident,
                            arguments: Default::default(),
                        })
                        .collect(),
                };
                let result_seg = result_path.segments.last_mut().unwrap();

                // The brackets after the Result type, like in `Result<OriginalType, ()>`
                result_seg.arguments =
                    PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                        colon2_token: None,
                        lt_token: Default::default(),
                        args: [
                            // Ok type:
                            ok_type,
                            // Err type `()``
                            Type::Tuple(TypeTuple {
                                elems: Punctuated::new(),
                                paren_token: Default::default(),
                            }),
                        ]
                        .into_iter()
                        .map(GenericArgument::Type)
                        .collect(),
                        gt_token: Default::default(),
                    });

                // Replace the original type:
                gen_sig.output = ReturnType::Type(
                    if let ReturnType::Type(arrow, _) = gen_sig.output {
                        arrow
                    } else {
                        Default::default()
                    },
                    Box::new(Type::Path(TypePath {
                        qself: None,
                        path: result_path,
                    })),
                );
                true
            };

            // Ignore receivers for commands since free standing functions can't
            // have them:
            gen_sig.inputs = gen_sig
                .inputs
                .into_iter()
                .filter(|arg| !matches!(arg, FnArg::Receiver(_)))
                .collect();

            // Fix usage `Self` to refer to associated types:
            'fix_self: {
                if self.skip_rewrite_self {
                    break 'fix_self;
                }
                let Some((_, trait_path, _)) = &*trait_ else {
                    break 'fix_self;
                };
                let Some(_trait_ident) = trait_path.segments.last() else {
                    break 'fix_self;
                };

                for arg in &mut gen_sig.inputs {
                    let FnArg::Typed(arg) = arg else { continue };

                    SelfRewriter::new(self_ty, trait_path).rewrite_self_in_type(&mut arg.ty);

                    // Previous work to rewrite associated type to actual concrete type:
                    /*
                    let Some(assoc) = Self::associated_type(&arg.ty, &trait_ident.ident) else {
                        continue;
                    };
                    let Some(name) = assoc.segments.last() else {
                        continue;
                    };
                    let name = name.ident.to_string();
                    let Some(actual_ty) = associated_types.get(&name) else {
                        continue;
                    };
                    *arg.ty = actual_ty.clone();
                    */
                }
            };
            // If the implementation is empty then maybe delegate to another type:
            if let (Some(delegate_to), true) =
                (&self.delegate_empty_methods_to, block.stmts.is_empty())
            {
                let mut named_args = Vec::new();
                let args: Punctuated<TokenStream2, Token![,]> = sig
                    .inputs
                    .iter()
                    .map(|arg| match arg {
                        // Create a receiver type if needed since we can't
                        // forward self in the command function:
                        FnArg::Receiver(Receiver {
                            reference,
                            mutability,
                            ..
                        }) => match (reference.is_some(), mutability.is_some()) {
                            (true, true) => quote!(&mut #delegate_to),
                            (true, false) => quote!(&#delegate_to),
                            (false, _) => quote!(#delegate_to),
                        },
                        FnArg::Typed(PatType { pat, .. }) => match &**pat {
                            Pat::Ident(PatIdent { ident, .. }) => {
                                named_args.push(ident.clone());
                                ident.to_token_stream()
                            }
                            _ => {
                                errors.push(syn::Error::new(
                                    pat.span(),
                                    "only identifiers are allowed in function arguments",
                                ));
                                quote!(::core::panic!())
                            }
                        },
                    })
                    .collect();
                let convert_args = if let Some(convert_arg) = &self.delegate_args_using {
                    named_args
                        .iter()
                        .map(|ident| {
                            let lit = LitStr::new(&ident.to_string(), ident.span());
                            quote!(#convert_arg!(#lit, #ident);)
                        })
                        .collect()
                } else {
                    TokenStream2::new()
                };
                let ty_to_call_on = if let Some((_, trait_path, _)) = &*trait_ {
                    quote!(<#delegate_to as #trait_path>)
                } else {
                    quote!(#delegate_to)
                };
                let fn_name = &sig.ident;
                let maybe_async = sig.asyncness.is_some().then(|| quote!(.await));
                *block = syn::parse2(quote!({
                    #convert_args
                    #ty_to_call_on::#fn_name(#args) #maybe_async
                }))
                .expect("failed to delegate empty method to another type");
            }

            // The methods in the trait should call the generated command
            // methods (to not duplicate code too much)
            let mut block = if changed_return_type {
                // If we changed the return type we currently don't delegate to
                // the command function (instead both impls will look the same):
                block.clone()
            } else {
                std::mem::replace(block, {
                    let args: Punctuated<TokenStream2, Token![,]> = sig
                        .inputs
                        .iter()
                        .filter_map(|arg| match arg {
                            // Receivers were filtered out and so aren't needed for the command fn:
                            FnArg::Receiver(_) => None,
                            FnArg::Typed(PatType { pat, .. }) => match &**pat {
                                Pat::Ident(PatIdent { ident, .. }) => Some(ident.to_token_stream()),
                                _ => {
                                    errors.push(syn::Error::new(
                                        pat.span(),
                                        "only identifiers are allowed in function arguments",
                                    ));
                                    // Insert panic so that the arguments are still
                                    // on the right "index":
                                    Some(quote!(::core::panic!()))
                                }
                            },
                        })
                        .collect();
                    let cmd_name = &gen_sig.ident;
                    let maybe_async = gen_sig.asyncness.is_some().then(|| quote!(.await));
                    syn::parse2(quote!({
                        #cmd_name(#args) #maybe_async
                    }))
                    .expect("failed to delegate trait method to generated command function")
                })
            };

            if changed_return_type {
                block = syn::parse2(quote!( {::core::result::Result::Ok(#block)} ))
                    .expect("failed to ok wrap return value of async command");
            }

            let generated_fn = ItemFn {
                attrs,
                // specta macro will cause compile error if applied to a public
                // command. So lets make it `pub(crate)`.
                vis: Visibility::Restricted(VisRestricted {
                    pub_token: Default::default(),
                    paren_token: Default::default(),
                    in_token: Default::default(),
                    path: Box::new(Path {
                        leading_colon: None,
                        segments: Some(PathSegment {
                            arguments: Default::default(),
                            ident: Ident::from(<Token![crate]>::default()),
                        })
                        .into_iter()
                        .collect(),
                    }),
                }),
                sig: gen_sig,
                block: Box::new(block),
            };
            generated.push(if let Some((_, trait_path, _)) = &*trait_ {
                let literal: LitStr = LitStr::new(&sig.ident.to_string(), sig.ident.span());

                // Use helper macro to add documentation from trait definition
                // to command:
                quote!(
                    #trait_path! { #literal, #generated_fn }
                )
            } else {
                generated_fn.into_token_stream()
            });
        }

        let macro_name = match (&trait_, &**self_ty) {
            // Prefer trait name over self_ty name:
            (Some((_, path, _)), _) | (_, Type::Path(TypePath { qself: _, path })) => {
                path.segments.last().and_then(|last| {
                    if !last.arguments.is_empty() {
                        return None;
                    }
                    Some(format_ident!("with_commands_for_{}", last.ident))
                })
            }
            _ => None,
        };

        let mut out = item.into_token_stream();
        out.extend(generated);

        if let Some(macro_name) = macro_name {
            let private_name = format_ident!("__{}", macro_name);
            if let Some(module_path) = &self.module_path {
                for tokens in &mut generated_cmd_names {
                    let cmd_name = std::mem::take(tokens);
                    *tokens = quote!(#module_path::#cmd_name);
                }
            }
            out.extend(quote!(
                /// A macro that will call another macro with the names of all generated Tauri commands.
                #[cfg(not(target_family = "wasm"))]
                macro_rules! #private_name {
                    // Allow callback paths with leading colons:
                    ({$($callback:tt)*} $(then $( {$($callback_rest:tt)*} )then* )? $(with $($forward:tt)* )?) => {
                        $($callback)*! { $($( {$($callback_rest)*} )then* with)?  $(   $($forward)*  ,)?  #generated_cmd_names }
                    };
                    // Simpler syntax when leading colons aren't needed:
                    ($($callback:ident)::+ $(then $( $($callback_rest:ident)::* )then* )? $(with $($forward:tt)* )?) => {
                        $($callback)::*! { $($( $($callback_rest)::* )then* with)?  $(  $($forward)*  ,)?  #generated_cmd_names }
                    };
                }

                // Allow normal namespace rules to apply to macros:
                #[cfg(not(target_family = "wasm"))]
                pub(crate) use #private_name as #macro_name;
            ));
        }

        out
    }
}

struct SelfRewriter<'a> {
    self_ty: &'a Type,
    trait_path: &'a Path,
    self_ident: Ident,
}
impl<'a> SelfRewriter<'a> {
    fn new(self_ty: &'a Type, trait_path: &'a Path) -> Self {
        Self {
            self_ty,
            trait_path,
            self_ident: Ident::from(<Token![Self]>::default()),
        }
    }
    fn rewrite_self_in_expr(&self, _expr: &mut Expr) {
        // For now we don't parse any expressions
    }
    fn rewrite_self_in_generic(&self, generic: &mut AngleBracketedGenericArguments) {
        for arg in &mut generic.args {
            match arg {
                GenericArgument::Lifetime(_) => (),
                GenericArgument::Type(ty) => {
                    self.rewrite_self_in_type(ty);
                }
                GenericArgument::Const(arg) => {
                    self.rewrite_self_in_expr(arg);
                }
                GenericArgument::AssocType(arg) => {
                    if let Some(generics) = &mut arg.generics {
                        self.rewrite_self_in_generic(generics);
                    }
                    self.rewrite_self_in_type(&mut arg.ty);
                }
                GenericArgument::AssocConst(arg) => {
                    if let Some(generics) = &mut arg.generics {
                        self.rewrite_self_in_generic(generics);
                    }
                    self.rewrite_self_in_expr(&mut arg.value);
                }
                GenericArgument::Constraint(arg) => {
                    if let Some(generics) = &mut arg.generics {
                        self.rewrite_self_in_generic(generics);
                    }
                    for bound in &mut arg.bounds {
                        let syn::TypeParamBound::Trait(bound) = bound else {
                            continue;
                        };
                        self.rewrite_self_in_path(&mut bound.path);
                    }
                }
                _ => (),
            }
        }
    }
    fn rewrite_self_in_path(&self, path: &mut Path) {
        for seg in &mut path.segments {
            match &mut seg.arguments {
                PathArguments::None => {}
                PathArguments::AngleBracketed(args) => {
                    self.rewrite_self_in_generic(args);
                }
                PathArguments::Parenthesized(arg) => {
                    if let ReturnType::Type(_, return_ty) = &mut arg.output {
                        self.rewrite_self_in_type(&mut *return_ty);
                    }
                    for input in &mut arg.inputs {
                        self.rewrite_self_in_type(input);
                    }
                }
            }
        }
    }
    fn rewrite_self_in_type(&self, mut ty: &mut Type) {
        loop {
            match ty {
                Type::Infer(_) => break,
                Type::Macro(_) => break,
                Type::Never(_) => break,
                Type::Verbatim(_) => break,
                Type::Array(v) => ty = &mut v.elem,
                Type::Group(v) => ty = &mut *v.elem,
                Type::Paren(v) => ty = &mut *v.elem,
                Type::Reference(v) => ty = &mut *v.elem,
                Type::Ptr(v) => ty = &mut *v.elem,
                Type::Slice(v) => ty = &mut *v.elem,
                Type::BareFn(v) => {
                    for arg in &mut v.inputs {
                        self.rewrite_self_in_type(&mut arg.ty);
                    }
                    let ReturnType::Type(_, ret_ty) = &mut v.output else {
                        return;
                    };
                    ty = &mut *ret_ty;
                }
                Type::Tuple(v) => {
                    for elem in &mut v.elems {
                        self.rewrite_self_in_type(elem);
                    }
                    break;
                }
                Type::ImplTrait(TypeImplTrait { bounds, .. })
                | Type::TraitObject(TypeTraitObject { bounds, .. }) => {
                    for bound in bounds {
                        let syn::TypeParamBound::Trait(bound) = bound else {
                            continue;
                        };
                        self.rewrite_self_in_path(&mut bound.path);
                    }
                    break;
                }
                // `Self`:
                Type::Path(TypePath { qself: None, path })
                    if path.segments.len() == 1 && path.is_ident(&self.self_ident) =>
                {
                    *ty = self.self_ty.clone();
                }
                Type::Path(TypePath { qself, path }) => {
                    if qself.is_none() && path.segments.len() > 1 {
                        let Some(first) = path.segments.first_mut() else {
                            break;
                        };
                        if first.ident == self.self_ident {
                            // Was: Self::Something
                            // Want: <Self as Trait>::Something
                            *qself = Some(syn::QSelf {
                                lt_token: Default::default(),
                                ty: Box::new(Type::Path(TypePath {
                                    qself: None,
                                    path: Path {
                                        leading_colon: None,
                                        segments: once(PathSegment {
                                            arguments: Default::default(),
                                            ident: first.ident.clone(),
                                        })
                                        .collect(),
                                    },
                                })),
                                position: self.trait_path.segments.len(),
                                as_token: Default::default(),
                                gt_token: Default::default(),
                            });
                            // Replace beginning of path with the trait:
                            path.segments = self
                                .trait_path
                                .segments
                                .iter()
                                // Remove `Self` from `Self::Path::To::Thing`:
                                .chain(path.segments.iter().skip(1))
                                .cloned()
                                .collect();
                        }
                    }
                    self.rewrite_self_in_path(path);
                    let Some(qself) = qself else { break };
                    ty = &mut qself.ty
                }
                _ => break,
            }
        }
    }
}
