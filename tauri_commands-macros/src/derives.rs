use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2, TokenTree};
use quote::{quote, ToTokens};
use syn::{parse_macro_input, Attribute, DeriveInput, Expr, ExprAssign, LitStr, Meta, MetaList};

#[derive(Default, Debug)]
struct DeriveAttributes {
    /// Contains the path to the `tauri_commands` crate as specified by a
    /// `#[tauri_serde(crate = path::to::tauri_commands)]` attribute.
    crate_: Option<TokenStream2>,
    /// `true` if there is already a `#[serde(crate = "path::to::serde")`
    /// attribute on the type.
    has_inner_crate_attr: bool,
}
impl DeriveAttributes {
    pub fn crate_path(&self) -> TokenStream2 {
        self.crate_
            .clone()
            .unwrap_or_else(|| quote!(::tauri_commands))
    }
}

#[allow(clippy::enum_variant_names)]
pub enum DeriveInfo {
    TauriSerialize,
    TauriDeserialize,
    TauriType,
}
impl DeriveInfo {
    fn macro_name(&self) -> &'static str {
        match self {
            DeriveInfo::TauriSerialize => "TauriSerialize",
            DeriveInfo::TauriDeserialize => "TauriDeserialize",
            DeriveInfo::TauriType => "TauriType",
        }
    }
    fn custom_attr(&self) -> &'static str {
        if let Self::TauriType = self {
            "tauri_type"
        } else {
            "tauri_serde"
        }
    }
    fn is_serde_derive(&self) -> bool {
        matches!(self, Self::TauriSerialize | Self::TauriDeserialize)
    }
    fn parse_crate_attr(&self, attr: &Attribute) -> syn::Result<TokenStream2> {
        let value: Expr = attr.parse_args()?;
        if let Expr::Assign(ExprAssign { left, right, .. }) = value {
            if left.to_token_stream().to_string().trim() == "crate" {
                return Ok(right.to_token_stream());
            }
        }
        Err(syn::Error::new_spanned(
            attr,
            "expected a key-value pair, such as \"crate = path::to::crate\"",
        ))
    }
    fn parse_derive_attributes(&self, item: &DeriveInput) -> Result<DeriveAttributes, syn::Error> {
        let mut info = DeriveAttributes::default();
        for attr in &item.attrs {
            if attr.path().is_ident(match self {
                Self::TauriDeserialize | Self::TauriSerialize => "serde",
                Self::TauriType => "specta",
            }) {
                if matches!(&attr.meta, Meta::List(MetaList { tokens, .. }) if matches!(
                    tokens.clone().into_iter().next(), Some(TokenTree::Ident(ident)) if ident == "crate"
                )) {
                    info.has_inner_crate_attr = true;
                }
                continue;
            } else if !attr.path().is_ident(self.custom_attr()) {
                continue;
            }
            match self.parse_crate_attr(attr) {
                Ok(path) => {
                    info.crate_ = Some(path);
                }
                Err(mut e) => {
                    e.combine(syn::Error::new_spanned(
                        attr,
                        format_args!(
                            "tauri_commands::{} failed to parse {} attribute, attribute content was: {}",
                            self.macro_name(), self.custom_attr(), attr.clone().into_token_stream()
                        ),
                    ));
                    return Err(e);
                }
            }
        }
        Ok(info)
    }
    fn get_derive_attributes(&self, input: DeriveInput) -> TokenStream2 {
        // Parse container attributes:
        let derive_attr = match self.parse_derive_attributes(&input) {
            Ok(v) => v,
            Err(e) => {
                let mut err_out = input.into_token_stream();
                err_out.extend(e.into_compile_error());
                return err_out;
            }
        };

        // Path to tauri_commands crate:
        let crate_ = derive_attr.crate_path();

        // Conditionally add serde derive:
        let mut output = TokenStream2::new();

        // Ensure the complier ignores `serde` attributes on the type:
        let noop_derive_name = if self.is_serde_derive() {
            quote!(#crate_::__private::AllowSerde)
        } else {
            quote!(#crate_::__private::NoopSpectaType)
        };
        output.extend(quote!(#[::core::prelude::v1::derive(#noop_derive_name)]));

        let _re_export_path = LitStr::new(
            &format!(
                "{}::__private::{}",
                if matches!(self, Self::TauriType) {
                    crate_
                        .to_string()
                        // `specta::Type` macro can't handle paths that start with `::`
                        .trim_start_matches(|c: char| c.is_whitespace() || c == ':')
                        .to_string()
                } else {
                    crate_.to_string()
                },
                match self {
                    Self::TauriDeserialize | Self::TauriSerialize => "serde",
                    Self::TauriType => "specta",
                }
            ),
            Span::call_site(),
        );

        if self.is_serde_derive() {
            let _serde_derive = if matches!(self, Self::TauriSerialize) {
                quote!(Serialize)
            } else {
                quote!(Deserialize)
            };

            #[cfg(feature = "tauri-export")]
            {
                output.extend(quote!(
                    #[cfg_attr(
                        not(target_family = "wasm"),
                        ::core::prelude::v1::derive(#crate_::__private::serde::#_serde_derive))
                    ]
                ));
            }
            #[cfg(feature = "tauri-import")]
            {
                output.extend(quote!(
                    #[cfg_attr(
                        target_family = "wasm",
                        ::core::prelude::v1::derive(#crate_::__private::serde::#_serde_derive))
                    ]
                ));
            }

            // If we emit too many `serde(crate = "")` attributes then we will get
            // the following error: "duplicate serde attribute `crate`"
            if !derive_attr.has_inner_crate_attr {
                const_cfg!(
                    if cfg!(all(feature = "tauri-export", feature = "tauri-import")) {
                        output.extend(quote!(
                            #[serde(crate = #_re_export_path)]
                        ));
                    } else {
                        #[cfg(feature = "tauri-export")]
                        {
                            output.extend(quote!(
                                #[cfg_attr(
                                    not(target_family = "wasm"),
                                    serde(crate = #_re_export_path))
                                ]
                            ));
                        }
                        #[cfg(feature = "tauri-import")]
                        {
                            output.extend(quote!(
                                #[cfg_attr(
                                    target_family = "wasm",
                                    serde(crate = #_re_export_path))
                                ]
                            ));
                        }
                    }
                );
            }
        } else if cfg!(feature = "specta") {
            output.extend(quote!(
                #[::core::prelude::v1::derive(specta::Type)]
            ));

            // Note: we no longer re-export specta, so ignore the below code:

            // Don't override re-export path if already set:
            //if !derive_attr.has_inner_crate_attr {
            //    output.extend(quote!(
            //        #[specta(crate = #_re_export_path)]
            //    ));
            //}
        }

        output
    }

    pub fn parse_and_get_attributes(&self, args: TokenStream2, item: DeriveInput) -> TokenStream2 {
        // Parse attribute args:
        if !args.is_empty() {
            return syn::Error::new_spanned(
                args,
                format!(
                    "tauri_commands::{} attribute doesn't take any arguments",
                    self.macro_name()
                ),
            )
            .into_compile_error();
        }

        self.get_derive_attributes(item)
    }

    pub fn parse_and_add_derives(&self, args: TokenStream2, item: TokenStream) -> TokenStream {
        let mut output: TokenStream = {
            let item = item.clone();
            let item = parse_macro_input!(item as DeriveInput);

            self.parse_and_get_attributes(args, item).into()
        };

        // Add the item the macro was applied to last:
        //
        // Don't convert the original item to a `proc_macro2::TokenStream` in
        // the hopes of preserving more information.
        output.extend(item);

        // Replace input with modified tokens:
        output
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_serde_attributes() {
        let item: DeriveInput = syn::parse2(quote!(
            #[serde(crate = "path::to::serde")]
            #[tauri_serde(crate = some_other_path)]
            struct AType {
                field: String,
            }
        ))
        .expect("failed to parse test data");

        let attr = DeriveInfo::TauriSerialize
            .parse_derive_attributes(&item)
            .unwrap();
        assert!(attr.has_inner_crate_attr);
        assert_eq!(
            attr.crate_
                .map(|v| v.to_string().trim().to_owned())
                .as_deref(),
            Some("some_other_path")
        );
    }
}
