//! Abstract over "commands" to be able to execute them in different contexts.
//!
//! - Share definitions of commands between the Tauri backend and the
//!   WebAssembly frontend.
//! - Define "command" functions in a separate crate that doesn't depend on
//!   Tauri and then easily export them in the Tauri application. This allows
//!   re-using the command logic in another native program that doesn't use
//!   Tauri.
//!
//! # Examples
//!
//! ## Define commands
//!
//! ```no_run
//! use tauri_commands::tauri_commands;
//!
//! #[cfg(target_family = "wasm")]
//! #[derive(Clone, Copy)]
//! pub struct WasmClient;
//!
//! #[tauri_commands(wasm_client_impl_for = WasmClient)]
//! pub trait Greet {
//!     /// Get a personalized greeting.
//!     async fn greet(name: String) -> String;
//! }
//! ```
//!
//! ## Export in Tauri program
//!
//! Then you can write the following code for your main Tauri program:
//!
//! ```ignore
//! # pub trait Greet {
//! #     fn greet(name: String) -> String;
//! # }
//! use tauri_commands::tauri_commands;
//!
//! pub struct TauriGreet;
//!
//! #[tauri_commands]
//! impl Greet for TauriGreet {
//!     fn greet(name: String) -> String {
//!         format!("Hello, {}! You've been greeted from Rust!", name)
//!     }
//! }
//!
//! tauri::Builder::default()
//!     .invoke_handler(with_commands_for_Greet!(tauri::generate_handler))
//!     .run(tauri::generate_context!())
//!     .expect("error while running tauri application");
//! ```
//!
//! ## Invoke from WebAssembly
//!
//! After that you can call the commands as if they were normal functions inside
//! WebAssembly:
//!
//! ```ignore
//! assert_eq!(WasmClient::greet("WebAssembly").await, "Hello, WebAssembly! You've been greeted from Rust!");
//! ```
//!
//! ### Guard against panics if Tauri commands aren't available
//!
//! Note that if the `__TAURI__` global variable isn't available to the
//! WebAssembly program then the command function will panic when it is called.
//! You can use the [`is_tauri_web_site`] function to check if the global
//! variable is present before calling any commands.
//!
//! Sometime the [`has_host_access`] is more convenient, on WebAssembly it
//! behaves the same as `is_tauri_web_site` but it differs by returning `true`
//! on all other targets.
//!
//! ## Export bindings for JavaScript
//!
//! If the `specta` feature is enabled then all generated commands will be
//! annotated with the `specta::specta` attribute. This requires that the specta
//! crate is a direct dependency of the crate where the attribute is emitted.
//!
//! The [`tauri-specta`](https://crates.io/crates/tauri-specta) crate can then
//! be used to easily generate bindings for the Tauri commands.
//!
//! The code below can be run to easily export the bindings:
//!
//! ```ignore
//! let specta_plugin = {
//!     // export to JavaScript with JSDoc
//!     #[cfg(debug_assertions)] // <- Only export on non-release builds
//!     tauri_specta::js::builder()
//!         .commands(with_commands_for_Greet!(tauri_specta::collect_commands))
//!         .path("../src/bindings.js")
//!         .export()
//!         .unwrap();
//!
//!     #[cfg(debug_assertions)] // <- Only export on non-release builds
//!     tauri_specta::ts::builder()
//!         .commands(with_commands_for_Greet!(tauri_specta::collect_commands))
//!         .path("../src/bindings.ts")
//!         .export()
//!         .unwrap();
//!
//!     tauri_specta::ts::builder()
//!         .commands(with_commands_for_Greet!(tauri_specta::collect_commands))
//!         .into_plugin()
//! };
//! tauri::Builder::default()
//!     .plugin(specta_plugin)
//!     .invoke_handler(with_commands_for_Greet!(tauri::generate_handler))
//!     .run(tauri::generate_context!())
//!     .expect("error while running tauri application");
//! ```
//!
//! Be careful where the exported bindings are since a current limitation of
//! `tauri-specta` is that:
//!
//! - Exporting your schema within a directory tracked by Tauri's hot reload
//!   will cause an infinite reload loop.
//!
//! Note also that there is the [`TauriType`] attribute to allow generating
//! JavaScript type info for other Rust types. Use this on types that are used
//! as arguments or returned from Tauri commands. When the `specta` feature is
//! disabled this attribute will not do anything except allow some other
//! attributes to not cause compile errors.

/// A simple macro that makes it easier to work with different `cfg` flags.
///
/// The macro's name comes from the fact that unlike the [`cfg`] macro it
/// doesn't just generate a boolean that is then checked at runtime, instead it
/// doesn't even compile the parts of the code that is for other targets.
///
/// ```
/// use tauri_commands::const_cfg;
///
/// // Use parenthesis when invoking the macro so that rustfmt still works:
/// const_cfg!(if cfg!(feature = "tauri") {
///     // Tauri specific code.
/// } else if cfg!(target_family = "wasm") {
///     // Wasm specific code.
/// } else {
///     // Fallback code.
/// });
/// ```
#[macro_export]
macro_rules! const_cfg {
    // If else:
    (@parse {if cfg!($($cfg_if:tt)*) { $($then:tt)* } else $($else:tt)*} {  $({ $($so_far:tt)* })*  }) => {
        $( #[cfg(not(  $($so_far)*  ))] )*
        #[cfg($($cfg_if)*)]
        {
            $($then)*
        }
        $crate::const_cfg! {@parse {$($else)*} { $({ $($so_far)* })* { $($cfg_if)* } } }
    };
    // Final if clause:
    (@parse {if cfg!($($cfg_if:tt)*) { $($then:tt)* }} {  $({ $($so_far:tt)* })*  }) => {
        $( #[cfg(not(  $($so_far)*  ))] )*
        #[cfg($($cfg_if)*)]
        {
            $($then)*
        }
        // No else, so enforce that this is a statement:
        ;
    };
    // Final else clause:
    (@parse {{ $($else:tt)* }} { $({ $($so_far:tt)* })* }) => {
        $( #[cfg(not(  $($so_far)*  ))] )*
        {
            $($else)*
        }
    };
    (@parse {} { $($so_far:tt)* }) => { ::core::compile_error!("Macro input ended unexpectedly") };
    (@parse {$($fail:tt)*} { $($so_far:tt)* }) => {
        ::core::compile_error!(::core::concat!("Macro parsing failed, this text remained: ", ::core::stringify!($($fail)*)))
    };
    // Initial tokens:
    ($($tokens:tt)*) => {{
        $crate::const_cfg!{@parse {$($tokens)*} {}}
    }};
}

pub use tauri_commands_macros::tauri_commands;

/// Utility macro for combining multiple `with_commands_for_TraitName` macros
/// into a single macro.
///
/// Write the new macro name first then a comma and then the same syntax as the
/// normal `with_commands_for_` macros (except it doesn't accept `with` followed
/// by custom command names).
///
/// Currently this needs to be invoked with at least two `with_commands_for_`
/// macros, otherwise it won't expand correctly.
#[macro_export]
macro_rules! combine_commands {
    // Allow callback paths with leading colons:
    (
        @inner
        {$($attr:tt)*}
        {$new_name:ident}
        {$dollar:tt}
        {
            {$($callback:tt)*}
            $(then $( {$($callback_rest:tt)*} )then* )?
            // $(with $($forward:tt)* )?
        }
    ) => {
        // Hint to language server that these should be paths:
        #[allow(unused_imports)]
        const _:() = {
            use $($callback)* as _;
            $($(use $($callback_rest)* as _;)*)?
        };
        $($attr)*
        macro_rules! $new_name {
            (
                $dollar ({$dollar ($dollar callback_rest_inner:tt)*} )then*
                $dollar (with $dollar ($dollar forward_inner:tt)* )?
            ) => {
                $($callback)*! {
                    // Combined callbacks:
                    $($( {$($callback_rest)*} )then* )?
                    // Callbacks given to new macro:
                    $dollar (then  {  $dollar (  $dollar callback_rest_inner  )*  } )*
                    // Commands given to new macro:
                    $dollar (with  $dollar ($dollar forward_inner)*)?
                }
            }
        }
    };
    // Simpler syntax when leading colons aren't needed:
    (
        @inner
        {$($attr:tt)*}
        {$new_name:ident}
        {$dollar:tt}
        {
            $($callback:ident)::+
            $(then $($callback_rest:ident)::* )*
            // $(with $($forward:tt)* )?
        }
    ) => {
        // Hint to language server that these should be paths:
        #[allow(unused_imports)]
        const _:() = {
            use $($callback)::* as _;
            $(use $($callback_rest)::* as _;)*
        };
        $($attr)*
        macro_rules! $new_name {
            // Complex syntax:
            (
                $dollar ({$dollar ($dollar callback_rest_inner:tt)*} )then*
                $dollar (with $dollar ($dollar forward_inner:tt)* )?
            ) => {
                $($callback)::*! {
                    // Combined callbacks:
                    $( $($callback_rest)::* )then*
                    // Callbacks given to new macro:
                    $dollar (then  {  $dollar (  $dollar callback_rest_inner  )*  } )*
                    // Commands given to new macro:
                    $dollar (with  $dollar ($dollar forward_inner)*)?
                }
            };
            // Simple syntax:
            (
                $dollar ($dollar ($dollar callback_rest_inner:ident)::* )then*
                $dollar (with $dollar ($dollar forward_inner:tt)* )?
            ) => {
                $($callback)::*! {
                    // Combined callbacks:
                    $( $($callback_rest)::* )then*
                    // Callbacks given to new macro:
                    $dollar (then  $dollar (  $dollar callback_rest_inner  )::*  )*
                    // Commands given to new macro:
                    $dollar (with  $dollar ($dollar forward_inner)*)?
                }
            };
        }
    };
    ($(#[$attr:meta])* $new_name:ident, $($rest:tt)*) => {
        // Call the macro itself so we have access to a "$" token when we expand:
        $crate::combine_commands! {
            @inner
            {$(#[$attr])*}
            {$new_name}
            {$}
            {$($rest)*}
        }
    };
}

/// PRIVATE API!!!
///
/// DO NOT USE!
#[doc(hidden)]
pub mod __private {
    #![allow(unused_imports)]

    // Items needed to invoke tauri commands on wasm target:
    pub use crate::tauri_import::*;
    // Items needed to declare tauri commands on host target:
    pub use crate::tauri_export::*;

    // Used by serde macros:
    pub use tauri_commands_macros::AllowSerde;

    // Used by TauriType macro:
    pub use tauri_commands_macros::NoopSpectaType;
}

#[cfg(all(target_family = "wasm", feature = "tauri-import"))]
mod tauri_import {
    //! Interact with Tauri.
    //!
    //! # References
    //!
    //! [Introduction - The `wasm-bindgen`
    //! Guide](https://rustwasm.github.io/wasm-bindgen/introduction.html) [Calling
    //! Rust from the frontend | Tauri
    //! Apps](https://tauri.app/v1/guides/features/command/)

    use serde::{de::DeserializeOwned, Serialize};
    use std::{fmt, marker::PhantomData, ops::Deref};
    use wasm_bindgen::prelude::*;

    // Needed by macros:
    pub use serde;

    #[wasm_bindgen]
    extern "C" {
        #[wasm_bindgen(js_name = window)]
        type Window;

        /// The type of the global `__Tauri__` variable.
        type Tauri;

        type Tauri1InvokeFn;

        /// Get the global `__Tauri__` variable if it is available.
        ///
        /// # References
        ///
        /// - https://docs.rs/js-sys/0.3.61/src/js_sys/lib.rs.html#5727-5802
        /// - https://docs.rs/web-sys/0.3.61/src/web_sys/features/gen_Window.rs.html#4
        /// - https://rustwasm.github.io/wasm-bindgen/reference/attributes/on-js-imports/getter-and-setter.html
        /// - https://rustwasm.github.io/docs/wasm-bindgen/reference/types/imported-js-types.html
        #[wasm_bindgen(getter, catch, static_method_of = Window, js_class = "window", js_name  = "__TAURI__")]
        fn get_tauri() -> Result<Option<Tauri>, JsValue>;

        #[wasm_bindgen(getter, catch, js_namespace = ["window", "__TAURI__", "tauri"], js_name  = "invoke")]
        fn check_tauri1() -> Result<Option<Tauri1InvokeFn>, JsValue>;

        #[wasm_bindgen(catch, js_namespace = ["window", "__TAURI__", "tauri"], js_name  = "invoke")]
        async fn tauri1_invoke(cmd: &str, args: JsValue) -> Result<JsValue, JsValue>;

        #[wasm_bindgen(catch, js_namespace = ["window", "__TAURI__", "core"], js_name  = "invoke")]
        async fn tauri2_invoke(cmd: &str, args: JsValue) -> Result<JsValue, JsValue>;
    }

    async fn invoke(cmd: &str, args: JsValue) -> Result<JsValue, JsValue> {
        use core::sync::atomic::{AtomicI8, Ordering::Relaxed};
        static IS_TAURI_V1: AtomicI8 = AtomicI8::new(-1);
        let mut is_tauri_v1 = IS_TAURI_V1.load(Relaxed);

        if is_tauri_v1 == -1 {
            is_tauri_v1 = check_tauri1().map_or(false, |value| value.is_some()) as i8;
            IS_TAURI_V1.store(is_tauri_v1, Relaxed);
        }

        match is_tauri_v1 {
            0 => tauri2_invoke(cmd, args).await,
            1 => tauri1_invoke(cmd, args).await,
            _ => unreachable!("we only set this variable to 0 or 1")
        }
    }

    /// Check if the web context that the WebAssembly code is running inside has
    /// access to Tauri commands.
    pub fn is_tauri_web_site() -> bool {
        use core::sync::atomic::{AtomicI8, Ordering::Relaxed};
        static IS_TAURI: AtomicI8 = AtomicI8::new(-1);
        let is_tauri = IS_TAURI.load(Relaxed);
        match is_tauri {
            0 => false,
            1 => true,
            _ => {
                let is_tauri = matches!(Window::get_tauri(), Ok(Some(_)));
                IS_TAURI.store(is_tauri as i8, Relaxed);
                is_tauri
            }
        }
    }

    #[derive(Debug)]
    #[expect(dead_code, reason = "we don't currently expose the underlying error")]
    enum InnerError {
        DeserializeArgs(serde_wasm_bindgen::Error),
        DeserializeReturn(serde_wasm_bindgen::Error),
        DeserializeError(serde_wasm_bindgen::Error),
        UnspecifiedError(JsValue),
    }
    #[derive(Debug)]
    pub struct Error {
        inner: InnerError,
    }
    impl fmt::Display for Error {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match &self.inner {
                InnerError::DeserializeArgs(_) => {
                    writeln!(f, "failed to serialize Tauri command's arguments")?;
                }
                InnerError::DeserializeReturn(_) => {
                    writeln!(f, "failed to deserialize Tauri command's return value")?;
                }
                InnerError::DeserializeError(_) => {
                    writeln!(f, "failed to deserialize Tauri command's error")?;
                }
                InnerError::UnspecifiedError(_) => {
                    writeln!(f, "Tauri command returned an unexpected error")?;
                }
            }
            Ok(())
        }
    }
    impl From<InnerError> for Error {
        fn from(inner: InnerError) -> Self {
            Error { inner }
        }
    }

    /// Invoke a Tauri command that is expected to never fail.
    pub async fn invoke_command<A, R>(cmd: &str, args: &A) -> Result<R, Error>
    where
        A: Serialize,
        R: DeserializeOwned,
    {
        let args = serde_wasm_bindgen::to_value(args).map_err(InnerError::DeserializeArgs)?;
        match { invoke(cmd, args) }.await {
            Ok(value) => serde_wasm_bindgen::from_value(value)
                .map_err(InnerError::DeserializeReturn)
                .map_err(Into::into),
            Err(err) => Err(InnerError::UnspecifiedError(err).into()),
        }
    }

    /// Invoke a Tauri command that could fail with a specified error type.
    pub async fn try_invoke_command<A, R, E>(cmd: &str, args: &A) -> Result<Result<R, E>, Error>
    where
        A: Serialize,
        R: DeserializeOwned,
        E: DeserializeOwned,
    {
        match invoke_command(cmd, args).await {
            // Deserialize command's error:
            Err(Error {
                inner: InnerError::UnspecifiedError(err),
            }) => Ok(Err(
                serde_wasm_bindgen::from_value(err).map_err(InnerError::DeserializeError)?
            )),
            // Normal return and serialization errors are unmodified:
            Ok(v) => Ok(Ok(v)),
            Err(e) => Err(e),
        }
    }

    /// Uses ["Autoref-based stable
    /// specialization"](https://github.com/dtolnay/case-studies/blob/master/autoref-specialization/README.md).
    pub struct TauriCommandSpecialization<T>(TauriCommandFallback<T>);
    impl<T> TauriCommandSpecialization<T> {
        pub const fn new() -> Self {
            Self(TauriCommandFallback(PhantomData))
        }
    }
    impl<R, E> TauriCommandSpecialization<Result<R, E>> {
        pub async fn __invoke_tauri_command<A>(
            &self,
            cmd: &str,
            args: &A,
        ) -> Result<Result<R, E>, Error>
        where
            A: Serialize,
            R: DeserializeOwned,
            E: DeserializeOwned,
        {
            try_invoke_command(cmd, args).await
        }
    }
    impl<T> Deref for TauriCommandSpecialization<T> {
        type Target = TauriCommandFallback<T>;
        fn deref(&self) -> &Self::Target {
            &self.0
        }
    }
    pub struct TauriCommandFallback<R>(PhantomData<R>);
    impl<R> TauriCommandFallback<R> {
        pub async fn __invoke_tauri_command<A>(&self, cmd: &str, args: &A) -> Result<R, Error>
        where
            A: Serialize,
            R: DeserializeOwned,
        {
            invoke_command(cmd, args).await
        }
    }
}

#[cfg(not(all(target_family = "wasm", feature = "tauri-import")))]
mod tauri_import {
    /// Check if the web context that the WebAssembly code is running inside has
    /// access to Tauri commands.
    pub fn is_tauri_web_site() -> bool {
        false
    }
}

#[cfg(all(not(target_family = "wasm"), feature = "tauri-export"))]
mod tauri_export {
    // Needed by serde derive macros:
    pub use serde;
}
#[cfg(not(all(not(target_family = "wasm"), feature = "tauri-export")))]
mod tauri_export {}

#[doc(inline)]
pub use tauri_import::is_tauri_web_site;

/// Check if the program has access to host functions. This can be `true` either
/// by not running as WebAssembly or by invoking Tauri commands.
pub fn has_host_access() -> bool {
    cfg!(not(target_family = "wasm")) || is_tauri_web_site()
}

#[cfg(any(
    all(feature = "tauri-export", not(target_family = "wasm")),
    all(feature = "tauri-import", target_family = "wasm")
))]
mod tauri_serde {
    pub use serde::{Deserialize as TauriDeserialize, Serialize as TauriSerialize};
}
#[cfg(not(any(
    all(feature = "tauri-export", not(target_family = "wasm")),
    all(feature = "tauri-import", target_family = "wasm")
)))]
mod tauri_serde {
    /// Use as trait bound to require `serde::Serialize` when targeting Tauri.
    pub trait TauriSerialize {}
    impl<T: ?Sized> TauriSerialize for T {}

    /// Use as trait bound to require `serde::Deserialize` when targeting Tauri.
    pub trait TauriDeserialize {}
    impl<T: ?Sized> TauriDeserialize for T {}
}
#[doc(inline)]
pub use tauri_serde::*;

/// Implements `serde::Deserialize` via `serde`'s derive macro only when
/// targeting Tauri.
///
/// Note that this isn't a derive macro so it should be used as
/// `#[TauriDeserialize]`, not as `#[derive(TauriDeserialize)]`.
pub use tauri_commands_macros::TauriDeserialize;

/// Implements `serde::Serialize` via `serde`'s derive macro only when targeting
/// Tauri.
///
/// Note that this isn't a derive macro so it should be used as
/// `#[TauriSerialize]`, not as `#[derive(TauriSerialize)]`.
pub use tauri_commands_macros::TauriSerialize;

/// Provides JavaScript type information for a type.
///
/// Really just a convenient way to use the `::specta::Type` derive macro.
///
/// Note that this isn't a derive macro so it should be used as `#[TauriType]`,
/// not as `#[derive(TauriType)]`.
#[doc(inline)]
pub use tauri_commands_macros::TauriType;
