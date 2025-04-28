# tauri_commands

<!-- cargo-rdme start -->

Abstract over "commands" to be able to execute them in different contexts.

- Share definitions of commands between the Tauri backend and the
  WebAssembly frontend.
- Define "command" functions in a separate crate that doesn't depend on
  Tauri and then easily export them in the Tauri application. This allows
  re-using the command logic in another native program that doesn't use
  Tauri.

## Examples

### Define commands

```rust
use tauri_commands::tauri_commands;

#[cfg(target_family = "wasm")]
#[derive(Clone, Copy)]
pub struct WasmClient;

#[tauri_commands(wasm_client_impl_for = WasmClient)]
pub trait Greet {
    /// Get a personalized greeting.
    async fn greet(name: String) -> String;
}
```

### Export in Tauri program

Then you can write the following code for your main Tauri program:

```rust
use tauri_commands::tauri_commands;

pub struct TauriGreet;

#[tauri_commands]
impl Greet for TauriGreet {
    fn greet(name: String) -> String {
        format!("Hello, {}! You've been greeted from Rust!", name)
    }
}

tauri::Builder::default()
    .invoke_handler(with_commands_for_Greet!(tauri::generate_handler))
    .run(tauri::generate_context!())
    .expect("error while running tauri application");
```

### Invoke from WebAssembly

After that you can call the commands as if they were normal functions inside
WebAssembly:

```rust
assert_eq!(WasmClient::greet("WebAssembly").await, "Hello, WebAssembly! You've been greeted from Rust!");
```

#### Guard against panics if Tauri commands aren't available

Note that if the `__TAURI__` global variable isn't available to the
WebAssembly program then the command function will panic when it is called.
You can use the [`is_tauri_web_site`] function to check if the global
variable is present before calling any commands.

Sometime the [`has_host_access`] is more convenient, on WebAssembly it
behaves the same as `is_tauri_web_site` but it differs by returning `true`
on all other targets.

### Export bindings for JavaScript

If the `specta` feature is enabled then all generated commands will be
annotated with the `specta::specta` attribute. This requires that the specta
crate is a direct dependency of the crate where the attribute is emitted.

The [`tauri-specta`](https://crates.io/crates/tauri-specta) crate can then
be used to easily generate bindings for the Tauri commands.

The code below can be run to easily export the bindings:

```rust
let specta_plugin = {
    // export to JavaScript with JSDoc
    #[cfg(debug_assertions)] // <- Only export on non-release builds
    tauri_specta::js::builder()
        .commands(with_commands_for_Greet!(tauri_specta::collect_commands))
        .path("../src/bindings.js")
        .export()
        .unwrap();

    #[cfg(debug_assertions)] // <- Only export on non-release builds
    tauri_specta::ts::builder()
        .commands(with_commands_for_Greet!(tauri_specta::collect_commands))
        .path("../src/bindings.ts")
        .export()
        .unwrap();

    tauri_specta::ts::builder()
        .commands(with_commands_for_Greet!(tauri_specta::collect_commands))
        .into_plugin()
};
tauri::Builder::default()
    .plugin(specta_plugin)
    .invoke_handler(with_commands_for_Greet!(tauri::generate_handler))
    .run(tauri::generate_context!())
    .expect("error while running tauri application");
```

Be careful where the exported bindings are since a current limitation of
`tauri-specta` is that:

- Exporting your schema within a directory tracked by Tauri's hot reload
  will cause an infinite reload loop.

Note also that there is the [`TauriType`] attribute to allow generating
JavaScript type info for other Rust types. Use this on types that are used
as arguments or returned from Tauri commands. When the `specta` feature is
disabled this attribute will not do anything except allow some other
attributes to not cause compile errors.

<!-- cargo-rdme end -->

## License

This project is released under either:

- [MIT License](https://github.com/Lej77/tauri_commands/blob/master/LICENSE-MIT)
- [Apache License (Version 2.0)](https://github.com/Lej77/tauri_commands/blob/master/LICENSE-APACHE)

at your choosing.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally
submitted for inclusion in the work by you, as defined in the Apache-2.0
license, shall be dual licensed as above, without any additional terms or
conditions.
