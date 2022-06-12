# slothjs_wasm

This is a wasm32-unknown-unknown build of slothjs, created and packaged via wasm-pack.

Build: 

```console
$ wasm-pack build
```

Test:

```console
$ wasm-pack test --headless --firefox
```

## Libraries

* [`wasm-bindgen`](https://github.com/rustwasm/wasm-bindgen) for communicating
  between WebAssembly and JavaScript.
* [`console_error_panic_hook`](https://github.com/rustwasm/console_error_panic_hook)
  for logging panic messages to the developer console.
* [`wee_alloc`](https://github.com/rustwasm/wee_alloc), an allocator optimized
  for small code size.
