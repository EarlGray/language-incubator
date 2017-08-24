This is a basic JIT created following [this tutorial](http://www.jonathanturner.org/2015/12/building-a-simple-jit-in-rust.html).

To build without Cargo:

1. Clone [libc crate](git@github.com:rust-lang/libc.git) into a directory `$LIBC_DIR`;
2. `cargo build` there;
3. Compile here:

    $ rustc --extern libc=$LIBC_DIR/target/debug/liblibc.rlib -o hello_jit src/main.rs
