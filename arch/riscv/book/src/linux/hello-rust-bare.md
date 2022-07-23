# Linux "Hello world" in `no_std` Rust

[The Rust-glibc example](hello-rust-libc.md) used glibc, which provided Rust with OS APIs.
In this section, the goal is to write the same "Hello world" program without any libc.
It must rely only on RISC-V Linux ABI, reimplementing the required OS API as we need it, just like
[the GNU assembly "Hello world"](hello-gas.md).

This section is inspired by [Embeddonomicon](https://docs.rust-embedded.org/embedonomicon/smallest-no-std.html).
It takes the Linux process sandbox as a kind of an "embedded" environment, with complete control
over memory, without any external code, with only Linux ABI as its "hardware". A custom runtime
will be grown as we go.


## Tools and references

Ensure that [rustup](https://rustup.rs/) is installed. 

Make sure that a rustc target `riscv64gc-unknown-none-elf` is installed:

```console
$ rustup target add riscv64gc-unknown-none-elf
```

Unlike `riscv64gc-unknown-linux-gnu`, it assumes a "baremetal" environment and does not try to link
any libraries.

`cargo-binutils` is not strictly necessary, but it's nice to have `cargo objdump` and `cargo nm`:

```console
$ cargo install cargo-binutils
```

Documentation:

- [Embeddonomicon](https://docs.rust-embedded.org/embedonomicon) shows how to bring up a custom
  Rust runtime in a new environment.
- [Rust inline assembly](https://doc.rust-lang.org/nightly/reference/inline-assembly.html) is
  the definitive reference manual for the topic.


## The minimal `no_std` binary

```console
$ cargo init --name hello-nostd
```

Set the default target and runner in `.cargo/config.toml`:

```toml
[build]
target = "riscv64gc-unknown-none-elf"   # build for this target by default

[target.riscv64gc-unknown-none-elf]     # configuration for this target
runner = "qemu-riscv64"                 # for `cargo run` to work on x86_64
```

Set `src/main.rs` to be `#![no_main]` and `#![no_std]`.

A `no_std` environment still requires at least two basic runtime mechanisms, both related to
Rust panicking:

- what to do when unwinding the stack on panic. This is implemented by a
  [`#[lang = "eh_personality"]`](https://docs.rust-embedded.org/embedonomicon/smallest-no-std.html#eh_personality)
  function or just by waiving it off in `Cargo.toml`:

  ```toml
  [profile.dev]
  panic = "abort"

  [profile.release]
  panic = "abort"
  ```

  Although: `riscv64gc-unknown-none-elf` assumes `"panic-strategy": "abort"` by default.

- a `#[panic_handler]` function to execute when panic happened and the stack was unwound
  successfully:

  `src/main.rs`:
  ```
  #![no_main]
  #![no_std]

  #[panic_handler]
  fn panic_handler(_panic: &core::panic::PanicInfo) -> ! {
      loop {}           // for now, just hang to satisfy the typechecker.
  }
  ```

`#![no_main]` means that we should also remove `fn main()`. We'll get back to it later.

The binary that can be built at this stage does not actually contain any executable code.

```console
$ cargo run
'cargo run' terminated by signal SIGSEGV (Address boundary error)

$ cargo objdump --release -- -d | rustfilt
hello-nostd:    file format elf64-littleriscv
```


## A minimal executable that exits successfully

The output of `cargo rustc -- -Z unstable-options --print target-spec-json` suggests that
`riscv64-unknown-none-elf` uses [`rust.lld`](https://lld.llvm.org/) as its default linker.
I did not dig into details, but I guessed that its default linker script uses a `_start` symbol
as its entrypoint.

Reading the Rust inline assembly guide and translating the knowledge from 
[the assembly "Hello world"](hello-gas.md), we get this:

```rust
#![no_main]
#![no_std]
#![feature(start)]              // to enable #[start]

use core::arch::asm;            // to use asm!()

#[panic_handler]
fn panic_handler(_panic: &core::panic::PanicInfo) -> ! {
    loop {}
}

#[no_mangle]                    // for linker to be able to see `_start`
#[start]
pub unsafe extern "C"           // everything about this function is unsafe!
fn _start() -> ! {              // does not return
    asm!(
        "ecall",
        in("a7") 93,            // __NR_exit
        in("a0") 0,             // status code 0
        options(noreturn),
    )                           // `noreturn` assigns this block the return type `!`
}
```

## Writing to stdout

```rust
...
fn _start() -> ! {
    static HELLO: &[u8] = b"Hello world!\n";

    asm!(
        "ecall",
        in("a7") 64,                    // __NR_write
        in("a0") 1,                     // STDOUT_FILENO
        in("a1") HELLO.as_ptr().addr(), // #![feature(strict_provenance)]
        in("a2") HELLO.len(),
        options(readonly),              // expect no changes to memory
    );

    ...
}
```

## Tidying up: `linux-rt` and its linker script

TODO

<!--
In order to initialize memory and jump to some entrypoint, we'll need another (hopefully reusable)
Cargo crate, which we'll call `linux-rt` ("Linux runtime"):

```console
$ cd ..
$ cargo new --lib linux-rt
$ cd linux-rt
```

Add the `#![no_std]` attribute to `src/lib.rs`.

How do we specify the entrypoint and memory layout of the process? 
That's what [linker scripts](https://sourceware.org/binutils/docs/ld/Scripts.html) do. 
-->

## Troubleshooting

<details>
<summary>Getting the JSON spec of the current rustc target (requires nightly):

`cargo rustc -- -Z unstable-options --print target-spec-json` 

</summary>
<div>

```console
$ cargo rustc -- -Z unstable-options --print target-spec-json
   Compiling hello-nostd v0.1.0 (/home/user/code/learn/eval/rvemu/riscv/hello-nostd)
{
  "arch": "riscv64",
  "code-model": "medium",
  "cpu": "generic-rv64",
  "data-layout": "e-m:e-p:64:64-i64:64-i128:128-n64-S128",
  "eh-frame-header": false,
  "emit-debug-gdb-scripts": false,
  "features": "+m,+a,+f,+d,+c",
  "is-builtin": true,
  "linker": "rust-lld",
  "linker-flavor": "ld.lld",
  "llvm-abiname": "lp64d",
  "llvm-target": "riscv64",
  "max-atomic-width": 64,
  "panic-strategy": "abort",
  "relocation-model": "static",
  "target-pointer-width": "64"
}
```

</div>
</details>
