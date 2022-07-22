# Linux "Hello world" in RISC-V GNU assembly

Let's write the smallest possible RISC-V Linux program that:

- outputs "Hello world" to the standard output
- exits successfully


## Cross-compilation and RISCV-emulation packages on Ubuntu

I'm using a x86\_64 machine with Ubuntu 22.04 and a RISC-V GCC toolchain from its repositories, 
[gcc-riscv64-unknown-elf](https://packages.ubuntu.com/jammy/gcc-riscv64-unknown-elf).

In order to get Linux-specific APIs for RISC-V, we'll also use
package [linux-libc-dev-riscv64-cross](https://packages.ubuntu.com/jammy/linux-libc-dev-riscv64-cross)
that provides RISC-V specific C headers in `/usr/riscv64-linux-gnu/include`.

Package [qemu-riscv64](https://packages.ubuntu.com/jammy/qemu-user) allows to run RV64 binaries 
in a software-emulated RV64 Linux environment.

Documentation and references:

- [The GNU Assembler Manual](https://sourceware.org/binutils/docs/as.html)
- [The GNU linker Manual](https://sourceware.org/binutils/docs/ld/)
- [RISC-V Assembly Programmer's Manual](https://github.com/riscv-non-isa/riscv-asm-manual)


## The assembly code

First of all, the program must exit successfully. On Linux, this is done via 
the `exit` system call: <https://linux.die.net/man/2/exit>

How do we write RISC-V assembly to actually call it?

According to [man 2 syscalls](https://man7.org/linux/man-pages/man2/syscalls.2.html),
the actual syscall numbers for the host instruction set architecture can be found
in `/usr/include/asm/unistd.h` as `__NR_xxx` constants (e.g. `__NR_exit` for the `exit` syscall).

This RISC-V cross-compilation toolchain defines the actual number in 
`/usr/riscv64-linux-gnu/include/asm-generic/unistd.h` as `__NR_exit`.

According to [man 2 syscall](https://man7.org/linux/man-pages/man2/syscall.2.html),
the RISC-V way of making Linux system calls looks like this:

  + put the syscall number into `a7`: `li  a7, __NR_exit`
  + put the syscall arguments into `a0`, `a1`, ..., `a5`
  + `ecall` performs the system call
  + returned values can be found in `a0`, `a1`

Therefore, `_exit(0)` in C translates to:

```asm
    li  a7, __NR_exit
    li  a0, 0
    ecall
```

[man 2 write](https://man7.org/linux/man-pages/man2/write.2.html) describes the syscall arguments:

```asm
#define STDOUT_FILENO   1

.text
    # write(STDOUT_FILENO, greeting, greetlen):
    li  a7, __NR_write
    li  a0, STDOUT_FILENO       # `int fd`
    la  a1, greeting            # `const void *buf`
    li  a2, greetlen            # `size_t count`
    ecall
```

Symbols `greeting` and `greetlen` are defined in section `.rodata`:

```asm
.section .rodata

greeting: .asciz "Hello world\n"
.equ greetlen, . - greeting
```

The complete assembly code in `hello.S` (capital `.S` means "assembly source, preprocessed"):

```asm
#include <asm-generic/unistd.h>

#define STDOUT_FILENO   1

.section .rodata                    # a section for read-only data

greeting: .asciz "Hello world\n"    # const char *greeting = "Hello world\n";
.equ greetlen, . - greeting         # const size_t greetlen = sizeof greeting;

.text                               # a section for executable code
.globl _start                       # export the program entrypoint symbol for the linker
_start:                             # linkers use `_start` as the default entrypoint
    li  a7, __NR_write
    li  a0, STDOUT_FILENO
    la  a1, greeting
    li  a2, greetlen
    ecall                           # write(STDOUT_FILENO, greeting, greetlen)

    li  a7, __NR_exit
    li  a0, 0
    ecall                           # _exit(0)
1:  j 1b                            # hang, in the very unlikely case `exit` failed
```


## Compilation

By default, `riscv64-unknown-elf-gcc` tries to link start code from some `crt0.o` to enable 
libc functionality. Our program does not need libc, so let's add `-nostdlib` to `LDFLAGS`.

To be able to include `asm-generic/unistd.h` from `/usr/riscv64-linux-gnu/include`, adjust
`ASFLAGS` to include files from `-I /usr/riscv64-linux-gnu/include`.

The `Makefile`:

```make
CC = riscv64-unknown-elf-gcc
ASFLAGS += -I /usr/riscv64-linux-gnu/include
LDFLAGS += -nostdlib

# GNU make has an implicit rule for %: %.S which is roughly
#   $(CC) $(ASFLAGS) $(LDFLAGS) $< -o $@
hello: hello.S

# find and clean all the executables here
clean:
    -find -executable -type f -delete

# `clean` is not a file!
.PHONY: clean

```

Calling `make` produces a 1208-byte ELF executable that runs via `qemu-riscv64` and outputs
"Hello world" (or just run it directly if `qemu-user-binfmt` is installed):

```sh
$ make hello
riscv64-unknown-elf-gcc -I /usr/riscv64-linux-gnu/include -nostdlib hello.S -o hello

$ stat hello
  File: hello
  Size: 1208            Blocks: 8          IO Block: 4096   regular file
  ...

$ qemu-riscv64 hello
Hello world

```
