extern crate libc;

use std::ptr;
use libc::c_void;

use crate::{Compiler, Memory};

/// The language
#[derive(Clone, PartialEq, Debug)]
pub enum Op {
    Add(isize),
    Move(isize),
    Print,
    Read,
    Begin(usize),
    End(usize)
}


pub struct Impl {
    putchar: *mut c_void,
    getchar: *mut c_void,
    getchar_ctx: *mut c_void,
}

impl Impl {
    pub fn new() -> Impl {
        Impl {
            putchar: ptr::null_mut(),
            getchar: ptr::null_mut(),
            getchar_ctx: ptr::null_mut(),
        }
    }
}

impl Compiler for Impl {
    type IR = Vec<Op>;

    fn make(putchar: *mut c_void, getchar: *mut c_void, getchar_ctx: *mut c_void) -> Self {
        Self { putchar, getchar, getchar_ctx }
    }

    fn parse(&self, prog: &str) -> Self::IR {
        let mut ops = Vec::<Op>::new();

        let mut loops = Vec::<usize>::new();

        for c in prog.chars() {
            let cp = ops.len();
            let op = match c {
                '+' => Op::Add(1),
                '-' => Op::Add(-1),
                '<' => Op::Move(-1),
                '>' => Op::Move(1),
                '.' => Op::Print,
                ',' => Op::Read,
                '[' => {
                    loops.push(cp);
                    Op::Begin(0)
                }
                ']' => {
                    let beginp = loops.pop().expect("malformed loop");
                    ops[beginp] = Op::Begin(cp);

                    Op::End(beginp)
                }
                _ => continue
            };
            if cp > 0 {
                // there may be a repeated command:
                match (&op, &ops[cp - 1]) {
                    (&Op::Add(step),    &Op::Add(n))  => ops[cp - 1] = Op::Add(n + step),
                    (&Op::Move(step),   &Op::Move(n)) => ops[cp - 1] = Op::Move(n + step),
                    _ => ops.push(op)
                }
            } else {
                ops.push(op);
            }
        }
        ops
    }


    #[cfg(
      any(
        not(target_family = "unix"),
        not(any(
          target_arch = "x86",
          target_arch = "x86_64",
          target_arch = "aarch64",
        ))
    ))]
    fn compile(&self, _ops: &Vec<Op>, exe: &mut Memory) {
        compile_error!("This target_arch/target_family is not supported");
    }

    #[cfg(all(target_family = "unix", target_arch = "x86"))]
    fn compile(&self, _ops: &Vec<Op>, exe: &mut Memory) {
        compile_error!("TODO: target_arch = x86");
    }

    #[cfg(all(target_family = "unix", target_arch = "x86_64"))]
    fn compile(&self, ops: &Vec<Op>, exe: &mut Memory) {
        use asm::x64;

        /* save registers */
        exe.emit(&[0x55]); // push %rbp
        exe.emit(&[0x53]); // push %rbx
        exe.emit(&[0x48, 0x83, 0xec, 0x08]); // sub $8, %rsp

        /* set up %rbp and %rbx */
        exe.emit(&[0x48, 0x89, 0xfd]); // movq %rdi, %rbp
        exe.emit(&[0x48, 0x31, 0xdb]); // xorq %rbx, %rbx

        /* compile operations */
        let mut loop_addrs = Vec::<usize>::new();

        for op in ops.iter() {
            match op {
                Op::Add(n) => {
                    exe.emit(&[0x80, 0x44, 0x1d, 0x00, *n as u8]);  // addb $n, (%rbp, %rbx)
                }
                Op::Move(n) => {
                    // TODO: check if %rbx is in bounds
                    exe.emit(&[0x48, 0x81, 0xc3]);                  // add $n, %rbx
                    exe.emit_u32(*n as u32);
                }
                Op::Print => {
                    exe.emit(&[0x8a, 0x44, 0x1d, 0x00]);            // movb (%rbp, %rbx), %al
                    exe.emit(&[0x48, 0x0f, 0xb6, 0xf8]);            // movzx %al, %rdi

                    exe.emit(&[0x48, 0xb8]);                        // movabs $putchar, %rax
                    exe.emit_u64(self.putchar as u64);

                    exe.emit(&[0xff, 0xd0]);                        // call *%rax
                }
                Op::Read => {

                    exe.emit(&[0x48, 0xbf]);                        // movabs $ctx, %rdi
                    exe.emit_u64(self.getchar_ctx as u64);

                    exe.emit(&[0x48, 0xb8]);                        // movabs $getchar, %rax
                    exe.emit_u64(self.getchar as u64);

                    exe.emit(&[0xff, 0xd0]);                        // call *%rax

                    exe.emit(&[0x88, 0x44, 0x1d, 0x00]);            // movb %al, (%rbp, %rbx)
                }
                Op::Begin(_end) => {
                    exe.emit(&[0x8a, 0x44, 0x1d, 0x00]);            // movb (%rbp, %rbx), %al
                    exe.emit(&[0x84, 0xc0]);                        // test %al, %al

                    exe.emit(&[0x0f, 0x84]);                        // jz <loop_end>
                    exe.emit_u32(0);    // to be filled later.

                    // remember the address just behind the intruction:
                    let addr = exe.current_position();
                    loop_addrs.push(addr);
                }
                Op::End(_begin) => {
                    exe.emit(&[0x8a, 0x44, 0x1d, 0x00]);            // movb (%rbp, %rbx), %al

                    exe.emit(&[0x84, 0xc0]);                        // test %al, %al
                    // jnz <loop_begin>
                    let begin = loop_addrs.pop().expect("end without beginning");
                    let end = exe.current_position() + 6;
                    let offset = (end - begin) as u32;
                    exe.emit(&[0x0f, 0x85]);
                    exe.emit_u32((0 - offset as i32) as u32);

                    // backfill the begin jump:
                    exe.at(begin - 4).emit_u32(offset);
                }
            }
        }

        /* restore %rbp and %rbx */
        exe.emit(&[0x48, 0x83, 0xc4, 0x08]); // add $8, %rsp
        exe.emit(&[0x5b]); // pop %rbx
        exe.emit(&[0x5d]); // pop %rbp

        /* exit */
        exe.emit(x64::ret);
    }

    // TODO: change `ops` to `&[Op]`
    // TODO: signal that compilation failed, return Result
    #[cfg(all(target_family = "unix", target_arch = "aarch64"))]
    fn compile(&self, ops: &Vec<Op>, exe: &mut Memory) {
        // the data pointer register - x20
        // the function prolog
        exe.emit(&[0xff, 0x43, 0x00, 0xd1]);      // sub sp, sp, #0x10

        // the memory offset is passed in x0, let's save it in x19
        exe.emit(&[0xf3, 0x03, 0x00, 0xaa]); // mov x19, x0

        // initialize x20 to 0
        exe.emit(&[0x94, 0x02, 0x14, 0xca]); // eor x20, x20, x20

        for op in ops.iter() {
            match op {
                Op::Move(by) => {
                    if !(-0x1000 < *by && *by < 0x1000) {
                        panic!("shifts must fit in 12 bits, but by={}", by);
                    }
                    let by = *by as i32;
                    if by < 0 {
                        // sub x20, x20, #-by
                        let n = -(by & 0xfff) as u32;
                        exe.emit_u32(0xd1000294 | (n << 10));
                    } else {
                        // add x20, x20, #by
                        let n = by as u32;
                        exe.emit_u32(0x91000294 | (n << 10));
                    }
                }
                _ => {
                    panic!("TODO: {:?}", op);
                }
            }
        }

        // epilogue
        exe.emit(&[0xff, 0x43, 0x00, 0x91]);      // add sp, sp, #0x10
        exe.emit(&[0xc0, 0x03, 0x5f, 0xd6]);      // ret
    }
}
