extern crate libc;

use std::ptr;
use libc::c_void;

use jit;

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

impl jit::Compiler for Impl {
    type IR = Vec<Op>;

    fn set_putchar(&mut self, putchar: *mut c_void) -> &mut Self {
        self.putchar = putchar;
        self
    }

    fn set_getchar(&mut self, getchar: *mut c_void, ctx: *mut c_void) -> &mut Self {
        self.getchar = getchar;
        self.getchar_ctx = ctx;
        self
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
        ))
    ))]
    pub fn compile(_ops: &Vec<Op>, exe: &mut jit::Memory) {
        target_error!("This target_arch/target_family is not supported");
    }

    #[cfg(all(target_family = "unix", target_arch = "x86"))]
    pub fn compile(_ops: &Vec<Op>, exe: &mut jit::Memory) {
        target_error!("TODO: target_arch = x86");
        use asm::x86;
    }

    #[cfg(all(target_family = "unix", target_arch = "x86_64"))]
    fn compile(&self, ops: &Vec<Op>, exe: &mut jit::Memory) {
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
                    // addb $n, (%rbp, %rbx)
                    exe.emit(&[0x80, 0x44, 0x1d, 0x00, *n as u8]);
                },
                Op::Move(n) => {
                    // add $n, %rbx
                    exe.emit(&[0x48, 0x81, 0xc3]);
                    exe.emit_u32(*n as u32);
                    // TODO: check if %rbx is in bounds
                },
                Op::Print => {
                    // movb (%rbp, %rbx), %al
                    exe.emit(&[0x8a, 0x44, 0x1d, 0x00]);
                    // movzx %al, %rdi
                    exe.emit(&[0x48, 0x0f, 0xb6, 0xf8]);
                    // movabs $putchar, %rax
                    exe.emit(&[0x48, 0xb8]);
                    exe.emit_u64(self.putchar as u64);
                    // call *%rax
                    exe.emit(&[0xff, 0xd0]);
                },
                Op::Begin(_end) => {
                    // movb (%rbp, %rbx), %al
                    exe.emit(&[0x8a, 0x44, 0x1d, 0x00]);
                    // test %al, %al
                    exe.emit(&[0x84, 0xc0]);
                    // jz <loop_end>
                    exe.emit(&[0x0f, 0x84]);
                    exe.emit_u32(0);    // to be filled later.
                    // remember the address just behind the intruction
                    let addr = exe.current_position();
                    loop_addrs.push(addr);
                },
                Op::End(_begin) => {
                    // movb (%rbp, %rbx), %al
                    exe.emit(&[0x8a, 0x44, 0x1d, 0x00]);
                    // test %al, %al
                    exe.emit(&[0x84, 0xc0]);
                    // jnz <loop_begin>
                    let begin = loop_addrs.pop().expect("end without beginning");
                    let end = exe.current_position() + 6;
                    let offset = (end - begin) as u32;
                    exe.emit(&[0x0f, 0x85]);
                    exe.emit_u32((0 - offset as i32) as u32);
                    // backfill the begin jump:
                    exe.at(begin - 4).emit_u32(offset);
                },
                _ => panic!("TODO: bf3::compile({:?})", op)
            }
        }

        /* restore %rbp and %rbx */
        exe.emit(&[0x48, 0x83, 0xc4, 0x08]); // add $8, %rsp
        exe.emit(&[0x5b]); // pop %rbx
        exe.emit(&[0x5d]); // pop %rbp

        /* exit */
        exe.emit(x64::ret);
    }
}
