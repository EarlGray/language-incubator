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
    getchar: usize,
    putchar: usize
}

impl Impl {
    pub fn new() -> Impl {
        Impl { putchar: 0, getchar: 0 }
    }
}

impl jit::Compiler for Impl {
    type IR = Vec<Op>;

    fn set_putchar(&mut self, putchar: usize) -> &mut Self {
        self.putchar = putchar;
        self
    }

    fn set_getchar(&mut self, getchar: usize) -> &mut Self {
        self.getchar = getchar;
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
    fn compile(&self, _ops: &Vec<Op>, exe: &mut jit::Memory) {
        use asm::x64;

        /* TODO: save registers */

        /* TODO: set up %rbp and %rbx */

        /* TODO: compile operations */

        /* TODO: restore %rbp and %rbx */

        /* exit */
        exe.emit(x64::ret);
    }
}
