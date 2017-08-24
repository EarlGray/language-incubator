use jit;

/// The language
#[derive(Clone, PartialEq, Debug)]
enum Op {
    Add(isize),
    Move(isize),
    Print,
    Read,
    Begin(usize),
    End(usize)
}

fn parse(prog: &str) -> Vec<Op> {
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
fn compile(_ops: &Vec<Op>, exe: &mut jit::Memory) {
    target_error!("This target_arch/target_family is not supported");
}

#[cfg(all(target_family = "unix", target_arch = "x86"))]
fn compile(_ops: &Vec<Op>, exe: &mut jit::Memory) {
    target_error!("TODO: target_arch = x86");
    use asm::x86;
}

#[cfg(all(target_family = "unix", target_arch = "x86_64"))]
fn compile(_ops: &Vec<Op>, exe: &mut jit::Memory) {
    use asm::x64;

    /* TODO: save registers */

    /* TODO: set up %rbp and %rbx */

    /* TODO: compile operations */

    /* TODO: restore %rbp and %rbx */

    /* exit */
    exe.emit(x64::ret);
}



fn bf_print(c: char) {
    print!("{}", c);
}

pub fn run(contents: &str) {
    println!("bf::run(): running!");

    /* data memory */
    const MEM_SIZE: usize = 30000;
    let mut mem = [0; MEM_SIZE];
    let memptr = &mut mem as *mut _ as usize;

    println!("  mem\t = *0x{:x}!", memptr);

    /* jit compilation */
    let ops = parse(contents);

    let putchar = &bf_print as *const _ as usize;
    println!("  print\t = *0x{:x}", putchar);

    let mut exe = jit::Memory::new(jit::Pages(1));
    /* TODO: pass putchar/getchar */
    compile(&ops, &mut exe);

    /* run */
    let entry = exe.get_entry1();
    entry(memptr);

    println!("bf::run(): done!");
}
