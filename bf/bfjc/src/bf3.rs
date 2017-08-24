use std::ops::IndexMut;
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


fn compile<Buf: IndexMut<usize>>(_ops: &Vec<Op>, _buf: &mut Buf) {
    //
}


pub fn run(contents: &str) {
    println!("bf::run(): running!");

    let ops = parse(contents);

    let mut exe = jit::Memory::new(jit::Pages(1));
    compile(&ops, &mut exe);

    exe.emit(&[0xc3]);

    let entry = exe.get_entry();
    entry();

    println!("bf::run(): done!");
}
