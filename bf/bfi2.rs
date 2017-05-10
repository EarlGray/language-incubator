#![feature(io)]   // to allow `input.chars()`:

use std::env;
use std::fs::File;

use std::io;
use std::io::prelude::*;

#[allow(unused_imports)]
use std::io::Write;

use std::num::Wrapping;


const MEM_SIZE: usize = 30000;

#[derive(Clone, Copy, Debug, PartialEq)]
enum Op {
    Inc,
    Dec,
    Left,
    Right,
    Print,
    Read,
    Begin(usize),
    End(usize)
}

fn compile(prog: &String) -> Vec<Op> {
    let mut ops = Vec::<Op>::new();
    let mut loops = Vec::<usize>::new();

    for c in prog.chars() {
        let op = match c {
            '+' => Op::Inc,
            '-' => Op::Dec,
            '<' => Op::Left,
            '>' => Op::Right,
            '.' => Op::Print,
            ',' => Op::Read,
            '[' => {
                loops.push(ops.len());
                Op::Begin(0)
            }
            ']' => {
                let beginp = loops.pop().expect("malformed loop");
                ops[beginp] = Op::Begin(ops.len());
                Op::End(beginp)
            }
            _ => continue
        };
        ops.push(op);
    }
    ops
}

#[cfg(not(trace))]
fn print_trace(_: &[u64; 8]) {}
#[cfg(trace)]
fn print_trace(trace: &[u64; 8]) {
    let mut stderr = io::stderr();
    writeln!(&mut stderr, "\nTRACING INFO:").expect("");
    writeln!(&mut stderr, "| + | {}\t", trace[0]).expect("trace[0]: failed");
    writeln!(&mut stderr, "| - | {}\t", trace[1]).expect("trace[1]: failed");
    writeln!(&mut stderr, "| < | {}\t", trace[2]).expect("trace[2]: failed");
    writeln!(&mut stderr, "| > | {}\t", trace[3]).expect("trace[3]: failed");
    writeln!(&mut stderr, "| , | {}\t", trace[4]).expect("trace[4]: failed");
    writeln!(&mut stderr, "| . | {}\t", trace[5]).expect("trace[5]: failed");
    writeln!(&mut stderr, "| [ | {}\t", trace[6]).expect("trace[6]: failed");
    writeln!(&mut stderr, "| ] | {}\t", trace[7]).expect("trace[7]: failed");
}

macro_rules! trace_op {
    ( $stat:ident, $n:expr ) => { if cfg!(trace) { $stat[$n] += 1; } };
}

fn interpret<In: io::Read, Out: io::Write>(prog: &Vec<Op>, input: In, output: &mut Out) {
    let mut mem = [0u8; MEM_SIZE];
    let mut input = input.chars();
    let mut trace = [0u64; 8];

    let mut cp = 0;  // code pointer
    let mut dp = 0;  // data pointer
    loop {
        //println!("Executing {:?}", prog[cp]);
        match prog[cp] {
            Op::Inc => {
                let c = Wrapping(mem[dp] as usize);
                mem[dp] = (c + Wrapping(1)).0 as u8;
                trace_op!(trace, 0);
            }
            Op::Dec => {
                let c = Wrapping(mem[dp] as usize);
                mem[dp] = (c - Wrapping(1)).0 as u8;
                trace_op!(trace, 1);
            }
            Op::Left   => {
                dp -= 1;
                trace_op!(trace, 2);
            },
            Op::Right  => {
                dp += 1;
                trace_op!(trace, 3);
            },
            Op::Read   => {
                let c = input.next();
                let c = c.expect("end of stream");
                let c = c.unwrap();
                mem[dp] = c as u8;
                trace_op!(trace, 4);
            }
            Op::Print  => {
                let c = mem[dp];
                let buf = [c; 1];
                output.write(&buf).unwrap();
                trace_op!(trace, 5);
            }
            Op::Begin(endp) => {
                let c = mem[dp];
                if c == 0 {
                    cp = endp;
                    continue;
                }
                trace_op!(trace, 6);
            }
            Op::End(beginp) => {
                let c = mem[dp];
                if c != 0 {
                    cp = beginp;
                    continue;
                }
                trace_op!(trace, 7);
            }
        }
        cp += 1;

        if cp >= prog.len() {
            break;
        }
    }

    print_trace(&trace);
}

fn run(source: &String) {
    let ops = compile(source);
    // println!("compiled: {:?}", &ops);

    let stdin = io::stdin();
    let mut stdout = io::stdout();

    interpret(&ops, stdin, &mut stdout);
}

fn main() {
    let args: Vec<String> = env::args().collect();

    let progfile = &args[1];

    let mut f = File::open(progfile).expect("file not found");

    let mut contents = String::new();
    f.read_to_string(&mut contents).expect("i/o error");

    run(&contents);
}
