/*
 * This is a Rust implementation of a repeated-commands optimizing Brainfuck interpreter in
 * http://eli.thegreenplace.net/2017/adventures-in-jit-compilation-part-1-an-interpreter/
 *
 * Compilation:
 * $ rustc --cfg trace -O -o bfi3.opt bfi3.rs
 */
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
    Inc(usize),
    Dec(usize),
    Left(usize),
    Right(usize),
    Print,
    Read,
    Begin(usize),
    End(usize)
}

fn compile(prog: &String) -> Vec<Op> {
    let mut ops = Vec::<Op>::new();
    let mut loops = Vec::<usize>::new();

    for c in prog.chars() {
        let cp = ops.len();
        let op = match c {
            '+' => Op::Inc(1),
            '-' => Op::Dec(1),
            '<' => Op::Left(1),
            '>' => Op::Right(1),
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
            match (op, ops[cp - 1]) {
                (Op::Inc(_),    Op::Inc(n))   => ops[cp - 1] = Op::Inc(n + 1),
                (Op::Dec(_),    Op::Dec(n))   => ops[cp - 1] = Op::Dec(n + 1),
                (Op::Left(_),   Op::Left(n))  => ops[cp - 1] = Op::Left(n + 1),
                (Op::Right(_),  Op::Right(n)) => ops[cp - 1] = Op::Right(n + 1),
                _ => ops.push(op)
            }
        } else {
            ops.push(op);
        }
    }
    ops
}

#[cfg(not(trace))]
fn print_trace(_: &[u64; 8]) {}
#[cfg(trace)]
fn print_trace(trace: &[u64; 8]) {
    let mut total = 0u64;
    for n in trace.iter() {
        total += *n
    }

    let mut stderr = io::stderr();
    writeln!(&mut stderr, "\nTRACING INFO ({} total):", total).expect("");
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
            Op::Inc(n) => {
                let c = Wrapping(mem[dp] as usize);
                mem[dp] = (c + Wrapping(n)).0 as u8;
                trace_op!(trace, 0);
            }
            Op::Dec(n) => {
                let c = Wrapping(mem[dp] as usize);
                mem[dp] = (c - Wrapping(n)).0 as u8;
                trace_op!(trace, 1);
            }
            Op::Left(n) => {
                dp -= n;
                trace_op!(trace, 2);
            },
            Op::Right(n) => {
                dp += n;
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
    // writeln!(&mut io::stderr(), "compiled: {:?}", &ops).expect("");

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
