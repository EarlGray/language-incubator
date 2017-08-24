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

use std::collections::HashMap;


const MEM_SIZE: usize = 30000;

#[derive(Clone, PartialEq, Debug)]
enum Op {
    Add(isize),
    Move(isize),
    Print,
    Read,
    Begin(usize),
    End(usize, Option<String>)
}

///
fn loop_image(ops: &Vec<Op>) -> String {
    let mut image = Vec::<char>::new();
    let mut i = ops.len() - 1;
    loop {
        match ops[i] {
            Op::Add(_) => image.push('+'),
            Op::Move(_) => image.push('>'),
            Op::Print => image.push('.'),
            Op::Read => image.push(','),
            Op::Begin(_) => {
                image.reverse();
                return image.iter().collect();
            },
            _ => panic!("should not happen in an internal loop")
        }
        i -= 1;
    }
}

fn parse(prog: &String) -> Vec<Op> {
    let mut ops = Vec::<Op>::new();

    let mut loops = Vec::<usize>::new();
    let mut internal_loop: bool = false;

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
                internal_loop = true;
                loops.push(cp);
                Op::Begin(0)
            }
            ']' => {
                let beginp = loops.pop().expect("malformed loop");
                ops[beginp] = Op::Begin(cp);

                let image = if internal_loop {
                    internal_loop = false;
                    Some(loop_image(&ops))
                } else { None };
                Op::End(beginp, image)
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

#[cfg(not(trace))]
fn print_trace(_: &[u64; 8], _: Option<HashMap<String, u64>>) {}
#[cfg(trace)]
fn print_trace(trace: &[u64; 8], loops: Option<HashMap<String, u64>>) {
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

    if let Some(loops) = loops {
        for (image, count) in loops.iter() {
            writeln!(stderr, "{}\t: {}", image, count).expect("");
        }
    }
}

macro_rules! trace_op {
    ( $stat:ident, $n:expr ) => { if cfg!(trace) { $stat[$n] += 1; } };
}

fn interpret<In: io::Read, Out: io::Write>(prog: &Vec<Op>, input: In, output: &mut Out) {
    let mut mem = [0u8; MEM_SIZE];
    let mut input = input.chars();
    let mut trace = [0u64; 8];
    let mut trace_loops = HashMap::<String, u64>::new();

    let mut cp = 0;  // code pointer
    let mut dp = 0;  // data pointer
    loop {
        //println!("Executing {:?}", prog[cp]);
        match prog[cp] {
            Op::Add(n) => {
                let c = Wrapping(mem[dp] as isize);
                mem[dp] = (c + Wrapping(n)).0 as u8;
                trace_op!(trace, 0);
            }
            Op::Move(n) => {
                dp = (dp as isize + n) as usize;
                trace_op!(trace, 2);
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
            Op::End(beginp, ref loop_image) => {
                let c = mem[dp];
                if c != 0 {
                    cp = beginp;
                    continue;
                }

                trace_op!(trace, 7);
                if cfg!(trace) {
                    if let Some(ref image) = *loop_image {
                        let mut loop_trace = trace_loops.entry(image.clone()).or_insert(0);
                        *loop_trace += 1;
                    }
                }
            }
        }
        cp += 1;

        if cp >= prog.len() {
            break;
        }
    }

    print_trace(&trace, Some(trace_loops));
}

fn run(source: &String) {
    let ops = parse(source);
    // writeln!(&mut io::stderr(), "parsed: {:?}", &ops).expect("");

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
