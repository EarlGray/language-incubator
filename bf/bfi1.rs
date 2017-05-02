#![feature(io)]   // to allow `input.chars()`:

use std::env;
use std::fs::File;

use std::io;
use std::io::prelude::*;

use std::num::Wrapping;



const ONE: Wrapping<u8> = Wrapping(1);

const MEM_SIZE: usize = 30000;
const SYNTAX_CHARS: &'static str = "+-<>.,[]";

#[derive(Debug, PartialEq)]
enum Op {
    Inc,
    Dec,
    Left,
    Right,
    Print,
    Read,
    Begin,
    End
}

fn compile(prog: &String) -> Vec<Op> {
    let syntax = SYNTAX_CHARS.to_string();
    prog.chars()
        .filter(|c: &char|
            syntax.contains(*c)
        ).map(|c: char| -> Op {
            if      c == '+' { Op::Inc   }
            else if c == '-' { Op::Dec   }
            else if c == '<' { Op::Left  }
            else if c == '>' { Op::Right }
            else if c == '.' { Op::Print }
            else if c == ',' { Op::Read  }
            else if c == '[' { Op::Begin }
            else if c == ']' { Op::End   }

            // "an unknown symbol" here implies "BF has syntax errors":
            else { panic!("Syntax error!") } 
        }).collect()
}

fn find_jump(prog: &Vec<Op>, cp: usize, whom: Op) -> usize {
    let (me, step) = match whom {
        Op::Begin => (Op::End,   -1isize),
        Op::End   => (Op::Begin, 1isize),
        _ => panic!("Not a loop companion: {:?}", whom)
    };

    let mut nest = 0isize;
    let mut i = cp;

    loop {
        i = (i as isize + step) as usize;
        let op = &prog[i];

        if *op == me {
            nest += 1
        } else if *op == whom {
            if nest > 0 {
                nest -= 1
            } else if nest == 0 {
                return i;
            } else {
                panic!("Unmatched loop: {}", i);
            }
        }
    }
}

fn interpret<In: io::Read, Out: io::Write>(prog: &Vec<Op>, input: In, output: &mut Out) {
    let mut mem = [0u8; MEM_SIZE];
    let mut input = input.chars();

    let mut cp = 0;  // code pointer
    let mut dp = 0;  // data pointer
    loop {
        //println!("Executing {:?}", prog[cp]);
        match prog[cp] {
            Op::Inc    => {
                let c = Wrapping(mem[dp]);
                mem[dp] = (c + ONE).0;
            }
            Op::Dec    => {
                let c = Wrapping(mem[dp]);
                mem[dp] = (c - ONE).0;
            }
            Op::Left   => dp -= 1,
            Op::Right  => dp += 1,
            Op::Read   => {
                let c = input.next();
                let c = c.expect("end of stream");
                let c = c.unwrap();
                mem[dp] = c as u8;
            }
            Op::Print  => {
                let c = mem[dp];
                let buf = [c; 1];
                output.write(&buf).unwrap();
            }
            Op::Begin  => {
                let c = mem[dp];
                if c == 0 {
                    cp = find_jump(prog, cp, Op::End);
                    continue;
                }
            }
            Op::End    => {
                let c = mem[dp];
                if c != 0 {
                    cp = find_jump(prog, cp, Op::Begin);
                    continue;
                }
            }
        }
        cp += 1;

        if cp >= prog.len() {
            break;
        }
    } 
}

fn run(source: &String) {
    let ops = compile(source);

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
