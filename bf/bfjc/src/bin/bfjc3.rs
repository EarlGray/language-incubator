use std::env;
use std::io;
use std::io::prelude::*;
use std::process::exit;
use std::fs::File;

extern crate bfjc;
use bfjc::bf3;

fn main() {
    let mut stderr = io::stderr();

    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        writeln!(&mut stderr, "Usage: {} <file.bf>", args[0]).unwrap();
        exit(1);
    }

    let progfile = &args[1];

    let contents = {
        let mut f = File::open(progfile)
            .expect(&format!("failed to open {}", progfile));

        let mut contents = String::new();
        f.read_to_string(&mut contents)
            .expect("i/o error");
        contents
    };

    bf3::run(&contents);
}
