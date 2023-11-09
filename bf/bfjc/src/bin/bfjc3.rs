extern crate bfjc;

use std::env;
use std::fs;
use std::io;
use std::process;

use bfjc::bf3;

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <file.bf>", args[0]);
        process::exit(1);
    }
    let path = &args[1];
    let contents = fs::read_to_string(path)?;

    bfjc::execute::<bf3::Impl>(&contents);
    Ok(())
}
