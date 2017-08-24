/*
 *  Reexport
 */
pub mod bf3;
pub mod jit;
pub mod asm;

/*
 *  Implementation
 */
use std::env;
use std::io;
use std::io::prelude::*;
use std::process::exit;
use std::fs::File;


pub fn read_file(contents: &mut String) {
    let mut stderr = io::stderr();

    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        writeln!(&mut stderr, "Usage: {} <file.bf>", args[0]).unwrap();
        exit(1);
    }

    let progfile = &args[1];

    let mut f = File::open(progfile)
        .expect(&format!("failed to open {}", progfile));

    f.read_to_string(contents)
        .expect("i/o error");
}


/*
 *  Tests
 */

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
    }
}

