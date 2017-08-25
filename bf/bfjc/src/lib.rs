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


fn bf_print(c: char) {
    print!("{}", c);
}

pub fn execute<L: jit::Compiler>(contents: &str, lang: &mut L) {
    println!("bf::run(): running!");

    let putchar = &bf_print as *const _ as usize;
    lang.set_putchar(putchar);
    println!("  print\t = *0x{:x}", putchar);

    let mut exe = jit::Memory::new(jit::Pages(1));

    /* jit compilation */
    let ops = lang.parse(contents);
    lang.compile(&ops, &mut exe);

    /* data memory */
    const MEM_SIZE: usize = 30000;
    let mut mem = [0; MEM_SIZE];
    let memptr = &mut mem as *mut _ as usize;

    println!("  mem\t = *0x{:x}!", memptr);

    /* run */
    let entry = exe.get_entry1();
    entry(memptr);

    println!("bf::run(): done!");
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

