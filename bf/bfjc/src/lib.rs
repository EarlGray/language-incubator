extern crate libc;
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
use std::mem;

use libc::c_void;


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


pub extern fn bf_print(c: u8) {
    print!("{}", c as char);
}

pub extern fn bf_input(ctx: *mut c_void) -> u8 {
    let stdin: &mut io::Stdin = unsafe { mem::transmute(ctx) };
    let mut buf = [0; 1];
    stdin.lock().read_exact(&mut buf).unwrap();
    buf[0]
}

pub fn execute<L: jit::Compiler>(contents: &str, lang: &mut L) {
    eprintln!("bf::run(): running!");

    let putchar = bf_print as *mut c_void;
    lang.set_putchar(putchar);
    eprintln!("  putchar\t = *0x{:x}", putchar as usize);

    let mut stdin = io::stdin();
    let stdinptr: *mut c_void = unsafe { mem::transmute(&mut stdin) };
    lang.set_getchar(bf_input as *mut c_void, stdinptr);
    eprintln!("  getchar\t = *0x{:x}", bf_input as usize);

    // assume up to 16 bytes per Op:
    let pages = jit::Pages::from(contents.len() * 16);
    let mut exe = jit::Memory::new(pages);
    eprintln!("  code mem\t = *0x{:x}", &exe[0] as *const _ as usize);

    /* jit compilation */
    let ops = lang.parse(contents);
    lang.compile(&ops, &mut exe);

    /* data memory */
    const MEM_SIZE: usize = 30000;
    let mut mem = [0; MEM_SIZE];
    let memptr = &mut mem as *mut _ as usize;

    eprintln!("  data mem\t = *0x{:x}", memptr);

    /* run */
    let entry = exe.get_entry1();
    entry(memptr);

    eprintln!("bf::run(): done!");
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

