extern crate libc;

/*
 *  Reexport
 */
pub mod bf3;
pub mod jit;
pub mod asm;

pub use jit::Memory;

/*
 *  Implementation
 */
use std::io;
use std::io::prelude::*;
use std::mem;
use std::pin::pin;

use libc::c_void;


pub trait Compiler {
    type IR;

    fn make(putc: *mut c_void, getc: *mut c_void, getc_ctx: *mut c_void) -> Self;

    fn parse(&self, source: &str) -> Self::IR;

    fn compile(&self, program: &Self::IR, exe: &mut Memory);
}


pub extern "C" fn bf_print(c: u8) {
    print!("{}", c as char);
}

pub extern "C" fn bf_input(ctx: *mut c_void) -> u8 {
    let stdin: &mut io::Stdin = unsafe { mem::transmute(ctx) };
    let mut buf = [0; 1];
    stdin.lock().read_exact(&mut buf).unwrap();
    buf[0]
}

pub fn execute<L: Compiler>(contents: &str) {
    eprintln!("bf::run(): running!");

    let putchar = bf_print as *mut c_void;
    eprintln!("  putchar\t = *0x{:x}", putchar as usize);

    let mut stdin = io::stdin();
    pin!(&mut stdin);   // please don't yank stdin from beneath our feet

    let stdinptr = &mut stdin as *mut io::Stdin as *mut c_void;
    let getchar = bf_input as *mut c_void;
    eprintln!("  getchar\t = *0x{:x}", bf_input as usize);

    let lang = L::make(putchar, getchar, stdinptr);

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

