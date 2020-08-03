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

pub extern fn bf_input(ctx: *mut libc::c_void) -> u8 {
    // https://stackoverflow.com/a/42587849/621719
    // I have no idea what I'm doing.
    let closure: &mut &mut dyn FnMut() -> u8 = unsafe { mem::transmute(ctx) };
    closure()
}

pub fn execute<L: jit::Compiler>(contents: &str, lang: &mut L) {
    eprintln!("bf::run(): running!");

    let putchar = bf_print as *mut libc::c_void;
    lang.set_putchar(putchar);
    eprintln!("  putchar\t = *0x{:x}", putchar as usize);

    /*
    let stdin = io::stdin();
    let input_cb = move || {
        let mut buf = [0u8; 1];
        stdin.lock().read_exact(&mut buf).unwrap();
        buf[0]
    };
    let input_cb_ptr = &mut input_cb as *mut &mut dyn FnMut() -> u8;
    let getchar = bf_input as *mut libc::c_void;
    lang.set_getchar(getchar, input_cb_ptr as *mut libc::c_void);
    eprintln!("  getchar\t = *0x{:x}", getchar as usize);
    // */

    let mut exe = jit::Memory::new(jit::Pages(1));
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

