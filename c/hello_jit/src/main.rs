/*
 *   http://www.jonathanturner.org/2015/12/building-a-simple-jit-in-rust.html
 */

extern crate libc;

use std::mem;
use std::ops::{Index, IndexMut};


const PAGE_SIZE: usize = 4096;

extern {
    fn memset(
        s: *mut libc::c_void,
        c: libc::uint32_t,
        n: libc::size_t
    ) -> *mut libc::c_void;
}


struct JitMemory {
    contents: *mut u8
}

impl JitMemory {
    fn new(n_pages: usize) -> JitMemory {
        let contents: *mut u8 = unsafe {
            let size = n_pages * PAGE_SIZE;
            let mut pages: *mut libc::c_void = mem::uninitialized();
            libc::posix_memalign(&mut pages, PAGE_SIZE, size);

            let rwx = libc::PROT_EXEC | libc::PROT_READ | libc::PROT_WRITE;
            libc::mprotect(pages, size, rwx);

            memset(pages, 0xC3, size);

            mem::transmute(pages)
        };

        JitMemory { contents: contents }
    }
}

impl Index<usize> for JitMemory {
    type Output = u8;

    fn index(&self, _index: usize) -> &u8 {
        unsafe { &mut *self.contents.offset(_index as isize) }
    }
}

impl IndexMut<usize> for JitMemory {
    fn index_mut(&mut self, _index: usize) -> &mut u8 {
        unsafe { &mut *self.contents.offset(_index as isize) }
    }
}

fn main() {
    let mut jit: JitMemory = JitMemory::new(1);

    jit[0] = 0x48;
    jit[1] = 0xC7;
    jit[2] = 0xC0;
    jit[3] = 0x03;
    jit[4] = 0;
    jit[5] = 0;
    jit[6] = 0;

    let fun: (fn() -> i64) = unsafe { mem::transmute(jit.contents) };

    println!("{}", fun());
}
