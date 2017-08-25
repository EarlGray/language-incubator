extern crate libc;

use std::mem;
use std::ops::{Index, IndexMut};


/*
 *  Pages
 */


pub struct Pages(pub usize);

impl Pages {
    const SIZE: usize = 4096;

    pub fn byte_size(&self) -> usize {
        self.0 * Pages::SIZE
    }
}


/*
 *  JITting
 */
pub struct Memory {
    contents: *mut u8,
    size: usize,
    emit_pos: usize
}

impl Index<usize> for Memory {
    type Output = u8;

    fn index(&self, _index: usize) -> &u8 {
        unsafe { &*self.contents.offset(_index as isize) }
    }
}

impl IndexMut<usize> for Memory {
    fn index_mut(&mut self, _index: usize) -> &mut u8 {
        let size = self.size;
        if _index >= size {
            panic!("More than {} bytes to emit", size);
        }

        unsafe { &mut *self.contents.offset(_index as isize) }
    }
}

impl Memory {
    pub fn new(pages: Pages) -> Memory {
        let contents: *mut u8;
        let size = pages.byte_size();
        unsafe {
            let mut _contents: *mut libc::c_void = mem::uninitialized();

            libc::posix_memalign(&mut _contents, Pages::SIZE, size);
            let flags = libc::PROT_READ | libc::PROT_WRITE | libc::PROT_EXEC;
            libc::mprotect(_contents, size, flags);

            // libc::memset(_contents, 0xc3, size);  // RET
            contents = mem::transmute(_contents);
        }

        Memory { contents: contents, emit_pos: 0, size: size }
    }

    pub fn emit(&mut self, code: &[u8]) {
        for b in code.iter() {
            let pos = self.emit_pos;
            self[pos] = *b;
            self.emit_pos += 1;
        }
    }

    pub fn get_entry(&self) -> (fn() -> ()) {
        unsafe { mem::transmute(self.contents) }
    }
    pub fn get_entry1(&self) -> (fn(usize) -> ()) {
        unsafe { mem::transmute(self.contents) }
    }
}


pub trait Compiler {
    type IR;

    fn parse(&self, source: &str) -> Self::IR;

    fn set_putchar(&mut self, putchar: usize) -> &mut Self;
    fn set_getchar(&mut self, getchar: usize) -> &mut Self;

    fn compile(&self, program: &Self::IR, exe: &mut Memory);
}
