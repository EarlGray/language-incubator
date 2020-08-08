extern crate libc;

use std::mem;
use std::ops::{Index, IndexMut};

use libc::c_void;

/*
 *  Pages
 */


pub struct Pages(pub usize);

impl Pages {
    const SIZE: usize = 4096;

    pub fn from(byte_size: usize) -> Pages {
        Pages(byte_size / Pages::SIZE + 1)
    }

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
            let mut memptr: *mut libc::c_void = mem::MaybeUninit::uninit().assume_init();

            libc::posix_memalign(&mut memptr, Pages::SIZE, size);
            let flags = libc::PROT_READ | libc::PROT_WRITE | libc::PROT_EXEC;
            libc::mprotect(memptr, size, flags);

            // libc::memset(_contents, 0xc3, size);  // RET
            contents = mem::transmute(memptr);
        }

        Memory { contents, size, emit_pos: 0 }
    }

    pub fn emit(&mut self, code: &[u8]) {
        for b in code.iter() {
            let pos = self.emit_pos;
            self[pos] = *b;
            self.emit_pos += 1;
        }
    }

    pub fn emit_u32(&mut self, val: u32) {
        self.emit(&val.to_le_bytes());
    }

    pub fn emit_u64(&mut self, val: u64) {
        self.emit_u32(val as u32);
        self.emit_u32((val >> 32) as u32);
    }

    pub fn current_position(&self) -> usize {
        self.emit_pos
    }

    pub fn at(&mut self, index: usize) -> Memory {
        Memory { contents: self.contents, size: self.size, emit_pos: index }
    }

    pub fn get_entry(&self) -> fn() -> () {
        unsafe { mem::transmute(self.contents) }
    }
    pub fn get_entry1(&self) -> fn(usize) -> () {
        unsafe { mem::transmute(self.contents) }
    }
}


pub trait Compiler {
    type IR;

    fn parse(&self, source: &str) -> Self::IR;

    fn set_putchar(&mut self, putchar: *mut c_void) -> &mut Self;
    fn set_getchar(&mut self, getchar: *mut c_void, ctx: *mut c_void) -> &mut Self;

    fn compile(&self, program: &Self::IR, exe: &mut Memory);
}
