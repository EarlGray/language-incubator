use std::ffi;
use std::mem;
use std::ops::{Index, IndexMut};

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
    emit_pos: usize,
}

impl Index<usize> for Memory {
    type Output = u8;

    fn index(&self, index: usize) -> &u8 {
        unsafe { &*self.contents.offset(index as isize) }
    }
}

impl IndexMut<usize> for Memory {
    fn index_mut(&mut self, index: usize) -> &mut u8 {
        if index >= self.size {
            panic!("More than {} bytes to emit", self.size);
        }

        unsafe { &mut *self.contents.offset(index as isize) }
    }
}

impl Memory {
    pub fn new(pages: Pages) -> Memory {
        // SAFETY: just adding a new rwx memory region.
        let memptr: *mut ffi::c_void = unsafe {
            libc::mmap(
                std::ptr::null_mut(),
                pages.byte_size(),
                libc::PROT_READ | libc::PROT_WRITE | libc::PROT_EXEC,
                libc::MAP_PRIVATE | libc::MAP_ANON | libc::MAP_JIT,
                -1,
                0,
            )
        };

        Memory {
            contents: memptr as *mut u8,
            size: pages.byte_size(),
            emit_pos: 0,
        }
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
        if index >= self.size {
            panic!("index {} is out of bounds", index);
        }
        Memory {
            contents: self.contents,
            size: self.size,
            emit_pos: index,
        }
    }

    pub fn get_entry(&self) -> fn() -> () {
        unsafe {
            // SAFETY: if you don't execute the result, you'll be safe
            mem::transmute(self.contents)
        }
    }
    pub fn get_entry1(&self) -> fn(usize) -> () {
        unsafe {
            // SAFETY: if you don't execute the result, you'll be safe
            mem::transmute(self.contents)
        }
    }
}


