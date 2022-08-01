mod value;
mod object;

use core::num::NonZeroU32;

use crate::prelude::*;

use self::object::{
    Object,
};

pub use self::value::{
    JSString,
    JSNumber,
    Value,
};

/// An index on the heap.
type HRef = NonZeroU32;

/// An external reference to an object on the [`Heap`].
#[derive(Debug)]
pub struct JSRef {
    heap: Weak<Heap>,
    href: HRef,         // external references make internal references "pinned"
}

impl Clone for JSRef {
    fn clone(&self) -> Self {
        if let Some(heaprc) = self.heap.upgrade() {
            heaprc.get(self.href).make_extref();
        }
        JSRef{
            heap: Weak::clone(&self.heap),
            href: self.href,
        }
    }
}

impl Drop for JSRef {
    fn drop(&mut self) {
        if let Some(heaprc) = self.heap.upgrade() {
            heaprc.get(self.href).drop_extref();
        }
    }
}

/// ES6 Realm: a storage + its global object.
pub struct Realm {
    heap: Rc<Heap>,
    global: HRef,
}

/// Runtime heap: storage for objects.
#[derive(Debug)]
pub struct Heap{
    objects: Vec<Object>,
}

impl Heap {
    fn get(&self, href: HRef) -> &Object {
        self.objects.get(href.get() as usize)
            .unwrap_or_else(|| panic!("href={:?} not found", href))
    }
}
