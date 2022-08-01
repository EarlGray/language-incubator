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

impl Drop for JSRef {
    fn drop(&mut self) {
        if let Some(heaprc) = Rc::upgrade(self.heap) {
            Rc::get(heaprc).objects[self.href].drop_extref();
        }
    }
}

/// Runtime heap: storage for objects.
#[derive(Debug)]
pub struct Heap{
    objects: Vec<Object>,
}

impl Heap { }

/// ES6 Realm: a storage + its global object.
pub struct Realm {
    heap: Rc<Heap>,
    global: HRef,
}
