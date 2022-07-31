mod value;
mod object;

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
type HRef = usize;

/// An external reference to an object on the [`Heap`].
#[derive(Debug)]
pub struct JSRef {
    heap: Weak<Heap>,
    href: usize,
}

/// Runtime heap: storage for objects.
#[derive(Debug)]
pub struct Heap(Vec<Object>);

impl Heap { }

/// ES6 Realm: a storage + its global object.
pub struct Realm {
    heap: Rc<Heap>,
    global: HRef,
}
