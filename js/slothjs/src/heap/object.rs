use crate::prelude::*;

use super::{
    HRef,
    Value,
};

/// An object attribute name
#[derive(Hash, Clone)]
pub(crate) struct Key(String);

/// An attribute of an object
struct Property {
    content: Content,
    enumerable: bool,
    configurable: bool,
}

/// Content of a Property
enum Content {
    Data{
        value: HRef,
        writable: bool,
    },
    Accessor{
        get: HRef,
        set: HRef,
    },
}

/// An instrinsic value of an Object
enum Inner {
    /// A primitive value for String/Number/Boolean
    Prim(Value),

    /*
    /// An array for an Array
    Array(JSArray),

    /// A native function
    Func(),

    /// A closure
    Closure(),
    */
}

impl Default for Inner {
    fn default() -> Self {
        Inner::Prim(Value::Undefined)
    }
}

pub(crate) struct Object {
    proto: HRef,
    value: Inner,
    properties: HashSet<Key, Property>,
}
