use core::sync::atomic::{AtomicU32, Ordering};

use crate::prelude::*;

use super::{
    HRef,
    JSString,
    Value,
};

/// An attribute of an object
#[derive(Debug, Clone)]
struct Property {
    content: Content,
    enumerable: bool,
    configurable: bool,
}

/// Content of a Property
#[derive(Debug, Clone)]
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
#[derive(Debug, Clone)]
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

impl Inner {}

impl Default for Inner {
    fn default() -> Self {
        Inner::Prim(Value::Undefined)
    }
}

/// `Object` represents a JavaScript object/a primitive value on a [`Heap`].
///
/// All `Object`s should live on [`Heap`] or should be moved there as soon as possible.
///
/// In case of `proto=None`, `Object`s represent a placeholder for a primitive value that can be
/// referred to by an `HRef`. This also allows in-place upgrades to a String/Number/Boolean by
/// setting the corresponding `proto`.
///
/// - Non-extensible object: `extensible=false`
/// - Sealed object: non-extensible + all properties marked `configurable=false`
/// - Frozen object: sealed + all properties marked `writable=false`
#[derive(Debug)]
pub(super) struct Object {
    proto: Option<HRef>,    // None means this is an Inner::Prim(_) placeholder
    value: Inner,           // The intrinsic value or Prim::Undefined otherwise.
    n_ext: AtomicU32,       // counter for external references
    extensible: bool,       // new properties are allowed
    properties: HashMap<JSString, Property>,
    // TODO: symbol_properties: HashMap<JSSymbol, Property>
}

impl Object {
    // Create an `Object` with the given `proto`.
    fn with_proto(proto: HRef) -> Self {
        Object{
            proto: Some(proto),
            value: Inner::default(),
            properties: HashMap::new(),
            n_ext: AtomicU32::default(),
            extensible: true,
        }
    }

    // Create a placeholder for a `value`.
    fn with_value(value: Value) -> Self {
        Object{
            proto: None,
            value: Inner::Prim(value),
            properties: HashMap::new(),
            n_ext: AtomicU32::default(),
            extensible: false,
        }
    }

    pub(super) fn make_extref(&self) {
        self.n_ext.fetch_add(1, Ordering::Relaxed);
    }

    pub(super) fn drop_extref(&self) {
        // This AtomicU32 is only used in a single-threaded context,
        // so Ordering::Relaxed should be fine (?).
        self.n_ext.fetch_sub(1, Ordering::Relaxed);
    }
}
