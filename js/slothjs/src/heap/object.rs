use core::sync::atomic::{AtomicU32, Ordering};

use crate::prelude::*;

use crate::{
    Exception,
    JSResult,
};

use super::{
    HRef,
    HostFunc,
    HostFn,
    JSString,
    StrKey,
    Value,
};

/// An attribute of an object
#[derive(Debug, Clone)]
pub(super) enum Property {
    Data{
        configurable: bool,
        enumerable: bool,
        value: HRef,
        writable: bool,
    },
    Accessor{
        configurable: bool,
        enumerable: bool,
        get: HRef,
        set: HRef,
    },
}

impl Property {
    pub(super) fn from(value: HRef) -> Self {
        Property::Data{
            value,
            configurable: true,
            enumerable: true,
            writable: true,
        }
    }

    pub(super) fn configurable(&self) -> bool {
        match self {
            Property::Data{configurable, ..} => *configurable,
            Property::Accessor{configurable, ..} => *configurable,
        }
    }
}


/// An instrinsic value of an Object
#[derive(Debug, Clone)]
enum Inner {
    /// A primitive value for String/Number/Boolean
    Prim(Value),

    /// A native function
    Func(HostFunc),

    /*
    /// An array for an Array
    Array(JSArray),

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
    properties: HashMap<String, Property>,
    // TODO: symbol_properties: HashMap<JSSymbol, Property>
}

impl Object {
    // Create an `Object` with the given `proto`.
    pub(super) fn with_proto(proto: HRef) -> Self {
        Object{
            proto: Some(proto),
            value: Inner::default(),
            properties: HashMap::new(),
            n_ext: AtomicU32::default(),
            extensible: true,
        }
    }

    // Create a placeholder for a `value`.
    pub(super) fn with_value(value: Value) -> Self {
        Object{
            proto: None,
            value: Inner::Prim(value),
            properties: HashMap::new(),
            n_ext: AtomicU32::default(),
            extensible: false,
        }
    }

    pub(super) fn with_func(func: HostFn, proto: HRef) -> Self {
        let mut object = Self::with_proto(proto);
        object.value = Inner::Func(HostFunc(func));
        object
    }

    pub(super) fn to_primitive(&self) -> Option<Value> {
        match &self.value {
            Inner::Prim(value) => Some(value.clone()),
            _ => None,
        }
    }

    pub(super) fn set_proto(&mut self, proto: HRef) {
        self.proto = Some(proto);
    }

    pub(super) fn define(&mut self, name: &str, value: HRef) -> JSResult<()> {
        if !self.extensible {
            let message = format!("Object.defineProperty called on non-object");
            return Err(Exception::type_error(message));
        }
        self.properties.insert(name.to_string(), Property::from(value));
        Ok(())
    }

    pub(super) fn get(&self, name: &str) -> Option<HRef> {
        self.properties.get(name).map(|prop| match prop {
            Property::Data{ value, .. } => *value,
            Property::Accessor{ .. } => todo!(),
        })
    }

    pub(super) fn make_extref(&self) {
        // This AtomicU32 is only used in a single-threaded context,
        // so Ordering::Relaxed should be fine (?).
        self.n_ext.fetch_add(1, Ordering::Relaxed);
    }

    pub(super) fn drop_extref(&self) {
        self.n_ext.fetch_sub(1, Ordering::Relaxed);
    }
}
