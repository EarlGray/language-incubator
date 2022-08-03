use core::pin::Pin;

use crate::prelude::*;

use crate::JSResult;
use super::{Realm, Ref};

/// JavaScript string
///
/// It's not exactly a sequence of 16-bit code points, but close enough.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct JSString(String);

/// JavaScript number
#[derive(Debug, Clone, PartialEq)]
pub struct JSNumber(f64);

impl JSNumber {
    pub const NAN: JSNumber = JSNumber(f64::NAN);
}

/* TODO: string interner, its handles here.
/// JavaScript symbol
pub struct Symbol();
*/

/// JavaScript primitive (non-object) value.
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    /// The value of ES6 type Undefined, a (yet) absent value.
    Undefined,

    /// The value of ES6 type Null, the intentional absence of any object value.
    Null,

    /// Values of ES6 type Boolean
    Boolean(bool),

    /// Values of ES6 type Number
    Number(JSNumber),

    /// Values of ES6 type String
    String(JSString),

    /* TODO
    /// Values of ES6 type Symbol
    Symbol()
    */
}

/// A key in an object
pub type StrKey = JSString;

impl From<&str> for StrKey {
    fn from(s: &str) -> Self {
        JSString(s.to_string())
    }
}


/// What a Rust fn must look like to be callable from Javascript.
///
/// It receives a [`Ref`] bound to an array-like argument object.
/// It should write the return value back to the same [`Ref`].
pub type HostFn = fn(&Realm, Pin<&mut Ref>) -> JSResult<()>;

/// A wrapper for a native Rust function, [`HostFn`]
#[derive(Clone)]
pub struct HostFunc(pub HostFn);

impl fmt::Debug for HostFunc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "HostFunc(*{:#016p})", self.0 as *const ())
    }
}
