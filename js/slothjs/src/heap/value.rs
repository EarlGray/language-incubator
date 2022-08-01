use crate::prelude::*;

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
