use core::{
    borrow::Borrow,
    convert::Infallible,
    ops::Deref,
    str::Chars,
};

use crate::{
    prelude::*,
    CallContext,
    Exception,
    Heap,
    Interpreted,
    JSObject,
    JSRef,
    JSResult,
};

pub type JSON = serde_json::Value;

pub type JSNumber = f64;

/// A Javascript string value.
///
/// Why not `std::string::String`?
/// - Javascript strings are immutable. There is no point in cloning a string content.
/// - Ideally they should be a sequence of addressable 16-bit UTF-16 "code points" (potentially broken).
///   TODO: Indexing a Rust `String` over chars brings an unknown overhead of linear scanning.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct JSString(Rc<str>);

impl JSString {
    pub fn as_str(&self) -> &str {
        self.0.as_ref()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn chars(&self) -> Chars {
        self.0.chars()
    }
}

impl Default for JSString {
    fn default() -> Self {
        JSString(Rc::from(""))
    }
}

impl Borrow<str> for JSString {
    fn borrow(&self) -> &str {
        self.as_str()
    }
}

impl Deref for JSString {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.0.as_ref()
    }
}

impl fmt::Display for JSString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl FromStr for JSString {
    type Err = Infallible;

    fn from_str(s: &str) -> Result<JSString, Self::Err> {
        Ok(JSString(Rc::from(s)))
    }
}

impl From<String> for JSString {
    fn from(s: String) -> JSString {
        JSString(Rc::from(s.into_boxed_str()))
    }
}

impl From<&str> for JSString {
    fn from(s: &str) -> JSString {
        JSString(Rc::from(s))
    }
}

#[cfg(test)]
mod test_strings {
    use core::hash::{
        Hash,
        Hasher,
    };
    use std::collections::hash_map::DefaultHasher;

    use super::JSString;

    #[test]
    fn eq() {
        let s1 = JSString::from("hello");
        let s2 = JSString::from("hello");
        assert_eq!(s1, s2);
        assert_ne!(s1.as_ptr(), s2.as_ptr());

        let s3 = s2.clone();
        assert_eq!(s2.as_ptr(), s3.as_ptr());
    }

    fn get_hash<T: Hash>(t: &T) -> u64 {
        let mut h = DefaultHasher::new();
        t.hash(&mut h);
        h.finish()
    }

    #[test]
    fn hash() {
        let s1 = JSString::from("hello");
        let s2 = JSString::from("hello");
        assert_eq!(get_hash(&s1), get_hash(&s2));
    }
}

/// A `JSValue` is either a primitive value or a reference to an object.
#[derive(Debug, Clone, PartialEq)]
pub enum JSValue {
    Undefined,
    Bool(bool),
    Number(JSNumber),
    String(JSString),
    //Symbol(String)
    Ref(JSRef),
}

impl JSValue {
    pub const NULL: JSValue = JSValue::Ref(Heap::NULL);

    /// to_ref() tries to return the underlying object reference, if any.
    /// Throws Exception::ReferenceNotAnObject if it's not a reference.
    /// Checking if a JSValue is a reference: `val.to_ref().is_ok()`.
    pub fn to_ref(&self) -> JSResult<JSRef> {
        match self {
            JSValue::Ref(objref) => Ok(*objref),
            _ => {
                let what = Interpreted::Value(self.clone());
                Err(Exception::ReferenceNotAnObject(what))
            }
        }
    }

    #[allow(dead_code)]
    pub fn to_json(&self, heap: &Heap) -> JSResult<JSON> {
        match self {
            JSValue::Undefined => Ok(JSON::Null),
            JSValue::Bool(b) => Ok(JSON::from(*b)),
            JSValue::Number(n) => Ok(JSON::from(*n)),
            JSValue::String(s) => Ok(JSON::from(s.as_str())),
            JSValue::Ref(Heap::NULL) => Ok(JSON::Null),
            JSValue::Ref(href) => heap.get(*href).to_json(heap),
        }
    }

    /// `to_string()` makes a human-readable string representation of the value:
    /// ```
    /// # use serde_json::json;
    /// # use slothjs::{JSValue, Heap};
    /// # let mut heap = Heap::new();
    /// assert_eq!(
    ///     JSValue::from("1").to_string(&mut heap).unwrap().as_str(),
    ///     "\"1\""
    /// );
    /// assert_eq!(
    ///     JSValue::from(1).to_string(&mut heap).unwrap().as_str(),
    ///     "1"
    /// );
    ///
    /// let json_object = json!({"one": 1});
    /// let example_object = heap.object_from_json(&json_object);
    /// assert_eq!(
    ///     example_object.to_string(&mut heap).unwrap().as_str(),
    ///     "{ one: 1 }"
    /// );
    ///
    /// let json_array = json!([1, 2]);
    /// let example_array = heap.object_from_json(&json_array);
    /// assert_eq!(
    ///     example_array.to_string(&mut heap).unwrap().as_str(),
    ///     "[1, 2]"
    /// );
    /// ```
    pub fn to_string(&self, heap: &mut Heap) -> JSResult<JSString> {
        match self {
            JSValue::String(s) => {
                let jstr = JSON::from(s.as_str());
                Ok(JSString::from(jstr.to_string()))
            }
            JSValue::Ref(heapref) => {
                // without `.clone()` `heap` cannot be borrowed in both places
                heap.get(*heapref).clone().to_string(heap)
            }
            _ => self.stringify(heap),
        }
    }

    /// stringify() makes everything into a string
    /// used for evaluation in a string context.
    /// It corresponds to .toString() in JavaScript
    pub fn stringify(&self, heap: &mut Heap) -> JSResult<JSString> {
        match self {
            JSValue::Undefined => Ok("undefined".into()),
            JSValue::Bool(b) => Ok(b.to_string().into()),
            JSValue::Number(n) => Ok(n.to_string().into()),
            JSValue::String(s) => Ok(s.clone()),
            JSValue::Ref(r) if r == &Heap::NULL => Ok(JSString::from("null")),
            JSValue::Ref(r) => match heap.lookup_protochain(*r, "toString") {
                Some(to_string) => {
                    let funcref = to_string.to_ref(heap)?;
                    let result = heap.execute(
                        funcref,
                        CallContext {
                            this_ref: *r,
                            method_name: "toString".into(),
                            arguments: vec![],
                            loc: None,
                        },
                    )?;
                    Ok(result.to_value(heap)?.stringify(heap)?)
                }
                None => Ok("[object Object]".into()),
            },
        }
    }

    /// numberify() tries to make everything into a numeric value
    /// for evalation in a numeric context.
    /// It is slightly more strict than `+value` in JavaScript: only
    /// `value.numberify().unwrap_or(f64::NAN)` corresponds to `+value` in JavaScript.
    pub fn numberify(&self, heap: &Heap) -> Option<JSNumber> {
        match self {
            JSValue::Undefined => None, // Some(f64::NAN),
            JSValue::Bool(b) => Some(if *b { 1.0 } else { 0.0 }),
            JSValue::Number(n) => Some(*n),
            JSValue::String(s) => s.as_str().parse::<JSNumber>().ok(),
            JSValue::Ref(Heap::NULL) => Some(0.0),
            JSValue::Ref(r) => {
                let object = heap.get(*r);
                if let Some(array) = object.as_array() {
                    match &array.storage[..] {
                        [] => Some(0.0),              // +[]  == 0
                        [val] => val.numberify(heap), // +[x] == x
                        _ => None,                    // +[x, y, ..] == NaN
                    }
                } else {
                    object.to_primitive().and_then(|v| v.numberify(heap))
                }
            }
        }
    }

    /// boolify() treats everythings as a truthy value.
    /// ES5: ToBoolean
    pub fn boolify(&self, heap: &Heap) -> bool {
        match self {
            JSValue::Undefined => false,
            JSValue::String(s) => !s.as_str().is_empty(),
            JSValue::Ref(Heap::NULL) => false,
            JSValue::Ref(_) => true,
            _ => {
                if let Some(n) = self.numberify(heap) {
                    !(n == 0.0 || f64::is_nan(n))
                } else {
                    true
                }
            }
        }
    }

    /// objectify() wraps a primitive into its object:
    /// - `undefined` becomes `null`
    /// - `bool`/`number`/`string` becomes `Boolean`/`Number`/`String`
    /// - objects just return their reference.
    pub fn objectify(&self, heap: &mut Heap) -> JSRef {
        match self {
            JSValue::Undefined => Heap::NULL,
            JSValue::Bool(b) => heap.alloc(JSObject::from_bool(*b)),
            JSValue::Number(_n) => {
                let _ = crate::source::print_callstack(heap);
                todo!(); // TODO: Number object
            }
            JSValue::String(s) => heap.alloc(JSObject::from(s.clone())),
            JSValue::Ref(r) => *r,
        }
    }

    /// Javascript's `typeof`
    pub fn type_of(&self, heap: &Heap) -> &'static str {
        match self {
            JSValue::Undefined => "undefined",
            JSValue::String(_) => "string",
            JSValue::Number(_) => "number",
            JSValue::Bool(_) => "boolean",
            JSValue::Ref(r) => match heap.get(*r).is_callable() {
                true => "function",
                false => "object",
            },
        }
    }

    /// Abstract Equality Comparison, `==`:
    /// <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Equality_comparisons_and_sameness#Loose_equality_using_>
    pub fn loose_eq(&self, other: &JSValue, heap: &Heap) -> bool {
        let lval = if self == &JSValue::NULL {
            &JSValue::Undefined
        } else {
            self
        };
        let rval = if other == &JSValue::NULL {
            &JSValue::Undefined
        } else {
            other
        };
        match (lval, rval) {
            (JSValue::Undefined, JSValue::Undefined) => true,
            (JSValue::Undefined, _) | (_, JSValue::Undefined) => false,
            (JSValue::Number(_), JSValue::Number(_))
            | (JSValue::String(_), JSValue::String(_))
            | (JSValue::Bool(_), JSValue::Bool(_)) => self == other,
            (JSValue::Ref(lref), JSValue::Ref(rref)) if lref == rref => true,
            (JSValue::Ref(lref), JSValue::Ref(rref)) => match (
                heap.get(*lref).to_primitive(),
                heap.get(*rref).to_primitive(),
            ) {
                (Some(lval), Some(rval)) => lval == rval,
                _ => false,
            },
            _ => match (self.numberify(heap), other.numberify(heap)) {
                (Some(lnum), Some(rnum)) => lnum == rnum,
                _ => false,
            },
        }
    }

    /// Strict Equality, `===`
    /// <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Equality_comparisons_and_sameness#strict_equality_using>
    pub fn strict_eq(&self, other: &JSValue, _heap: &Heap) -> bool {
        self == other
    }

    pub fn numerically<F>(&self, other: &JSValue, heap: &Heap, op: F) -> JSValue
    where
        F: Fn(f64, f64) -> f64,
    {
        let val = match (self.numberify(heap), other.numberify(heap)) {
            (Some(lnum), Some(rnum)) => op(lnum, rnum),
            _ => f64::NAN,
        };
        JSValue::Number(val)
    }

    /// Addition operator:
    /// <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Addition>
    pub fn plus(&self, other: &JSValue, heap: &mut Heap) -> JSResult<JSValue> {
        if let (JSValue::String(lstr), JSValue::String(rstr)) = (self, other) {
            return Ok(JSValue::from(lstr.to_string() + rstr.as_str()));
        }
        if let (Some(lnum), Some(rnum)) = (self.numberify(heap), other.numberify(heap)) {
            return Ok(JSValue::from(lnum + rnum));
        }
        let lvalstr = self.stringify(heap)?;
        let rvalstr = other.stringify(heap)?;
        Ok(JSValue::from(lvalstr.to_string() + rvalstr.as_str()))
    }

    /// Subtraction operator:
    /// <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Subtraction>
    pub fn minus(&self, other: &JSValue, heap: &Heap) -> JSResult<JSValue> {
        Ok(JSValue::numerically(self, other, heap, |a, b| a - b))
    }

    pub fn compare<StrCmpFn: Fn(&str, &str) -> bool, NumCmpFn: Fn(f64, f64) -> bool>(
        &self,
        other: &JSValue,
        heap: &Heap,
        stringly: StrCmpFn,
        numberly: NumCmpFn,
    ) -> JSValue {
        // TODO: Abstract Relational Comparison
        if let (JSValue::String(lstr), JSValue::String(rstr)) = (self, other) {
            return JSValue::from(stringly(lstr.as_str(), rstr.as_str()));
        };
        let lnum = self.numberify(heap).unwrap_or(f64::NAN);
        let rnum = other.numberify(heap).unwrap_or(f64::NAN);
        JSValue::from(numberly(lnum, rnum))
    }
}

impl From<bool> for JSValue {
    fn from(b: bool) -> Self {
        JSValue::Bool(b)
    }
}

impl From<JSNumber> for JSValue {
    fn from(number: JSNumber) -> Self {
        JSValue::Number(number)
    }
}

impl From<i64> for JSValue {
    fn from(number: i64) -> Self {
        JSValue::Number(number as JSNumber)
    }
}

impl<S> From<S> for JSValue
where
    JSString: From<S>,
{
    fn from(s: S) -> Self {
        JSValue::String(JSString::from(s))
    }
}

impl From<JSRef> for JSValue {
    fn from(r: JSRef) -> Self {
        JSValue::Ref(r)
    }
}

impl TryFrom<&JSON> for JSValue {
    type Error = ();

    /// Constructs a pure value (without references), if possible.
    /// Excludes objects and arrays.
    fn try_from(json: &JSON) -> Result<JSValue, Self::Error> {
        let value = if json.is_null() {
            JSValue::NULL
        } else if let Some(b) = json.as_bool() {
            JSValue::from(b)
        } else if let Some(n) = json.as_f64() {
            JSValue::from(n)
        } else if let Some(s) = json.as_str() {
            JSValue::from(s)
        } else {
            return Err(());
        };
        Ok(value)
    }
}

#[test]
fn test_numberify() {
    let dummy = Heap::new();
    assert_eq!(JSValue::from("5").numberify(&dummy), Some(5.0));
}

#[test]
fn test_boolify() {
    let dummy = Heap::new();

    // true
    assert!(JSValue::from(true).boolify(&dummy));
    assert!(JSValue::from(1).boolify(&dummy));
    assert!(JSValue::from("0").boolify(&dummy));
    //assert!( JSValue::from(json!([])).boolify(&dummy) );

    // false
    assert!(!JSValue::from(false).boolify(&dummy));
    assert!(!JSValue::from(0).boolify(&dummy));
    assert!(!JSValue::from(f64::NAN).boolify(&dummy));
    assert!(!JSValue::from("").boolify(&dummy));
    assert!(!JSValue::NULL.boolify(&dummy));
}
