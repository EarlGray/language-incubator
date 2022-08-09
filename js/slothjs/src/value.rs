use crate::{
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

/// A `JSValue` is either a primitive value or a reference to an object.
#[derive(Debug, Clone, PartialEq)]
pub enum JSValue {
    Undefined,
    Bool(bool),
    Number(JSNumber),
    String(String),
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
    ///     JSValue::from("1").to_string(&mut heap).unwrap(),
    ///     "\"1\""
    /// );
    /// assert_eq!(
    ///     JSValue::from(1).to_string(&mut heap).unwrap(),
    ///     "1"
    /// );
    ///
    /// let json_object = json!({"one": 1});
    /// let example_object = heap.object_from_json(&json_object);
    /// assert_eq!(
    ///     example_object.to_string(&mut heap).unwrap(),
    ///     "{ one: 1 }"
    /// );
    ///
    /// let json_array = json!([1, 2]);
    /// let example_array = heap.object_from_json(&json_array);
    /// assert_eq!( example_array.to_string(&mut heap).unwrap(), "[1, 2]" );
    /// ```
    pub fn to_string(&self, heap: &mut Heap) -> JSResult<String> {
        match self {
            JSValue::String(s) => Ok(JSON::from(s.as_str()).to_string()),
            JSValue::Ref(heapref) => heap.get(*heapref).clone().to_string(heap),
            _ => self.stringify(heap),
        }
    }

    /// stringify() makes everything into a string
    /// used for evaluation in a string context.
    /// It corresponds to .toString() in JavaScript
    pub fn stringify(&self, heap: &mut Heap) -> JSResult<String> {
        match self {
            JSValue::Undefined => Ok("undefined".to_string()),
            JSValue::Bool(b) => Ok(b.to_string()),
            JSValue::Number(n) => Ok(n.to_string()),
            JSValue::String(s) => Ok(s.clone()),
            JSValue::Ref(r) if r == &Heap::NULL => Ok("null".to_string()),
            JSValue::Ref(r) => match heap.lookup_protochain(*r, "toString") {
                Some(to_string) => {
                    let funcref = to_string.to_ref(heap)?;
                    let result = heap.execute(
                        funcref,
                        CallContext {
                            this_ref: *r,
                            method_name: "toString".to_string(),
                            arguments: vec![],
                            loc: None,
                        },
                    )?;
                    Ok(result.to_value(heap)?.stringify(heap)?)
                }
                None => Ok("[object Object]".to_string()),
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
            JSValue::String(s) => s.parse::<JSNumber>().ok(),
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
            JSValue::String(s) => !s.is_empty(),
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
                (Some(lval), Some(rval)) => (lval == rval),
                _ => false,
            },
            _ => match (self.numberify(heap), other.numberify(heap)) {
                (Some(lnum), Some(rnum)) => (lnum == rnum),
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
            return Ok(JSValue::from(lstr.to_string() + rstr));
        }
        if let (Some(lnum), Some(rnum)) = (self.numberify(heap), other.numberify(heap)) {
            return Ok(JSValue::from(lnum + rnum));
        }
        let lvalstr = self.stringify(heap)?;
        let rvalstr = other.stringify(heap)?;
        Ok(JSValue::from(lvalstr + &rvalstr))
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
            return JSValue::from(stringly(lstr, rstr));
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

impl From<String> for JSValue {
    fn from(s: String) -> Self {
        JSValue::String(s)
    }
}

impl From<&str> for JSValue {
    fn from(s: &str) -> Self {
        JSValue::String(s.to_string())
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
            JSValue::Bool(b)
        } else if let Some(n) = json.as_f64() {
            JSValue::Number(n)
        } else if let Some(s) = json.as_str() {
            JSValue::String(s.to_string())
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