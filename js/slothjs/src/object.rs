use std::collections::HashMap;
use std::convert::TryFrom;
use std::str::FromStr;
use std::fmt;

use bitflags::bitflags;
use serde_json::json;

use crate::error::Exception;
use crate::ast;
use crate::heap::{Heap, JSRef};

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
    /// It's useful for checking if a value points to an object.
    pub fn to_ref(&self) -> Result<JSRef, Exception> {
        match self {
            JSValue::Ref(objref) => Ok(*objref),
            _ => {
                let what = Interpreted::Value(self.clone());
                Err(Exception::ReferenceNotAnObject(what))
            }
        }
    }

    #[allow(dead_code)]
    pub fn to_json(&self, heap: &Heap) -> Result<JSON, Exception> {
        match self {
            JSValue::Undefined => Ok(JSON::Null),
            JSValue::Bool(b) => Ok(JSON::from(*b)),
            JSValue::Number(n) => Ok(JSON::from(*n)),
            JSValue::String(s) => Ok(JSON::from(s.as_str())),
            JSValue::Ref(Heap::NULL) => Ok(JSON::Null),
            JSValue::Ref(href) => heap.get(*href).to_json(heap),
        }
    }

    pub fn is_string(&self) -> bool {
        match self {
            JSValue::String(_) => true,
            _ => false,
        }
    }

    /// to_string() makes a human-readable string representation of the value:
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
    pub fn to_string(&self, heap: &mut Heap) -> Result<String, Exception> {
        match self {
            JSValue::String(s) =>
                Ok(JSON::from(s.as_str()).to_string()),
            JSValue::Ref(heapref) => {
                heap.get(*heapref).clone().to_string(heap)
            }
            _ => self.stringify(heap)
        }
    }

    /// stringify() makes everything into a string
    /// used for evaluation in a string context.
    /// It corresponds to .toString() in JavaScript
    pub fn stringify(&self, heap: &mut Heap) -> Result<String, Exception> {
        match self {
            JSValue::Undefined => Ok("undefined".to_string()),
            JSValue::Bool(b) => Ok(b.to_string()),
            JSValue::Number(n) => Ok(n.to_string()),
            JSValue::String(s) => Ok(s.clone()),
            JSValue::Ref(Heap::NULL) => Ok("null".to_string()),
            JSValue::Ref(r) => {
                match heap.lookup_protochain(*r, "toString") {
                    Some(to_string) => {
                        let funcref = to_string.to_ref(heap)?;
                        let result = heap.execute(funcref, *r, "toString", vec![])?;
                        Ok(result.to_value(heap)?.stringify(heap)?)
                    }
                    None => Ok("[object Object]".to_string())
                }
            }
        }
    }

    /// numberify() tries to make everything into a numeric value
    /// for evalation in a numeric context.
    /// It is slightly more strict than `+value` in JavaScript: only
    /// `value.numberify().unwrap_or(f64::NAN)` corresponds to `+value` in JavaScript.
    pub fn numberify(&self, heap: &Heap) -> Option<JSNumber> {
        match self {
            JSValue::Bool(b) => Some(if *b { 1.0 } else { 0.0 }),
            JSValue::Number(n) => Some(*n),
            JSValue::String(s) => s.parse::<JSNumber>().ok(),
            JSValue::Ref(Heap::NULL) => Some(0.0),
            JSValue::Ref(r) => {
                let object = heap.get(*r);
                if let Some(array) = object.as_array() {
                    match &array.storage[..] {
                        [] => Some(0.0),                // +[]  == 0
                        [val] => val.numberify(heap),   // +[x] == x
                        _ => None,                      // +[x, y, ..] == NaN
                    }
                } else {
                    object.to_primitive(heap).and_then(|v| v.numberify(heap))
                }
            }
            _ => None
        }
    }

    /// boolify() treats everythings as a truthy value.
    /// ES5: ToBoolean
    pub fn boolify(&self, heap: &Heap) -> bool {
        match self {
            JSValue::Undefined => false,
            JSValue::String(s) => s.len() > 0,
            JSValue::Ref(Heap::NULL) => false,
            JSValue::Ref(_) => true,
            _ =>
                if let Some(n) = self.numberify(heap) {
                    !(n == 0.0 || f64::is_nan(n))
                } else {
                    true
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
            JSValue::Bool(b) =>
                heap.alloc(JSObject::from_bool(*b)),
            JSValue::Number(_n) =>
                todo!(),    // TODO: Number object
            JSValue::String(_s) =>
                todo!(),    // TODO: String object
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
            JSValue::Ref(r) => {
                match heap.get(*r).value {
                    ObjectValue::Closure(_) |
                    ObjectValue::VMCall(_) =>
                        "function",
                    _ => "object"
                }
            }
        }
    }

    /// Abstract Equality Comparison, `==`:
    /// <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Equality_comparisons_and_sameness#Loose_equality_using_>
    pub fn loose_eq(&self, other: &JSValue, heap: &Heap) -> bool {
        let lval = if self == &JSValue::Ref(Heap::NULL) { &JSValue::Undefined } else { self };
        let rval = if other == &JSValue::Ref(Heap::NULL) { &JSValue::Undefined } else { other };
        match (lval, rval) {
            (JSValue::Undefined, JSValue::Undefined) => true,
            (JSValue::Undefined, _) | (_, JSValue::Undefined) => false,
            (JSValue::Number(_), JSValue::Number(_))
                | (JSValue::String(_), JSValue::String(_))
                | (JSValue::Bool(_), JSValue::Bool(_))
                => self == other,
            (JSValue::Ref(lref), JSValue::Ref(rref)) if lref == rref => true,
            (JSValue::Ref(lref), JSValue::Ref(rref)) =>
                match (heap.get(*lref).to_primitive(heap), heap.get(*rref).to_primitive(heap)) {
                    (Some(lval), Some(rval)) => (lval == rval),
                    _ => false,
                },
            _ =>
                match (self.numberify(heap), other.numberify(heap)) {
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
        where F: Fn(f64, f64) -> f64
    {
        let val = match (self.numberify(heap), other.numberify(heap)) {
            (Some(lnum), Some(rnum)) => op(lnum, rnum),
            _ => f64::NAN
        };
        JSValue::Number(val)
    }

    /// Addition operator:
    /// <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Addition>
    pub fn plus(&self, other: &JSValue, heap: &mut Heap) -> Result<JSValue, Exception> {
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
    pub fn minus(&self, other: &JSValue, heap: &Heap) -> Result<JSValue, Exception> {
        Ok(JSValue::numerically(self, other, heap, |a, b| a - b))
    }

    pub fn compare<
        StrCmpFn: Fn(&str, &str) -> bool,
        NumCmpFn: Fn(f64, f64) -> bool,
    >(
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
    fn from(b: bool) -> Self { JSValue::Bool(b) }
}

impl From<JSNumber> for JSValue {
    fn from(number: JSNumber) -> Self { JSValue::Number(number) }
}

impl From<i64> for JSValue {
    fn from(number: i64) -> Self { JSValue::Number(number as JSNumber) }
}

impl From<String> for JSValue {
    fn from(s: String) -> Self { JSValue::String(s) }
}

impl From<&str> for JSValue {
    fn from(s: &str) -> Self { JSValue::String(s.to_string()) }
}

impl From<JSRef> for JSValue {
    fn from(r: JSRef) -> Self { JSValue::Ref(r) }
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
    assert_eq!( JSValue::from("5").numberify(&dummy),    Some(5.0) );
}

#[test]
fn test_boolify() {
    let dummy = Heap::new();

    // true
    assert!( JSValue::from(true).boolify(&dummy) );
    assert!( JSValue::from(1).boolify(&dummy) );
    assert!( JSValue::from("0").boolify(&dummy) );
    //assert!( JSValue::from(json!([])).boolify(&dummy) );

    // false
    assert!( !JSValue::from(false).boolify(&dummy) );
    assert!( !JSValue::from(0).boolify(&dummy) );
    assert!( !JSValue::from(f64::NAN).boolify(&dummy) );
    assert!( !JSValue::from("").boolify(&dummy) );
    assert!( !JSValue::NULL.boolify(&dummy) );
}

/// Javascript objects.
/// A `JSObject` always has a `proto`.
/// It can have an optional `ObjectValue` (a primitive or array/function/closure).
/// It has a dictionary of `properties`.
#[derive(Debug, Clone)]
pub struct JSObject {
    pub proto: JSRef,
    pub value: ObjectValue,
    pub properties: HashMap<String, Property>,
}

impl JSObject {
    pub fn new() -> JSObject {
        JSObject{
            proto: Heap::OBJECT_PROTO,
            value: ObjectValue::None,
            properties: HashMap::new(),
        }
    }

    /// Wrap the given native call into a Function.
    pub fn from_func(f: NativeFunction) -> JSObject {
        JSObject{
            proto: Heap::FUNCTION_PROTO,
            value: ObjectValue::VMCall(VMCall(f)),
            properties: HashMap::new(),
        }
    }

    /// Wrap the given `closure` into a Function.
    pub fn from_closure(closure: Closure) -> JSObject {
        let params_count = closure.params.len() as f64;
        let mut function_object = JSObject{
            proto: Heap::FUNCTION_PROTO,
            value: ObjectValue::Closure(Box::new(closure)),
            properties: HashMap::new(),
        };
        function_object.set_nonconf("length", Content::from(params_count)).unwrap();
        function_object
    }

    /// Wrap the given vector into an Array.
    pub fn from_array(values: Vec<JSValue>) -> JSObject {
        JSObject{
            proto: Heap::ARRAY_PROTO,
            value: ObjectValue::Array(JSArray{ storage: values }),
            properties: HashMap::new(),
        }
    }

    pub fn from_bool(value: bool) -> JSObject {
        JSObject{
            proto: Heap::BOOLEAN_PROTO,
            value: ObjectValue::Boolean(value),
            properties: HashMap::new(),
        }
    }

    /// It's roughly `Object.valueOf(self)`
    pub fn to_primitive(&self, _heap: &Heap) -> Option<JSValue> {
        use ObjectValue::*;
        match &self.value {
            Boolean(b) => Some(JSValue::Bool(*b)),
            Number(n) => Some(JSValue::Number(*n)),
            String(s) => Some(JSValue::String(s.clone())),
            _ => Option::None,
        }
    }


    /// It `self` is an Array, give its underlying storage.
    pub fn as_array(&self) -> Option<&JSArray> {
        match &self.value {
            ObjectValue::Array(array) => Some(array),
            _ => None,
        }
    }

    /// It `self` is an Array, give its underlying storage mutably.
    pub fn as_array_mut(&mut self) -> Option<&mut JSArray> {
        match &mut self.value {
            ObjectValue::Array(array) => Some(array),
            _ => None,
        }
    }

    /// Tries to get JSValue of the own property `name`.
    /// This might call getters of the property.
    pub fn get_value(&self, name: &str) -> Option<&JSValue> {
        if let Some(array) = self.as_array() {
            if let Ok(index) = usize::from_str(name) {
                if let Some(value) = array.storage.get(index) {
                    return Some(value);
                }
            }
        }
        self.properties.get(name).and_then(|prop|
            match &prop.content {
                Content::Value(value) => Some(value),
            }
        )
    }

    fn set_maybe_nonwritable(
        &mut self,
        name: &str,
        content: Content,
        access: Access,
        even_nonwritable: bool,
    ) -> Result<(), Exception> {
        if let Ok(index) = usize::from_str(name) {
            if let Some(array) = self.as_array_mut() {
                // TODO: a[100500] will be interesting.
                while array.storage.len() <= index {
                    array.storage.push(JSValue::Undefined);
                }
                let value = content.to_value()?;
                array.storage[index] = value;
                return Ok(());
            }
        }

        match self.properties.get_mut(name) {
            Some(property) => {
                if property.access != access && !property.access.configurable() {
                    let what = Interpreted::from("???");  // TODO
                    let name = name.to_string();
                    return Err(Exception::TypeErrorNotConfigurable(what, name));
                }

                if !(even_nonwritable || property.access.writable()) {
                    let what = Interpreted::from("???");  // TODO
                    let name = name.to_string();
                    return Err(Exception::TypeErrorSetReadonly(what, name));
                }

                property.access = access;
                property.content = content;
            }
            None => {
                let prop = Property{ content, access };
                self.properties.insert( name.to_string(), prop);
            }
        }
        Ok(())
    }

    /// If the own property `name` does not exist, create it with the given `content` and `access`.
    /// Otherwise, if `name` is a number and `self` is an Array, assign the value of `content`
    /// into the array.
    /// Otherwise:
    /// - if the existing own property is not configurable and the given `access` differs, fail.
    /// - if the existing own property is not writable, fail
    /// - replace `content` and `access` of the property.
    pub fn set(
        &mut self,
        name: &str,
        content: Content,
        access: Access
    ) -> Result<(), Exception> {
        self.set_maybe_nonwritable(name, content, access, false)
    }

    /// Just like `.set()`, but updates even readonly properties.
    pub fn set_even_nonwritable(
        &mut self,
        name: &str,
        content: Content,
        access: Access
    ) -> Result<(), Exception> {
        self.set_maybe_nonwritable(name, content, access, true)
    }

    /// If `name` is a number and `self` is an Array, just set the array elemnt to `value`.
    /// Otherwise: if the own property `name` does not exist, create it with `Access::all()` and
    /// set to `Content::from(value)`.
    /// If the own property exists already, call `.set()` with its current access. This will fail
    /// to update non-writable properties.
    pub fn update(&mut self, name: &str, value: JSValue) -> Result<(), Exception> {
        let access = self.properties.get(name).map(|prop| prop.access).unwrap_or(Access::all());
        self.set(name, Content::from(value), access)
    }

    /// Just like `.update()`, but updates even non-writable properties.
    pub fn update_even_nonwritable(&mut self, name: &str, value: JSValue) -> Result<(), Exception> {
        let access = self.properties.get(name).map(|prop| prop.access).unwrap_or(Access::all());
        self.set_even_nonwritable(name, Content::from(value), access)
    }

    // are these shortcuts a good idea?
    pub fn set_property(&mut self, name: &str, content: Content) -> Result<(), Exception> {
        self.set(name, content, Access::all())
    }

    pub fn set_system(&mut self, name: &str, content: Content) -> Result<(), Exception> {
        self.set(name, content, Access::empty())
    }

    pub fn set_hidden(&mut self, name: &str, content: Content) -> Result<(), Exception> {
        self.set(name, content, Access::HIDDEN)
    }

    pub fn set_nonconf(&mut self, name: &str, content: Content) -> Result<(), Exception> {
        self.set(name, content, Access::NONCONF)
    }

    pub fn set_readonly(&mut self, name: &str, content: Content) -> Result<(), Exception> {
        self.set(name, content, Access::READONLY)
    }


    /// Create a `JSON` from this `JSObject`.
    pub fn to_json(&self, heap: &Heap) -> Result<JSON, Exception> {
        if let Some(array) = self.as_array() {
            let jvals = array.storage.iter()
                .map(|v| v.to_json(heap))
                .collect::<Result<Vec<_>, Exception>>()?;
            return Ok(JSON::Array(jvals));
        }

        let mut json = json!({});
        for (key, property) in self.properties.iter() {
            if !property.access.enumerable() {
                continue
            }

            let jvalue = property.content.to_value()?.to_json(heap)?;
            json[key] = jvalue;
        }
        Ok(json)
    }

    /// Create a human-readable representation of contents of an Array or an Object.
    pub fn to_string(&self, heap: &mut Heap) -> Result<String, Exception> {
        fn is_valid_identifier(s: &str) -> bool {
            let is_start = |c: char| (c.is_alphabetic() || c == '_' || c == '$');

            let mut it = s.chars();
            if let Some(c) = it.next() {
                is_start(c) && it.all(|c| is_start(c) || c.is_numeric())
            } else {
                false
            }
        }

        let is_array = self.as_array().is_some();

        let mut s = String::new();
        let mut empty = true;

        if let Some(array) = self.as_array() {
            s.push('[');
            for item in array.storage.iter() {
                empty = false;
                let itemstr = item.to_string(heap)?;
                s.push_str(&itemstr);
                s.push(','); s.push(' ');
            }
            if !empty { s.pop(); s.push(' '); }
        } else {
            s.push('{');
        }

        for (key, property) in self.properties.iter() {
            if !property.access.enumerable() {
                continue;
            }

            s.push(' ');
            if is_valid_identifier(&key) {
                s.push_str(key);
            } else {
                let skey = JSON::from(key.as_str()).to_string();
                s.push_str(&skey);
            }
            s.push_str(": ");
            let val = property.content.to_value()?.to_string(heap)?;
            s.push_str(&val);
            s.push(',');
            empty = false;
        }
        if is_array {
            if !empty { s.pop(); s.pop(); }
            s.push(']');
        } else {
            if !empty { s.pop(); s.push(' '); }
            s.push('}');
        }
        Ok(s)
    }
}


/// `ObjectValue` is used:
/// - as the primitive value of a `Number`/`Boolean`/`String` object;
/// - as the function entry in a `Function`.
/// - as optimizied storage in an `Array`
#[derive(Debug, Clone)]
pub enum ObjectValue {
    None,

    // primitive values
    Boolean(bool),
    Number(JSNumber),
    String(String),

    // Function
    VMCall(VMCall),
    Closure(Box<Closure>),

    // Array
    Array(JSArray),
}

impl ObjectValue {
    pub fn from_func(func: NativeFunction) -> ObjectValue {
        ObjectValue::VMCall(VMCall(func))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Property {
    /// A `JSValue` or accessors
    pub content: Content,
    /// (non)writable | (non)confiurable | (non)enumerable
    pub access: Access,
}

impl Property {
    pub fn to_ref(&self) -> Option<JSRef> {
        match self {
            Property{ content: Content::Value(JSValue::Ref(r)), .. } => Some(*r),
            _ => None,
        }
    }
}


bitflags! {
    pub struct Access: u8 {
        const ENUM = 0b001;
        const CONF = 0b010;
        const WRITE = 0b100;

        const HIDDEN = Self::CONF.bits | Self::WRITE.bits;
        const READONLY = Self::ENUM.bits | Self::CONF.bits;
        const NONCONF = Self::ENUM.bits | Self::WRITE.bits;
    }
}

impl Access {
    pub fn enumerable(&self) -> bool { self.contains(Access::ENUM) }
    pub fn configurable(&self) -> bool { self.contains(Access::CONF) }
    pub fn writable(&self) -> bool { self.contains(Access::WRITE) }
}


#[derive(Debug, Clone, PartialEq)]
pub enum Content {
    Value(JSValue),
    /*
    Accessor{
        get: Option<JSRef>,
        set: Option<JSRef>,
    },
    */
}

impl Content {
    /// This might call getters of the property.
    pub fn to_value(&self) -> Result<JSValue, Exception> {
        match self {
            Self::Value(value) => Ok(value.clone()),
        }
    }
}

impl<T> From<T> for Content where JSValue: From<T> {
    fn from(x: T) -> Content { Content::Value(JSValue::from(x)) }
}

/// A wrapper for NativeFunction to give it `fmt::Debug`.
#[derive(Clone)]
pub struct VMCall(NativeFunction);

impl VMCall {
    pub fn call(&self,
        this_ref: JSRef,
        method_name: String,
        arguments: Vec<Interpreted>,
        heap: &mut Heap
    ) -> Result<Interpreted, Exception> {
        self.0(this_ref, method_name, arguments, heap)
    }
    pub fn ptr(&self) -> usize {
        self.0 as *const () as usize
    }
}

impl fmt::Debug for VMCall {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "VMCall(*{:x})", self.ptr())
    }
}

pub type NativeFunction = fn(
    this_ref: JSRef,
    method_name: String,
    arguments: Vec<Interpreted>,
    heap: &'_ mut Heap,
) -> Result<Interpreted, Exception>;


#[derive(Clone, Debug)]
pub struct Closure {
    pub id: Option<ast::Identifier>,
    pub params: Vec<ast::Identifier>,
    pub body: Box<ast::BlockStatement>,
    pub captured_scope: JSRef,
}

/// The underlying storage of an Array object.
#[derive(Clone, Debug)]
pub struct JSArray {
    pub storage: Vec<JSValue>,
}

impl JSArray {}

#[derive(Debug, Clone, PartialEq)]
pub enum Interpreted {
    /// An object member; might not exist yet.
    Member{ of: JSRef, name: String },

    /// A value
    Value(JSValue),
}

impl Interpreted {
    pub const VOID: Interpreted = Interpreted::Value(JSValue::Undefined);
    pub const NAN: Interpreted = Interpreted::Value(JSValue::Number(f64::NAN));

    pub fn member(of: JSRef, name: &str) -> Interpreted {
        Interpreted::Member{ of, name: name.to_string() }
    }

    pub fn to_value(&self, heap: &Heap) -> Result<JSValue, Exception> {
        let value = match self {
            Interpreted::Value(value) =>
                value.clone(),
            Interpreted::Member{of, name} => {
                match heap.get(*of).get_value(name) {
                    Some(value) => value.clone(),
                    None => JSValue::Undefined,
                }
            }
        };
        Ok(value)
    }

    pub fn to_ref(&self, heap: &Heap) -> Result<JSRef, Exception> {
        match self {
            Interpreted::Value(JSValue::Ref(r)) =>
                Ok(*r),
            Interpreted::Member{of, name} =>
                match heap.get(*of).get_value(name) {
                    Some(JSValue::Ref(r)) => Ok(*r),
                    _ => Err(Exception::TypeErrorGetProperty(self.clone(), name.to_string()))
                }
            _ => Err(Exception::ReferenceNotAnObject(self.clone()))
        }
    }

    /// Corresponds to Javascript `delete` operator and all its weirdness.
    /// `Ok`/`Err` correspond to `true`/`false` from `delete`.
    /// <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/delete>
    pub fn delete(&self, heap: &mut Heap) -> Result<(), Exception> {
        match self {
            Interpreted::Member{ of, name } => {
                let object = heap.get_mut(*of);
                let configurable = match object.properties.get(name) {
                    Some(p) => p.access.configurable(),
                    None => return Ok(()),
                };
                if configurable {
                    object.properties.remove(name);
                    Ok(())
                } else {
                    let what = Interpreted::from(*of);
                    Err(Exception::TypeErrorNotConfigurable(what, name.to_string()))
                }
            }
            _ => Ok(()),
        }
    }
}

impl<T> From<T> for Interpreted where JSValue: From<T>  {
    fn from(value: T) -> Interpreted { Interpreted::Value(JSValue::from(value)) }
}
