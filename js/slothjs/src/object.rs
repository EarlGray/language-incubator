use bitflags::bitflags;
use serde_json::json;

use crate::error::TypeError;
use crate::prelude::*;

use crate::function::{Closure, HostFn, HostFunc};
use crate::{Exception, Heap, JSNumber, JSRef, JSResult, JSString, JSValue, JSON};

/// Javascript objects.
/// A `JSObject` always has a `proto`.
/// It can have an optional `ObjectValue` (a primitive or array/function/closure).
/// It has a dictionary of `properties`.
#[derive(Debug, Clone)]
pub struct JSObject {
    pub proto: JSRef,
    pub value: ObjectValue,
    pub properties: HashMap<JSString, Property>, // TODO: StrKey
                                                 // TODO: make fields private
}

impl JSObject {
    pub fn new() -> JSObject {
        JSObject {
            proto: Heap::OBJECT_PROTO,
            value: ObjectValue::None,
            properties: HashMap::new(),
        }
    }

    /// Wrap the given native call into a Function.
    pub fn from_func(f: HostFn) -> JSObject {
        JSObject {
            proto: Heap::FUNCTION_PROTO,
            value: ObjectValue::from_func(f),
            properties: HashMap::new(),
        }
    }

    /// Wrap the given `closure` into a Function.
    pub fn from_closure(closure: Closure) -> JSObject {
        let params_count = closure.function.params.len() as f64;
        let mut function_object = JSObject {
            proto: Heap::FUNCTION_PROTO,
            value: ObjectValue::Closure(closure),
            properties: HashMap::new(),
        };
        function_object
            .set_nonconf("length", Content::from(params_count))
            .unwrap();
        function_object
    }

    /// Wrap the given vector into an Array.
    pub fn from_array(values: Vec<JSValue>) -> JSObject {
        JSObject {
            proto: Heap::ARRAY_PROTO,
            value: ObjectValue::Array(JSArray { storage: values }),
            properties: HashMap::new(),
        }
    }

    /// Wrap the given bool into Boolean
    pub fn from_bool(value: bool) -> JSObject {
        JSObject {
            proto: Heap::BOOLEAN_PROTO,
            value: ObjectValue::Boolean(value),
            properties: HashMap::new(),
        }
    }

    /// Wrap the given string into String
    fn from_string(value: JSString) -> JSObject {
        let mut properties = HashMap::new();
        // TODO: String.prototype.length
        properties.insert(
            JSString::from("length"),
            Property {
                access: Access::empty(),
                content: Content::from(value.as_str().chars().count() as i64),
            },
        );
        JSObject {
            proto: Heap::STRING_PROTO,
            value: ObjectValue::String(value),
            properties,
        }
    }

    /// It's roughly `Object.valueOf(self)`
    pub fn to_primitive(&self) -> Option<JSValue> {
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

    /// If `self` is an Array, give its underlying storage mutably.
    pub fn as_array_mut(&mut self) -> Option<&mut JSArray> {
        match &mut self.value {
            ObjectValue::Array(array) => Some(array),
            _ => None,
        }
    }

    /// If `self` is a String, get it primitive value
    pub fn as_str(&self) -> Option<&str> {
        match &self.value {
            ObjectValue::String(s) => Some(s.as_str()),
            _ => None,
        }
    }

    #[allow(clippy::match_like_matches_macro)]
    pub fn is_callable(&self) -> bool {
        match self.value {
            ObjectValue::HostFn(_) | ObjectValue::Closure(_) => true,
            _ => false,
        }
    }

    /// Tries to get JSValue of the own property `name`.
    /// This might call getters of the property.
    pub fn get_own_value(&self, name: &str) -> Option<JSValue> {
        // indexing
        if let Ok(index) = usize::from_str(name) {
            match &self.value {
                ObjectValue::Array(array) => {
                    if let Some(value) = array.storage.get(index) {
                        return Some(value.clone());
                    }
                }
                ObjectValue::String(s) => {
                    // TODO: optimizie nth()'s sequential access
                    if let Some(c) = s.chars().nth(index) {
                        return Some(JSValue::from(c.to_string()));
                    }
                }
                _ => (),
            }
        } else if name == "length" {
            // TODO: make this hack a regular getter once getters are ready
            match &self.value {
                ObjectValue::Array(array) => {
                    return Some(JSValue::from(array.storage.len() as i64))
                }
                ObjectValue::Closure(closure) => {
                    return Some(JSValue::from(closure.function.params.len() as i64))
                }
                ObjectValue::String(s) => return Some(JSValue::from(s.len() as i64)),
                _ => (),
            }
        }

        self.properties.get(name).map(|prop| match &prop.content {
            Content::Value(value) => value.clone(),
        })
    }

    /// Check own and all inherited properties for `name` and returns the first found value.
    /// ES5: \[\[Get\]\], None corresponds to `undefined`
    pub fn lookup_value(&self, name: &str, heap: &Heap) -> Option<JSValue> {
        if let Some(value) = self.get_own_value(name) {
            return Some(value);
        }
        for protoref in self.protochain(heap) {
            if let Some(value) = heap.get(protoref).get_own_value(name) {
                return Some(value);
            }
        }
        None
    }

    pub fn protochain<'a>(&self, heap: &'a Heap) -> ProtoChainIter<'a> {
        ProtoChainIter {
            heap,
            protoref: self.proto,
        }
    }

    fn set_maybe_nonwritable(
        &mut self,
        name: &str,
        content: Content,
        access: Access,
        even_nonwritable: bool,
    ) -> JSResult<()> {
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
                    let what = Interpreted::from("???"); // TODO
                    return Err(Exception::TypeErrorNotConfigurable(
                        what,
                        JSString::from(name),
                    ));
                }

                if !(even_nonwritable || property.access.writable()) {
                    let what = Interpreted::from("???"); // TODO
                    return Err(Exception::attr_type_error(
                        TypeError::SET_READONLY,
                        what,
                        name,
                    ));
                }

                property.access = access;
                property.content = content;
            }
            None => {
                let prop = Property { content, access };
                self.properties.insert(JSString::from(name), prop);
            }
        }
        Ok(())
    }

    pub fn define_own_property(&mut self, name: &str, access: Access) -> JSResult<()> {
        let content = Content::from(JSValue::Undefined);
        self.set_maybe_nonwritable(name, content, access, true)
    }

    /// - if own property `name` does not exist, create it with the given `content` and `access`.
    /// - if `name` is a number and `self` is an Array, assign value of `content` into the array.
    /// - if the existing own property is not configurable and the given `access` differs, fail.
    /// - if the existing own property is not writable, fail
    /// - else: replace `content` and `access` of the property.
    fn set(&mut self, name: &str, content: Content, access: Access) -> JSResult<()> {
        self.set_maybe_nonwritable(name, content, access, false)
    }

    /// If `name` is a number and `self` is an Array, just set the array elemnt to `value`.
    /// Otherwise: if the own property `name` does not exist, create it with `Access::all()` and
    /// set to `Content::from(value)`.
    /// If the own property exists already, call `.set()` with its current access. This will fail
    /// to update non-writable properties.
    /// ES5: \[\[Put\]\] with strict error handing
    pub fn set_property<V>(&mut self, name: &str, value: V) -> JSResult<()>
    where
        Content: From<V>,
    {
        let access = (self.properties.get(name))
            .map(|prop| prop.access)
            .unwrap_or(Access::all());
        self.set(name, Content::from(value), access)
    }

    /// Just like `.set_property()`, but updates even non-writable properties.
    pub fn set_even_nonwritable<V>(&mut self, name: &str, value: V) -> JSResult<()>
    where
        Content: From<V>,
    {
        let access = (self.properties.get(name))
            .map(|prop| prop.access)
            .unwrap_or(Access::all());
        self.set_maybe_nonwritable(name, Content::from(value), access, true)
    }

    // are these shortcuts a good idea?
    /// A shortcut for `define_own_property(Access::NONE)` and assigning the value.
    pub fn set_system<V>(&mut self, name: &str, value: V) -> JSResult<()>
    where
        Content: From<V>,
    {
        self.set(name, Content::from(value), Access::empty())
    }

    /// A shortcut for defining a non-enumerable property and setting its value.
    pub fn set_hidden<V>(&mut self, name: &str, value: V) -> JSResult<()>
    where
        Content: From<V>,
    {
        self.set(name, Content::from(value), Access::HIDDEN)
    }

    /// A shortcut for defining a non-configurable property and setting its value.
    pub fn set_nonconf<V>(&mut self, name: &str, value: V) -> JSResult<()>
    where
        Content: From<V>,
    {
        self.set(name, Content::from(value), Access::NONCONF)
    }

    /// A shortcut for defining a non-writable property and setting its value.
    pub fn set_readonly<V>(&mut self, name: &str, value: V) -> JSResult<()>
    where
        Content: From<V>,
    {
        self.set(name, Content::from(value), Access::READONLY)
    }

    /// Create a `JSON` from this `JSObject`.
    pub fn to_json(&self, heap: &Heap) -> JSResult<JSON> {
        if let Some(array) = self.as_array() {
            let jvals = (array.storage.iter())
                .map(|v| v.to_json(heap))
                .collect::<JSResult<Vec<_>>>()?;
            return Ok(JSON::Array(jvals));
        }

        let mut json = json!({});
        for (key, property) in self.properties.iter() {
            if !property.access.enumerable() {
                continue;
            }

            let jvalue = property.content.to_value()?.to_json(heap)?;
            json[key.to_string()] = jvalue;
        }
        Ok(json)
    }

    /// Create a human-readable representation of contents of an Array or an Object.
    pub fn to_string(&self, heap: &mut Heap) -> JSResult<JSString> {
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
                s.push(',');
                s.push(' ');
            }
            if !empty {
                s.pop();
                s.push(' ');
            }
        } else {
            s.push('{');
        }

        for (key, property) in self.properties.iter() {
            if !property.access.enumerable() {
                continue;
            }

            s.push(' ');
            if is_valid_identifier(key) {
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
            if !empty {
                s.pop();
                s.pop();
            }
            s.push(']');
        } else {
            if !empty {
                s.pop();
                s.push(' ');
            }
            s.push('}');
        }
        Ok(JSString::from(s))
    }
}

impl<S> From<S> for JSObject
where
    JSString: From<S>,
{
    fn from(s: S) -> Self {
        JSObject::from_string(JSString::from(s))
    }
}

/*
impl From<&str> for JSObject {
    fn from(s: &str) -> Self {
        JSObject::from_string(JSString::from(s))
    }
}
*/

impl Default for JSObject {
    fn default() -> Self {
        JSObject::new()
    }
}

pub struct ProtoChainIter<'a> {
    heap: &'a Heap,
    protoref: JSRef,
}

impl<'a> Iterator for ProtoChainIter<'a> {
    type Item = JSRef;

    fn next(&mut self) -> Option<Self::Item> {
        match self.protoref {
            Heap::NULL => None,
            prevref => {
                self.protoref = self.heap.get(prevref).proto;
                Some(prevref)
            }
        }
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
    String(JSString),

    // Function
    HostFn(HostFunc),
    Closure(Closure),

    // Array
    Array(JSArray),
}

impl ObjectValue {
    pub fn from_func(func: HostFn) -> ObjectValue {
        ObjectValue::HostFn(HostFunc::from(func))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Property {
    /// A `JSValue` or accessors
    pub content: Content,
    /// (non)writable | (non)configurable | (non)enumerable
    pub access: Access,
}

impl Property {
    pub fn to_ref(&self) -> Option<JSRef> {
        match self {
            Property {
                content: Content::Value(JSValue::Ref(r)),
                ..
            } => Some(*r),
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
    pub fn new(configurable: bool, enumerable: bool, writable: bool) -> Access {
        let mut access = Access::empty();
        if configurable {
            access |= Access::CONF;
        }
        if enumerable {
            access |= Access::ENUM;
        }
        if writable {
            access |= Access::WRITE;
        }
        access
    }

    pub fn enumerable(&self) -> bool {
        self.contains(Access::ENUM)
    }
    pub fn configurable(&self) -> bool {
        self.contains(Access::CONF)
    }
    pub fn writable(&self) -> bool {
        self.contains(Access::WRITE)
    }
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
    pub fn to_value(&self) -> JSResult<JSValue> {
        match self {
            Self::Value(value) => Ok(value.clone()),
        }
    }
}

impl<T> From<T> for Content
where
    JSValue: From<T>,
{
    fn from(x: T) -> Content {
        Content::Value(JSValue::from(x))
    }
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
    Member { of: JSRef, name: JSString },

    /// A value
    Value(JSValue),
}

impl Interpreted {
    pub const VOID: Interpreted = Interpreted::Value(JSValue::Undefined);
    pub const NAN: Interpreted = Interpreted::Value(JSValue::Number(f64::NAN));

    /// A convenience wrapper for Interpreted::Member{} construction
    pub fn member(of: JSRef, name: &str) -> Interpreted {
        Interpreted::Member {
            of,
            name: name.into(),
        }
    }

    /// If Interpreted::Value, unwrap;
    /// if Interpreted::Member{of, name}, [`JSObject::lookup_value`] of `name` in `of`.
    pub fn to_value(&self, heap: &Heap) -> JSResult<JSValue> {
        match self {
            Interpreted::Value(value) => Ok(value.clone()),
            Interpreted::Member { of, name } => {
                if let Some(value) = heap.get(*of).lookup_value(name, heap) {
                    Ok(value)
                } else if heap.is_scope(*of) {
                    Err(Exception::no_reference(name.clone()))
                } else {
                    Ok(JSValue::Undefined)
                }
            }
        }
    }

    pub fn to_ref(&self, heap: &Heap) -> JSResult<JSRef> {
        match self {
            Interpreted::Value(JSValue::Ref(r)) => Ok(*r),
            Interpreted::Member { of, name } => match heap.get(*of).lookup_value(name, heap) {
                Some(JSValue::Ref(r)) => Ok(r),
                None if heap.is_scope(*of) => Err(Exception::no_reference(name.clone())),
                _ => Err(Exception::TypeErrorGetProperty(self.clone(), name.clone())),
            },
            _ => Err(Exception::not_an_object(self.clone())),
        }
    }

    pub fn put_value(&self, value: JSValue, heap: &mut Heap) -> JSResult<()> {
        match self {
            Interpreted::Member { of, name } => {
                heap.get_mut(*of).set_property(name.as_str(), value)
            }
            _ => Err(Exception::TypeErrorCannotAssign(self.clone())),
        }
    }

    /// Resolve self to: a callable JSRef, `this` JSRef and the method name.
    pub fn resolve_call(&self, heap: &Heap) -> JSResult<(JSRef, JSRef, JSString)> {
        match self {
            Interpreted::Member { of: this_ref, name } => {
                let of = match heap.lookup_protochain(*this_ref, name) {
                    Some(Interpreted::Member { of, .. }) => of,
                    Some(_) => unreachable!(),
                    None => return Err(Exception::TypeErrorNotCallable(self.clone())),
                };
                let func_value = (heap.get(of).get_own_value(name)).ok_or_else(|| {
                    Exception::TypeErrorNotCallable(Interpreted::member(of, name))
                })?;
                let func_ref = (func_value.to_ref())
                    .map_err(|_| Exception::TypeErrorNotCallable(Interpreted::member(of, name)))?;
                Ok((func_ref, *this_ref, name.clone()))
            }
            Interpreted::Value(JSValue::Ref(func_ref)) => {
                let this_ref = Heap::GLOBAL; // TODO: figure out what is this
                Ok((*func_ref, this_ref, "<anonymous>".into()))
            }
            _ => Err(Exception::TypeErrorNotCallable(self.clone())),
        }
    }

    /// Corresponds to Javascript `delete` operator and all its weirdness.
    /// `Ok`/`Err` correspond to `true`/`false` from `delete`.
    /// <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/delete>
    pub fn delete(&self, heap: &mut Heap) -> JSResult<()> {
        match self {
            Interpreted::Member { of, name } => {
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
                    Err(Exception::TypeErrorNotConfigurable(what, name.clone()))
                }
            }
            _ => Ok(()),
        }
    }
}

impl<T> From<T> for Interpreted
where
    JSValue: From<T>,
{
    fn from(value: T) -> Interpreted {
        Interpreted::Value(JSValue::from(value))
    }
}

/// A description of a JavaScript prototype+constructor with host methods.
pub struct HostClass {
    pub name: &'static str,
    pub constructor: HostFn,
    pub methods: &'static [(&'static str, HostFn)],
    pub static_methods: &'static [(&'static str, HostFn)],
}
