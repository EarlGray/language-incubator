use std::collections::HashMap;
use std::convert::TryFrom;
use std::fmt;

use bitflags::bitflags;
use serde_json::json;

use crate::error::Exception;
use crate::ast;
use crate::builtin;

pub type JSON = serde_json::Value;

pub type JSNumber = f64;

/// A value: either primitive or object
#[derive(Debug, Clone, PartialEq)]
pub enum JSValue {
    Undefined,
    Null,
    Bool(bool),
    String(String),
    //Symbol(String)
    Number(JSNumber),
    Object(JSObject),
}

impl JSValue {
    /// to_string() makes a human-readable string representation of the value:
    /// ```
    /// # use serde_json::json;
    /// # use slothjs::object::JSValue;
    /// # use slothjs::interpret::RuntimeState;
    /// # let mut state = RuntimeState::new();
    /// # let heap = &mut state.heap;
    /// assert_eq!( JSValue::from("1").to_string(heap), "\"1\"" );
    /// assert_eq!( JSValue::from(1).to_string(heap),    "1" );
    /// ```
    /// ```ignore
    /// let json_object = json!({"one": 1, "two": 2});
    /// let example_object = heap.object_from_json(&json_object);
    /// assert_eq!( example_object.to_string(heap), "{ one: 1, two: 2 }");
    ///
    /// let json_array = json!([1, 2]);
    /// let example_array = heap.object_from_json(&json_array);
    /// assert_eq!( example_array.to_string(heap), "[1,2]" );
    /// ```
    pub fn to_string(&self, heap: &Heap) -> String {
        match self {
            JSValue::Undefined => "undefined".to_string(),
            JSValue::Null => "null".to_string(),
            JSValue::Bool(b) => b.to_string(),
            JSValue::Number(n) => n.to_string(),
            JSValue::String(s) =>
                JSON::from(s.as_str()).to_string(),
            JSValue::Object(object) => {
                let mut s = String::new();
                let mut empty = true;
                s.push('{');
                for (key, property) in object.properties.iter() {
                    if !property.enumerable() {
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
                    let val = match &property.content {
                        Content::Data(heapref) => {
                            let value = heap.get(*heapref);
                            value.to_string(heap)
                        }
                        Content::NativeFunction(func) => {
                            format!("*{:x}", func_ptr(*func))
                        }
                        Content::Closure(closure) => {
                            format!("{:?}", closure)
                        }
                    };
                    s.push_str(&val);
                    s.push(',');
                    empty = false;
                }
                if !empty { s.pop(); s.push(' '); }
                s.push('}');
                s
            }
        }
    }

    /// to_object() tries to return the underlying object, if any.
    /// It's useful for checking if a value is an object.
    pub fn to_object(&self) -> Result<&JSObject, Exception> {
        if let JSValue::Object(object) = self {
            Ok(object)
        } else {
            /* TODO: wrap a primitive with an object */
            Err(Exception::ReferenceNotAnObject(Interpreted::Value(self.clone())))
        }
    }

    pub fn to_object_mut(&mut self) -> Result<&mut JSObject, Exception> {
        if let JSValue::Object(object) = self {
            Ok(object)
        } else {
            /* TODO: wrap a primitive with an object? */
            Err(Exception::ReferenceNotAnObject(Interpreted::Value(self.clone())))
        }
    }

    #[allow(dead_code)]
    pub fn to_json(&self, heap: &Heap) -> JSON {
        match self {
            JSValue::Undefined | JSValue::Null => JSON::Null,
            JSValue::Bool(b) => JSON::from(*b),
            JSValue::Number(n) => JSON::from(*n),
            JSValue::String(s) => JSON::from(s.as_str()),
            JSValue::Object(object) => {
                let mut json = json!({});
                for (key, property) in object.properties.iter() {
                    if !property.enumerable() {
                        continue
                    }

                    match property.content {
                        Content::Data(propref) => {
                            let value = heap.get(propref);
                            let jvalue = value.to_json(heap);
                            json[key] = jvalue;
                        }
                        Content::NativeFunction(_) => (),
                        Content::Closure(_) => (),
                    }
                }
                json
            }
        }
    }

    pub fn is_string(&self) -> bool {
        match self {
            JSValue::String(_) => true,
            _ => false,
        }
    }

    /// stringify() makes everything into a string
    /// used for evaluation in a string context.
    /// It corresponds to .toString() in JavaScript
    pub fn stringify(&self, heap: &Heap) -> String {
        match self {
            JSValue::String(s) => s.clone(),
            JSValue::Object(_obj) =>
                "[object Object]".to_string(),
            _ => self.to_string(heap),
        }
    }

    /// numberify() tries to make everything into a numeric value
    /// for evalation in a numeric context.
    /// It is slightly more strict than `+value` in JavaScript: only
    /// `value.numberify().unwrap_or(f64::NAN)` corresponds to `+value` in JavaScript.
    pub fn numberify(&self) -> Option<JSNumber> {
        match self {
            JSValue::Null => Some(0.0),
            JSValue::Bool(b) => Some(if *b { 1.0 } else { 0.0 }),
            JSValue::Number(n) => Some(*n),
            JSValue::String(s) => s.parse::<JSNumber>().ok(),
            // TODO: JSObject of Number/Boolean/String
            _ => None
        }
    }

    /// boolify() treats everythings as a truthy value.
    pub fn boolify(&self) -> bool {
        match self {
            JSValue::Undefined | JSValue::Null => false,
            JSValue::String(s) => s.len() > 0,
            _ =>
                if let Some(n) = self.numberify() {
                    !(n == 0.0 || f64::is_nan(n))
                } else {
                    true
                }
        }
    }

    pub fn type_of(&self) -> &'static str {
        match self {
            JSValue::Undefined => "undefined",
            JSValue::Null | JSValue::Object(_) => "object",
            JSValue::String(_) => "string",
            JSValue::Number(_) => "number",
            JSValue::Bool(_) => "boolean",
        }
    }

    /// Abstract Equality Comparison:
    /// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Equality_comparisons_and_sameness#Loose_equality_using_
    pub fn loose_eq(&self, other: &JSValue) -> bool {
        let lval = if self == &JSValue::Undefined { &JSValue::Null } else { self };
        let rval = if other == &JSValue::Undefined { &JSValue::Null } else { other };
        match (&lval, &rval) {
            (JSValue::Null, JSValue::Null) => true,
            (JSValue::Null, _) | (_, JSValue::Null) => false,
            (JSValue::Number(_), JSValue::Number(_))
                | (JSValue::String(_), JSValue::String(_))
                | (JSValue::Bool(_), JSValue::Bool(_))
                | (JSValue::Object(_), JSValue::Object(_)) =>
                (self == other),
            _ => {
                if let Some(lnum) = self.numberify() {
                    if let Some(rnum) = self.numberify() {
                        return lnum == rnum;
                    }
                }
                return false;
            }
        }
    }

    pub fn numerically<F>(&self, other: &JSValue, op: F) -> JSValue
        where F: Fn(f64, f64) -> f64
    {
        let val = match (self.numberify(), other.numberify()) {
            (Some(lnum), Some(rnum)) => op(lnum, rnum),
            _ => f64::NAN
        };
        JSValue::Number(val)
    }

    pub fn plus(&self, other: &JSValue, heap: &Heap) -> JSValue {
        if !(self.is_string() || other.is_string()) {
            if let Some(lnum) = self.numberify() {
                if let Some(rnum) = other.numberify() {
                    return JSValue::from(lnum + rnum);
                }
            }
        }
        let lvalstr = self.stringify(heap);
        let rvalstr = other.stringify(heap);
        JSValue::from(lvalstr + &rvalstr)
    }

    pub fn minus(&self, other: &JSValue, _heap: &Heap) -> JSValue {
        JSValue::numerically(self, other, |a, b| a - b)
    }


    pub fn less(&self, other: &JSValue, _heap: &Heap) -> JSValue {
        // TODO: Abstract Relational Comparison
        // TODO: toPrimitive()
        if let JSValue::String(lstr) = self {
            if let JSValue::String(rstr) = other {
                return JSValue::from(lstr < rstr);
            }
        };
        let lnum = self.numberify().unwrap_or(f64::NAN);
        let rnum = other.numberify().unwrap_or(f64::NAN);
        JSValue::from(lnum < rnum)
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

impl TryFrom<&JSON> for JSValue {
    type Error = ();

    /// Constructs a pure value (without references), if possible.
    /// Excludes objects and arrays.
    fn try_from(json: &JSON) -> Result<JSValue, Self::Error> {
        let value = if json.is_null() {
            JSValue::Null
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
    assert_eq!( JSValue::from("5").numberify(),    Some(5.0) );
}

#[test]
fn test_boolify() {
    // true
    assert!( JSValue::from(true).boolify() );
    assert!( JSValue::from(1).boolify() );
    assert!( JSValue::from("0").boolify() );
    //assert!( JSValue::from(json!([])).boolify() );

    // false
    assert!( !JSValue::from(false).boolify() );
    assert!( !JSValue::from(0).boolify() );
    assert!( !JSValue::from(f64::NAN).boolify() );
    assert!( !JSValue::from("").boolify() );
    assert!( !JSValue::Null.boolify() );
}

fn is_valid_identifier(s: &str) -> bool {
    let is_start = |c: char| (c.is_alphabetic() || c == '_' || c == '$');

    let mut it = s.chars();
    if let Some(c) = it.next() {
        is_start(c) && it.all(|c| is_start(c) || c.is_numeric())
    } else {
        false
    }
}

/// Runtime heap
pub struct Heap(Vec<JSValue>);

impl Heap {
    pub const NULL: JSRef = JSRef(0);
    pub const GLOBAL: JSRef = JSRef(1);

    pub fn new() -> Self {
        let heap_vec = vec![
            JSValue::Null,                     /* [Heap::NULL] */
            JSValue::Object(JSObject::new()),  /* [Heap::GLOBAL] */
        ];

        let mut heap = Heap(heap_vec);
        builtin::init(&mut heap)
            .expect("failed to initialize builtin objects");
        heap
    }

    pub fn object(&self, objref: JSRef) -> Result<&JSObject, Exception> {
        self.get(objref).to_object()
    }

    pub fn object_mut(&mut self, objref: JSRef) -> Result<&mut JSObject, Exception> {
        self.get_mut(objref).to_object_mut()
    }

    pub fn global(&self) -> &JSObject {
        self.object(Heap::GLOBAL).unwrap()
    }

    pub fn global_mut(&mut self) -> &mut JSObject {
        self.object_mut(Heap::GLOBAL).unwrap()
    }

    pub fn allocate(&mut self, value: JSValue) -> JSRef {
        let ind = self.0.len();
        self.0.push(value);
        JSRef(ind)
    }

    pub fn get(&self, jsref: JSRef) -> &JSValue {
        self.0.get(jsref.0).unwrap()
    }

    pub fn get_mut(&mut self, jsref: JSRef) -> &mut JSValue {
        self.0.get_mut(jsref.0).unwrap()
    }

    pub fn property_or_create(&mut self, objref: JSRef, name: &str) -> Result<JSRef, Exception> {
        let object = self.get(objref).to_object()?;
        match object.property_ref(name) {
            Some(propref) => Ok(propref),
            None => {
                let propref = self.allocate(JSValue::Undefined);
                let object = self.get_mut(objref).to_object_mut()?;
                object.set_property_ref(name, propref);
                Ok(propref)
            }
        }
    }

    pub fn property_assign(
        &mut self,
        objref: JSRef,
        name: &str,
        what: &Interpreted
    ) -> Result<(), Exception> {
        if let Ok(valref) = what.to_ref(self) {
            if let JSValue::Object{..} = self.get(valref) {
                let object = self.get_mut(objref).to_object_mut()?;
                object.set_property_ref(&name, valref);
                return Ok(());
            }
        }
        let object = self.get(objref).to_object()?;
        if object.properties.get(name).map(|prop| prop.access.writable()).unwrap_or(true) {
            let propref = self.property_or_create(objref, &name)?;
            let value = what.to_value(self)?;
            *self.get_mut(propref) = value.clone();
        }
        Ok(())
    }

    pub fn lookup_ref(&self, property_chain: &[&str]) -> Result<JSRef, Exception> {
        let mut objref = Heap::GLOBAL;
        for propname in property_chain {
            let object = self.get(objref).to_object()?;
            objref = object.property_ref(propname).ok_or_else(||
                Exception::TypeErrorGetProperty(Interpreted::Ref(objref), propname.to_string())
            )?;
        }
        Ok(objref)
    }

    pub fn object_from_json(&mut self, json: &JSON) -> JSValue {
        if let Some(obj) = json.as_object() {
            let mut object = JSObject::new();
            for (key, jval) in obj.iter() {
                let value = self.object_from_json(jval);
                let propref = self.allocate(value);
                object.set_property_ref(key, propref);
            }
            JSValue::Object(object)
        //} else if let Some(a) = json.as_array() {
        } else {
            JSValue::try_from(json).expect("primitive JSON")
        }
    }
}

/// A heap reference: a Heap index.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct JSRef(usize);

#[derive(Debug, Clone, PartialEq)]
pub enum Interpreted {
    /// An object member; might not exist yet.
    Member{ of: Box<Interpreted>, name: String },

    /// A heap reference
    Ref(JSRef),

    /// An off-heap value
    Value(JSValue),
}

impl Interpreted {
    pub const VOID: Interpreted = Interpreted::Value(JSValue::Undefined);
    pub const NAN: Interpreted = Interpreted::Value(JSValue::Number(f64::NAN));

    pub fn to_value(&self, heap: &Heap) -> Result<JSValue, Exception> {
        let value = match self {
            Interpreted::Ref(href) =>
                heap.get(*href).clone(),
            Interpreted::Value(value) =>
                value.clone(),
            Interpreted::Member{of, name} => {
                let objref = of.to_ref(heap)?;
                let object = heap.get(objref).to_object()?;
                match object.property_ref(&name) {
                    Some(propref) => heap.get(propref).clone(),
                    None => JSValue::Undefined,
                }
            }
        };
        Ok(value)
    }

    pub fn to_ref(&self, heap: &Heap) -> Result<JSRef, Exception> {
        match self {
            Interpreted::Ref(href) => Ok(*href),
            Interpreted::Member{of, name} => {
                let objref = of.to_ref(heap)?;
                let object = heap.get(objref).to_object()?;
                object.property_ref(name).ok_or_else(||
                    Exception::TypeErrorGetProperty(self.clone(), name.to_string())
                )
            }
            _ => Err(Exception::ReferenceNotAnObject(self.clone()))
        }
    }

    pub fn to_ref_or_allocate(&self, heap: &mut Heap) -> Result<JSRef, Exception> {
        match self {
            Interpreted::Ref(href) => Ok(*href),
            Interpreted::Value(value) => Ok(heap.allocate(value.clone())),
            Interpreted::Member{of, name} => {
                let objref = of.to_ref(heap)?;
                heap.property_or_create(objref, &name)
            }
        }
    }

    /// Corresponds to Javascript `delete` operator and all its weirdness.
    /// Ok/Err correspond to `true`/`false` from `delete`.
    /// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/delete
    pub fn delete(&self, heap: &mut Heap) -> Result<(), Exception> {
        match self {
            Interpreted::Member{ of, name } => {
                let objref = of.to_ref(heap)?;
                let object = heap.get_mut(objref).to_object_mut()?;
                // TODO: do not remove non-configurable properties
                // TODO: do not remove global/functions variables
                object.properties.remove(name);
                Ok(())
            }
            _ => Ok(()),
        }
    }
}

impl<T> From<T> for Interpreted where JSValue: From<T>  {
    fn from(value: T) -> Interpreted { Interpreted::Value(JSValue::from(value)) }
}

/// Javascript objects
#[derive(Debug, Clone, PartialEq)]
pub struct JSObject {
    pub properties: HashMap<String, Property>,
}

impl JSObject {
    /// A property with this name is used:
    /// - as the primitive value of a Number/Boolean/String object;
    /// - as the function entry in a Function.
    pub const VALUE: &'static str = "[[value]]";
    pub const PROTO: &'static str = "__proto__";

    pub fn new() -> JSObject {
        let properties = HashMap::new();
        JSObject{ properties }
    }

    pub fn property_ref(&self, name: &str) -> Option<JSRef> {
        self.properties.get(name).and_then(|prop|
            match prop.content {
                Content::Data(href) => Some(href),
                Content::NativeFunction(_) => None,
                Content::Closure(_) => None,
            }
        )
    }

    fn set_property_and_flags(&mut self, name: &str, content: Content, access: Access) {
        match self.properties.get_mut(name) {
            Some(prop) =>
                if prop.access.writable() {
                    prop.content = content;
                },
            None => {
                let prop = Property{ content, access };
                self.properties.insert( name.to_string(), prop);
            }
        }
    }

    pub fn set_property_ref(&mut self, name: &str, propref: JSRef) {
        self.set_property(name, Content::Data(propref))
    }
    pub fn set_property(&mut self, name: &str, content: Content) {
        self.set_property_and_flags(name, content, Access::ALL)
    }

    pub fn set_system(&mut self, name: &str, content: Content) {
        self.set_property_and_flags(name, content, Access::NONE)
    }

    pub fn set_hidden(&mut self, name: &str, content: Content) {
        self.set_property_and_flags(name, content, Access::ALL ^ Access::ENUM)
    }

    pub fn set_nonconf(&mut self, name: &str, content: Content) {
        self.set_property_and_flags(name, content, Access::ALL ^ Access::CONF)
    }

    pub fn set_readonly(&mut self, name: &str, content: Content) {
        self.set_property_and_flags(name, content, Access::ALL ^ Access::WRITE)
    }
}


#[derive(Debug, Clone, PartialEq)]
pub struct Property {
    pub content: Content,
    pub access: Access,
}

impl Property {
    pub fn from_ref(heapref: JSRef) -> Property {
        Property {
            content: Content::Data(heapref),
            access: Access::ALL,
        }
    }

    pub fn enumerable(&self) -> bool { self.access.enumerable() }
    pub fn writable(&self) -> bool { self.access.writable() }
}


bitflags! {
    pub struct Access: u8 {
        const ENUM = 0b001;
        const CONF = 0b010;
        const WRITE = 0b100;

        const NONE = 0b000;
        const ALL = 0b111;
    }
}

impl Access {
    pub fn enumerable(&self) -> bool { self.contains(Access::ENUM) }
    pub fn configurable(&self) -> bool { self.contains(Access::CONF) }
    pub fn writable(&self) -> bool { self.contains(Access::WRITE) }
}


#[derive(Clone)]
pub enum Content {
    Data(JSRef),
    NativeFunction(NativeFunction),
    Closure(Closure),
    /*
    Accesssor{
        get: Option<Callable>,
        set: Option<Callable>,
    },
    */
}

impl PartialEq for Content {
    fn eq(&self, other: &Content) -> bool {
        match (self, other) {
            (Content::Data(dit), Content::Data(dat)) =>
                dit == dat,
            (Content::NativeFunction(dit), Content::NativeFunction(dat)) =>
                func_ptr(*dit) == func_ptr(*dat),
            _ => false
        }
    }
}

impl fmt::Debug for Content {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Content::Data(heapref) => {
                write!(f, "Content::Data({:?})", heapref)
            }
            Content::NativeFunction(func) => {
                write!(f, "Content::NativeFunction(*{:x})", func_ptr(*func))
            }
            Content::Closure(closure) => {
                let name = closure.id.as_ref()
                    .map(|id| id.0.as_str())
                    .unwrap_or("<anonymous>");
                let param_names = closure.params.iter()
                    .map(|id| id.0.clone())
                    .collect::<Vec<String>>();
                write!(f, "Content::Closure( {}(", name)?;
                write!(f, "{}", param_names.join(", "))?;
                write!(f, "))")
            }
        }
    }
}

pub type NativeFunction = fn(
    this_ref: JSRef,
    method_name: String,
    arguments: Vec<Interpreted>,
    heap: &mut Heap,
) -> Result<Interpreted, Exception>;

fn func_ptr(func: NativeFunction) -> usize {
    func as *const () as usize
}


#[derive(Clone, Debug)]
pub struct Closure {
    pub id: Option<ast::Identifier>,
    pub params: Vec<ast::Identifier>,
    pub body: Box<ast::BlockStatement>,
    //pub free_variables: Vec<JSRef>,
}
