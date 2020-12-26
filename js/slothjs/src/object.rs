use std::collections::HashMap;
use std::convert::TryFrom;

use serde_json::json;

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
    /// to_string() makes a human-readable string representation of the value
    /// ```
    /// JSValue::from("1").to_string()    // "\"1\""
    /// JSValue::from(1).to_string()      // "1"
    /// JSValue::from(json!([1, 2])).to_string()   // "[1,2]"
    /// JSValue::from(json!({"one": 1, "two": 2})).to_string()  // "{ one: 1, two: 2}"
    /// ```
    pub fn to_string(&self, heap: &Heap) -> String {
        match self {
            JSValue::Undefined => "undefined".to_string(),
            JSValue::Null => "null".to_string(),
            JSValue::Bool(b) => b.to_string(),
            JSValue::Number(n) => n.to_string(),
            JSValue::String(s) => s.clone(),
            JSValue::Object(object) => {
                let mut s = String::new();
                let mut empty = true;
                s.push('{');
                for (key, property) in object.properties.iter() {
                    if !property.enumerable {
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
                    let val = match property.descriptor {
                        Descriptor::Data(heapref) => {
                            let value = heap.get(heapref);
                            value.to_string(heap)
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

    pub fn to_json(&self, heap: &Heap) -> JSON {
        match self {
            JSValue::Undefined | JSValue::Null => JSON::Null,
            JSValue::Bool(b) => JSON::from(*b),
            JSValue::Number(n) => JSON::from(*n),
            JSValue::String(s) => JSON::from(s.as_str()),
            JSValue::Object(object) => {
                let mut json = json!({});
                for (key, property) in object.properties.iter() {
                    if !property.enumerable {
                        continue
                    }

                    match property.descriptor {
                        Descriptor::Data(propref) => {
                            let value = heap.get(propref);
                            let jvalue = value.to_json(heap);
                            json[key] = jvalue;
                        }
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
        /*
        if let Some(a) = self.0.as_array() {
            return a.iter()
                    .map(|v| JSValue(v.clone()).to_string())
                    .collect::<Vec<String>>()
                    .join(",");
        }
        */
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
    //assert!( JSValue::from(json!({})).boolify() );
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
    pub const UNDEFINED: JSRef = JSRef(0);

    pub fn new() -> Self {
        let heap = vec![
            JSValue::Undefined,
        ];
        Heap(heap)
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

    pub fn property_or_create(&mut self, objref: JSRef, name: &str) -> Option<JSRef> {
        match self.get(objref) {
            JSValue::Object(object) => {
                if let Some(property) = object.properties.get(name) {
                    match property.descriptor {
                        Descriptor::Data(propref) => Some(propref),
                    }
                } else {
                    let propref = self.allocate(JSValue::Undefined);
                    if let JSValue::Object(object) = self.get_mut(objref) {
                        object.set_property(name, propref);
                        Some(propref)
                    } else {
                        unreachable!()
                    }
                }
            }
            _ => None,
        }
    }

    pub fn get_property(&self, object: &JSObject, name: &str) -> Option<&JSValue> {
        object.property_ref(name).map(|propref| self.get(propref))
    }

    pub fn place(&self, place: Place) -> &JSValue {
        let href = match place {
            Place::Ref(heapref) => heapref,
            Place::ObjectMember{..} => Heap::UNDEFINED,
        };
        self.get(href)
    }

    fn reify_place(&mut self, place: &Place) -> Place {
        match place {
            Place::ObjectMember{objref, member} => Place::Ref(
                self.property_or_create(*objref, member)
                    .expect("failed to create a property")
            ),
            _ => place.clone(),
        }
    }

    pub fn place_mut(&mut self, place: &mut Place) -> &mut JSValue {
        *place = self.reify_place(place);
        match place {
            Place::Ref(heapref) => self.get_mut(heapref.clone()),
            _ => panic!("{:?} is not reified", place)
        }
    }

    pub fn new_object(&mut self) -> JSRef {
        let object = JSObject::new();
        self.allocate(JSValue::Object(object))
    }

    pub fn object_from_json(&mut self, json: &JSON) -> JSValue {
        if let Some(obj) = json.as_object() {
            let mut object = JSObject::new();
            for (key, jval) in obj.iter() {
                let value = self.object_from_json(jval);
                let propref = self.allocate(value);
                object.set_property(key, propref);
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

#[derive(Debug, Clone)]
pub enum Place {
    /// A heap reference that already exists
    Ref(JSRef),

    /// A potential object member that does not exist yet.
    ObjectMember{ objref: JSRef, member: String }
}

/// Javascript objects
#[derive(Debug, Clone, PartialEq)]
pub struct JSObject {
    pub properties: HashMap<String, Property>,
}

impl JSObject {
    pub fn new() -> JSObject {
        let properties = HashMap::new();
        JSObject{ properties }
    }

    pub fn set_property(&mut self, name: &str, propref: JSRef) {
        self.properties.insert(name.to_string(), Property::from(propref));
    }

    pub fn property_ref(&self, name: &str) -> Option<JSRef> {
        self.properties.get(name).map(|prop|
            match prop.descriptor {
                Descriptor::Data(href) => href
            }
        )
    }
}


#[derive(Debug, Clone, PartialEq)]
pub struct Property {
    pub enumerable: bool,
    pub writable: bool,
    pub configurable: bool,

    pub descriptor: Descriptor,
}

impl Property {
    pub fn from(heapref: JSRef) -> Property {
        Property {
            enumerable: true,
            writable: true,
            configurable: true,
            descriptor: Descriptor::Data(heapref),
        }
    }

    pub fn readonly(&mut self) -> &mut Self {
        self.writable = false;
        self
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Descriptor {
    Data(JSRef),
    /*
    pub Accesssor{
        get: Option<Callable>,
        set: Option<Callable>,
    },
    */
}
