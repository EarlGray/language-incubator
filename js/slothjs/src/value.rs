use serde_json::json;


/*
 *  JSValue: "type" "system"
 */

pub type JSON = serde_json::Value;

#[derive(Debug, Clone, PartialEq)]
pub struct JSValue(pub JSON);

pub const UNDEFINED: JSValue = JSValue(JSON::Null);


pub type JSNumber = f64;

/*
#[derive(Debug, Clone)]
enum JSValue {
    Null,
    Undefined,
    Number(JSNumber),
    Str(String),
}
*/

fn is_valid_identifier(s: &str) -> bool {
    let is_start = |c: char| (c.is_alphabetic() || c == '_' || c == '$');

    let mut it = s.chars();
    if let Some(c) = it.next() {
        is_start(c) && it.all(|c| is_start(c) || c.is_numeric())
    } else {
        false
    }
}

impl JSValue {
    /// to_string() makes a string representation of the value
    /// ```
    /// JSValue::from("1").to_string()    // "\"1\""
    /// JSValue::from(1).to_string()      // "1"
    /// JSValue::from(json!([1, 2])).to_string()   // "[1,2]"
    /// ```
    pub fn to_string(&self) -> String {
        match &self.0 {
            JSON::Null => "null".to_string(),
            JSON::Number(n) => n.to_string(),
            JSON::Bool(b) => b.to_string(),
            JSON::String(_s) => {
                let mut s = String::new();
                s.push_str(&self.0.to_string());
                s
            }
            JSON::Array(a) => {
                let body = a.iter()
                    .map(|v| JSValue(v.clone()).to_string())
                    .collect::<Vec<String>>()
                    .join(",");

                let mut s = String::new();
                s.push_str("[");
                s.push_str(&body);
                s.push_str("]");
                s
            }
            JSON::Object(obj) => {
                let mut s = String::new();
                let mut empty = true;
                s.push('{');
                for (key, jval) in obj.iter() {
                    s.push(' ');
                    if is_valid_identifier(&key) {
                        s.push_str(key);
                    } else {
                        let skey = JSValue(json!(key)).to_string();
                        s.push_str(&skey);
                    }
                    s.push_str(": ");
                    let val = JSValue(jval.clone()).to_string();
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

    /// stringify() corresponds to .toString() in JavaScript
    pub fn stringify(&self) -> String {
        if let Some(s) = self.0.as_str() {
            return s.to_string();
        }
        if let Some(a) = self.0.as_array() {
            return a.iter()
                    .map(|v| JSValue(v.clone()).to_string())
                    .collect::<Vec<String>>()
                    .join(",");
        }
        if self.0.is_object() {
            return "[object Object]".to_string();
        }
        return self.to_string();
    }

    pub fn numberify(&self) -> Option<JSNumber> {
        if self.0.is_null() {
            Some(0.0)
        } else if let Some(b) = self.0.as_bool() {
            Some(if b { 1.0 } else { 0.0 })
        } else if let Some(n) = self.0.as_f64() {
            Some(n)
        } else if let Some(s) = self.0.as_str() {
            s.parse::<JSNumber>().ok()
        } else {
            None
        }
    }

    /*
    fn from_json(json: &JSON) -> JSValue {
        match json {
        }
    }


    fn as_number(&self) -> Option<JSNumber> {
    }
    */
}

impl From<JSON> for JSValue {
    fn from(json: JSON) -> Self { JSValue(json) }
}

impl From<JSNumber> for JSValue {
    fn from(number: JSNumber) -> Self { JSValue(json!(number)) }
}

impl From<i64> for JSValue {
    fn from(number: i64) -> Self { JSValue(json!(number)) }
}

impl From<String> for JSValue {
    fn from(s: String) -> Self { JSValue(json!(s)) }
}

impl From<&str> for JSValue {
    fn from(s: &str) -> Self { JSValue(json!(s.to_string())) }
}

#[test]
fn test_numberify() {
}

#[test]
fn test_boolify() {
}

