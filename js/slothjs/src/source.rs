use serde::{
    Deserialize,
    Serialize,
};

use crate::error::ParseError;
use crate::object::Content;
use crate::{
    Exception,
    Heap,
    JSObject,
    JSValue,
    JSON,
};

#[derive(Clone, Copy, Debug, Deserialize, Serialize)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

#[derive(Clone, Copy, Debug, Deserialize, Serialize)]
pub struct Location {
    pub start: Position,
    pub end: Position,
}

#[derive(Clone, Debug)]
pub struct Document {
    pub name: String,
}

const STASH_LOCATION: &'static str = "[[current_location]]";
//const STASH_DOCUMENT: &'static str = "[[current_document]]";

const CALLER_LOCATION: &'static str = "[[caller_location]]";

pub fn stash_location(loc: &Box<Location>, heap: &mut Heap) {
    let scope = heap.scope_mut();
    if let Some(stash_value) = scope.get_value(STASH_LOCATION) {
        if let Ok(stash_ref) = stash_value.to_ref() {
            let stash = heap.get_mut(stash_ref);
            if let Some(array) = stash.as_array_mut() {
                array.storage.clear();
                array.storage.push(JSValue::from(loc.start.line as f64));
                array.storage.push(JSValue::from(loc.start.column as f64));
                array.storage.push(JSValue::from(loc.end.line as f64));
                array.storage.push(JSValue::from(loc.end.column as f64));
            }
        }
    } else {
        let stash_array = JSObject::from_array(vec![]);
        let stash_ref = heap.alloc(stash_array);
        heap.scope_mut()
            .set_system(STASH_LOCATION, Content::from(stash_ref))
            .unwrap();

        stash_location(loc, heap);
    }
}

pub fn node_location(heap: &mut Heap) -> Option<Location> {
    let scope = heap.scope();
    if let Some(stash_value) = scope.get_value(STASH_LOCATION) {
        if let Ok(stash_ref) = stash_value.to_ref() {
            let stash = heap.get(stash_ref);
            if let Some(array) = stash.as_array() {
                if array.storage.len() == 4 {
                    let line = array.storage[0].numberify(heap).unwrap() as usize;
                    let column = array.storage[1].numberify(heap).unwrap() as usize;
                    let start = Position { line, column };

                    let line = array.storage[2].numberify(heap).unwrap() as usize;
                    let column = array.storage[3].numberify(heap).unwrap() as usize;
                    let end = Position { line, column };

                    return Some(Location { start, end });
                }
            }
        }
    }
    None
}

pub fn save_caller(loc: &Location, heap: &mut Heap) -> Result<(), Exception> {
    let loc_arr = vec![
        JSValue::from(loc.start.line as f64),
        JSValue::from(loc.start.column as f64),
        JSValue::from(loc.end.line as f64),
        JSValue::from(loc.end.column as f64),
    ];
    let loc_object = JSObject::from_array(loc_arr);
    let loc_ref = heap.alloc(loc_object);
    heap.scope_mut()
        .set_system(CALLER_LOCATION, Content::from(loc_ref))?;
    Ok(())
}

pub fn print_callstack(heap: &mut Heap) -> Result<(), Exception> {
    let loc = node_location(heap).ok_or(Exception::SyntaxError(ParseError::ObjectWithout {
        attr: STASH_LOCATION,
        value: JSON::Null,
    }))?;
    eprintln!("{:?}", &loc);

    let mut scoperef = heap.local_scope().unwrap_or(Heap::NULL);
    while scoperef != Heap::NULL {
        let loc = heap
            .get(scoperef)
            .get_value(CALLER_LOCATION)
            .ok_or(Exception::SyntaxError(ParseError::ObjectWithout {
                attr: CALLER_LOCATION,
                value: JSON::Null,
            }))?;
        eprintln!("    {:?}", &loc);

        scoperef = match heap.get(scoperef).get_value(Heap::SAVED_SCOPE) {
            Some(v) => v.to_ref().unwrap_or(Heap::NULL),
            None => Heap::NULL,
        };
    }

    Ok(())
}
