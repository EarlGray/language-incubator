use serde::{
    Deserialize,
    Serialize,
};

use crate::error::ParseError;
use crate::object::Content;
use crate::{
    Exception,
    Heap,
    Interpreted,
    JSObject,
    JSValue,
    JSON,
};

const CALLER_LOCATION: &'static str = "[[caller_location]]";

#[derive(Clone, Copy, Debug, Deserialize, Serialize)]
pub struct Position {
    line: usize,
    column: usize,
}

#[derive(Clone, Copy, Debug, Deserialize, Serialize)]
pub struct Location {
    start: Position,
    end: Position,
}

impl Location {
    fn from_saved(object: &JSObject, heap: &Heap) -> Result<Location, Exception> {
        if let Some(array) = object.as_array() {
            let line = array.storage[0].numberify(heap).unwrap() as usize;
            let column = array.storage[1].numberify(heap).unwrap() as usize;
            let start = Position { line, column };

            let line = array.storage[2].numberify(heap).unwrap() as usize;
            let column = array.storage[3].numberify(heap).unwrap() as usize;
            let end = Position { line, column };

            Ok(Location { start, end })
        } else {
            Err(Exception::TypeErrorNotArraylike(Interpreted::VOID))
        }
    }
}

#[derive(Clone, Debug)]
pub struct Document {
    name: String,
}

pub fn save_caller(caller: Option<Box<Location>>, heap: &mut Heap) -> Result<(), Exception> {
    if let Some(loc) = caller {
        let array = vec![
            JSValue::from(loc.start.line as f64),
            JSValue::from(loc.start.column as f64),
            JSValue::from(loc.end.line as f64),
            JSValue::from(loc.end.column as f64),
        ];
        let loc_ref = heap.alloc(JSObject::from_array(array));
        heap.scope_mut()
            .set_system(CALLER_LOCATION, Content::from(loc_ref))?;
    }
    Ok(())
}

pub fn print_callstack(heap: &Heap) -> Result<(), Exception> {
    let loc = heap.loc.clone();
    eprintln!("{:?}", loc);

    let mut scoperef = heap.local_scope().unwrap_or(Heap::NULL);
    while scoperef != Heap::NULL {
        let loc_ref = (heap.get(scoperef))
            .get_value(CALLER_LOCATION)
            .and_then(|v| v.to_ref().ok())
            .ok_or(Exception::SyntaxError(ParseError::ObjectWithout {
                attr: CALLER_LOCATION,
                value: JSON::Null,
            }))?;
        if let Ok(loc) = Location::from_saved(heap.get(loc_ref), heap) {
            eprintln!("   {:?}", loc);
        } else {
            eprintln!("   ???");
        }

        scoperef = match heap.get(scoperef).get_value(Heap::SAVED_SCOPE) {
            Some(v) => v.to_ref().unwrap_or(Heap::NULL),
            None => Heap::NULL,
        };
    }

    Ok(())
}
