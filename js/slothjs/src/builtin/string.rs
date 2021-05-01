use crate::object::{
    Content,
    ObjectValue,
};
use crate::{
    CallContext,
    Exception,
    Heap,
    Interpreted,
    JSObject,
    JSRef,
    JSValue,
};

fn string_constructor(call: CallContext, heap: &mut Heap) -> Result<Interpreted, Exception> {
    let arg = (call.arguments.get(0))
        .unwrap_or(&Interpreted::from(""))
        .to_value(&heap)?;
    let s = arg.stringify(heap)?;

    if !heap.smells_fresh(call.this_ref) {
        // take the argument and produce a string from it
        return Ok(Interpreted::from(s));
    }

    *heap.get_mut(call.this_ref) = JSObject::from_str(s);
    Ok(Interpreted::VOID)
}

fn object_to_str(this_ref: JSRef, heap: &Heap) -> Result<&str, Exception> {
    match &heap.get(this_ref).value {
        ObjectValue::String(s) => Ok(s.as_str()),
        _ => {
            let what = Interpreted::from(this_ref);
            let of = "String".to_string();
            Err(Exception::TypeErrorInstanceRequired(what, of))
        }
    }
}

#[allow(non_snake_case)]
fn string_proto_valueOf(call: CallContext, heap: &mut Heap) -> Result<Interpreted, Exception> {
    let s = object_to_str(call.this_ref, heap)?;
    Ok(Interpreted::from(s))
}

#[allow(non_snake_case)]
fn string_proto_charCodeAt(call: CallContext, heap: &mut Heap) -> Result<Interpreted, Exception> {
    let arg = (call.arguments.get(0))
        .unwrap_or(&Interpreted::from(0))
        .to_value(heap)?;
    let index = arg.numberify(heap).unwrap_or(0.0) as i64;

    let s = (heap.get(call.this_ref))
        .to_primitive(heap)
        .unwrap_or(JSValue::from(call.this_ref))
        .stringify(heap)?;
    let result = match s.chars().nth(index as usize) {
        Some(c) => c as i64 as f64,
        None => f64::NAN,
    };
    Ok(Interpreted::from(result))
}

pub fn init(heap: &mut Heap) -> Result<JSRef, Exception> {
    let mut string_proto = JSObject::new();
    string_proto.set_hidden("valueOf", Content::from_func(string_proto_valueOf, heap))?;
    string_proto.set_hidden("toString", Content::from_func(string_proto_valueOf, heap))?;
    string_proto.set_hidden(
        "charCodeAt",
        Content::from_func(string_proto_charCodeAt, heap),
    )?;

    *heap.get_mut(Heap::STRING_PROTO) = string_proto;

    let mut the_string = JSObject::from_func(string_constructor);
    the_string.set_system("prototype", Content::from(Heap::STRING_PROTO))?;

    let the_string_ref = heap.alloc(the_string);
    heap.get_mut(Heap::STRING_PROTO)
        .set_hidden("constructor", Content::from(the_string_ref))?;

    Ok(the_string_ref)
}
