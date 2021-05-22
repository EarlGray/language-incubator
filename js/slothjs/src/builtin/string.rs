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

impl CallContext {
    fn arg_as_index(&self, argnum: usize, heap: &Heap) -> Result<Option<i64>, Exception> {
        let arg = match self.arguments.get(argnum) {
            Some(arg) => arg.to_value(heap)?,
            None => return Ok(None),
        };
        Ok(Some(arg.numberify(heap).unwrap_or(0.0) as i64))
    }
}

impl Heap {
    fn ref_to_string(&mut self, href: JSRef) -> Result<String, Exception> {
        self.get(href)
            .to_primitive(self)
            .unwrap_or(JSValue::from(href))
            .stringify(self)
    }
}

#[allow(non_snake_case)]
fn string_proto_valueOf(call: CallContext, heap: &mut Heap) -> Result<Interpreted, Exception> {
    let s = object_to_str(call.this_ref, heap)?;
    Ok(Interpreted::from(s))
}

#[allow(non_snake_case)]
fn string_proto_charAt(call: CallContext, heap: &mut Heap) -> Result<Interpreted, Exception> {
    let index = call.arg_as_index(0, heap)?.unwrap_or(0);
    let s = heap.ref_to_string(call.this_ref)?;
    let result = match s.chars().nth(index as usize) {
        Some(c) => c.to_string(),
        None => "".to_string(),
    };
    Ok(Interpreted::from(result))
}

#[allow(non_snake_case)]
fn string_proto_charCodeAt(call: CallContext, heap: &mut Heap) -> Result<Interpreted, Exception> {
    let index = call.arg_as_index(0, heap)?.unwrap_or(0);
    let s = heap.ref_to_string(call.this_ref)?;
    let result = match s.chars().nth(index as usize) {
        Some(c) => c as i64 as f64,
        None => f64::NAN,
    };
    Ok(Interpreted::from(result))
}

fn string_proto_slice(call: CallContext, heap: &mut Heap) -> Result<Interpreted, Exception> {
    let s = heap.ref_to_string(call.this_ref)?;
    let strlen = s.chars().count() as i64;

    let begin = match call.arg_as_index(0, heap)?.unwrap_or(0) {
        b if b > strlen => return Ok(Interpreted::from("")),
        b if b <= -strlen => 0,
        b if b < 0 => b + strlen,
        b => b,
    } as usize;
    let end = match call.arg_as_index(1, heap)?.unwrap_or(strlen) {
        e if e <= -strlen => return Ok(Interpreted::from("")),
        e if e < 0 => e + strlen,
        e if e > strlen => strlen,
        e => e,
    } as usize;
    if end < begin {
        return Ok(Interpreted::from(""));
    }

    let substr = &s[begin..end];
    Ok(Interpreted::from(substr))
}

fn string_proto_substr(call: CallContext, heap: &mut Heap) -> Result<Interpreted, Exception> {
    let s = heap.ref_to_string(call.this_ref)?;
    let strlen = s.chars().count() as i64;
    let begin = match call.arg_as_index(0, heap)?.unwrap_or(0) {
        b if b > strlen => return Ok(Interpreted::from("")),
        b if b < -strlen => 0,
        b if b < 0 => b + strlen,
        b => b,
    } as usize;
    let end = match call.arg_as_index(1, heap)? {
        Some(e) if e <= 0 => begin as i64,
        Some(e) if e < strlen => begin as i64 + e,
        _ => strlen,
    } as usize;

    let substr = &s[begin..end];
    Ok(Interpreted::from(substr))
}

pub fn init(heap: &mut Heap) -> Result<JSRef, Exception> {
    let mut string_proto = JSObject::new();
    string_proto.set_hidden("valueOf", Content::from_func(string_proto_valueOf, heap))?;
    string_proto.set_hidden("toString", Content::from_func(string_proto_valueOf, heap))?;
    string_proto.set_hidden("charAt", Content::from_func(string_proto_charAt, heap))?;
    string_proto.set_hidden(
        "charCodeAt",
        Content::from_func(string_proto_charCodeAt, heap),
    )?;
    string_proto.set_hidden("slice", Content::from_func(string_proto_slice, heap))?;
    string_proto.set_hidden("substr", Content::from_func(string_proto_substr, heap))?;

    *heap.get_mut(Heap::STRING_PROTO) = string_proto;

    let mut the_string = JSObject::from_func(string_constructor);
    the_string.set_system("prototype", Content::from(Heap::STRING_PROTO))?;

    let the_string_ref = heap.alloc(the_string);
    heap.get_mut(Heap::STRING_PROTO)
        .set_hidden("constructor", Content::from(the_string_ref))?;

    Ok(the_string_ref)
}
