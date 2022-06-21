use crate::prelude::*;
use crate::{
    object::ObjectValue,
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
        .to_value(heap)?;
    let s = arg.stringify(heap)?;

    if !heap.smells_fresh(call.this_ref) {
        // take the argument and produce a string from it
        return Ok(Interpreted::from(s));
    }

    *heap.get_mut(call.this_ref) = JSObject::from(s);
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
            .unwrap_or_else(|| JSValue::from(href))
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
        Some(len) if len <= 0 => begin as i64,
        Some(len) if begin as i64 + len < strlen => begin as i64 + len,
        _ => strlen,
    } as usize;

    let substr = &s[begin..end];
    Ok(Interpreted::from(substr))
}

#[allow(non_snake_case)]
fn string_proto_indexOf(call: CallContext, heap: &mut Heap) -> Result<Interpreted, Exception> {
    let NOT_FOUND = Interpreted::from(-1);

    let heystack = heap.ref_to_string(call.this_ref)?;
    let strlen = heystack.chars().count() as i64; // COSTLY

    let needle = (call.arguments.get(0))
        .unwrap_or(&Interpreted::from("undefined"))
        .to_value(heap)?
        .stringify(heap)?;

    let char_start = match call.arg_as_index(1, heap)?.unwrap_or(0) {
        b if b < 0 => 0,
        b if b > strlen && needle.is_empty() => return Ok(Interpreted::from(strlen)),
        b if b > strlen => return Ok(NOT_FOUND),
        b => b,
    };
    // COSTLY
    let byte_start = match heystack.as_str().char_indices().nth(char_start as usize) {
        None => return Ok(NOT_FOUND),
        Some((offset, _)) => offset,
    };
    let heystack = &heystack[byte_start..];

    // COSTLY, but kudos to std::str for providing a fast and tested substring search
    let byte_index = match heystack.find(&needle) {
        None => return Ok(NOT_FOUND),
        Some(byte_index) => byte_index,
    };

    // COSTLY
    let char_index = (heystack.char_indices())
        .position(|(bi, _)| bi == byte_index)
        .unwrap(); // yes, there must be a position since we've `find`ed it.
    Ok(Interpreted::from(char_start + char_index as i64))
}

pub fn init(heap: &mut Heap) -> Result<JSRef, Exception> {
    let mut string_proto = JSObject::new();
    string_proto.set_hidden("valueOf", heap.alloc_func(string_proto_valueOf))?;
    string_proto.set_hidden("toString", heap.alloc_func(string_proto_valueOf))?;
    string_proto.set_hidden("charAt", heap.alloc_func(string_proto_charAt))?;
    string_proto.set_hidden("charCodeAt", heap.alloc_func(string_proto_charCodeAt))?;
    string_proto.set_hidden("slice", heap.alloc_func(string_proto_slice))?;
    string_proto.set_hidden("substr", heap.alloc_func(string_proto_substr))?;
    string_proto.set_hidden("indexOf", heap.alloc_func(string_proto_indexOf))?;

    *heap.get_mut(Heap::STRING_PROTO) = string_proto;

    let mut the_string = JSObject::from_func(string_constructor);
    the_string.set_system("prototype", Heap::STRING_PROTO)?;

    let the_string_ref = heap.alloc(the_string);
    heap.get_mut(Heap::STRING_PROTO)
        .set_hidden("constructor", the_string_ref)?;

    Ok(the_string_ref)
}
