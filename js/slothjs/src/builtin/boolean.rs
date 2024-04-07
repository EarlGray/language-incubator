//use crate::prelude::*;
use crate::{
    object::HostClass, CallContext, Exception, Heap, Interpreted, JSObject, JSRef, JSResult,
    JSValue,
};

pub static CLASS: HostClass = HostClass {
    name: "Boolean",
    constructor: boolean_constructor,
    methods: &[
        ("toString", boolean_proto_toString),
        ("valueOf", boolean_proto_valueOf),
    ],
    static_methods: &[],
};

fn boolean_constructor(call: CallContext, heap: &mut Heap) -> JSResult<Interpreted> {
    let arg = call.arg_value(0, heap)?.boolify(heap);

    if !heap.smells_fresh(call.this_ref) {
        return Ok(Interpreted::from(arg));
    }

    *heap.get_mut(call.this_ref) = JSObject::from_bool(arg);
    Ok(Interpreted::VOID)
}

fn object_to_bool(this_ref: JSRef, heap: &Heap) -> JSResult<bool> {
    match heap.get(this_ref).to_primitive() {
        Some(JSValue::Bool(b)) => Ok(b),
        _ => Err(Exception::instance_required(this_ref, "Boolean")),
    }
}

#[allow(non_snake_case)]
fn boolean_proto_toString(call: CallContext, heap: &mut Heap) -> JSResult<Interpreted> {
    let b = object_to_bool(call.this_ref, heap)?;
    Ok(Interpreted::from(if b { "true" } else { "false" }))
}

#[allow(non_snake_case)]
fn boolean_proto_valueOf(call: CallContext, heap: &mut Heap) -> JSResult<Interpreted> {
    let b = object_to_bool(call.this_ref, heap)?;
    Ok(Interpreted::from(b))
}
