//use crate::prelude::*;
use crate::{
    CallContext,
    Exception,
    Heap,
    Interpreted,
    JSObject,
    JSRef,
    JSResult,
    JSValue,
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

pub fn init(heap: &mut Heap) -> JSResult<JSRef> {
    /* Boolean.prototype */
    let mut boolean_proto = JSObject::new();
    boolean_proto.set_hidden("valueOf", heap.alloc_func(boolean_proto_valueOf))?;
    boolean_proto.set_hidden("toString", heap.alloc_func(boolean_proto_toString))?;

    *heap.get_mut(Heap::BOOLEAN_PROTO) = boolean_proto;

    /* the Boolean object */
    let mut the_boolean = JSObject::from_func(boolean_constructor);
    the_boolean.set_system("prototype", Heap::BOOLEAN_PROTO)?;

    let the_boolean_ref = heap.alloc(the_boolean);

    heap.get_mut(Heap::BOOLEAN_PROTO)
        .set_hidden("constructor", the_boolean_ref)?;

    Ok(the_boolean_ref)
}
