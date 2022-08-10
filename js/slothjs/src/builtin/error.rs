use crate::{
    function::CallContext,
    object::HostClass,
    prelude::*,
    Heap,
    Interpreted,
    JSObject,
    JSResult,
};

pub static CLASS: HostClass = HostClass {
    name: "Error",
    constructor: error_constructor,
    methods: &[("toString", error_proto_toString)],
    static_methods: &[],
};

pub fn error_constructor(call: CallContext, heap: &mut Heap) -> JSResult<Interpreted> {
    let message = (call.arguments.get(0))
        .unwrap_or(&Interpreted::from(""))
        .to_value(heap)?
        .stringify(heap)?;

    let mut error_object = JSObject::new();
    error_object.proto = Heap::ERROR_PROTO;

    error_object.set_hidden("message", message)?;

    let objref = heap.alloc(error_object);
    Ok(Interpreted::from(objref))
}

#[allow(non_snake_case)]
fn error_proto_toString(call: CallContext, heap: &'_ mut Heap) -> JSResult<Interpreted> {
    call.this_ref.expect_instance("Error", heap)?;

    let name = (heap.get(call.this_ref))
        .lookup_value("name", heap)
        .unwrap_or_else(|| JSValue::from(""))
        .stringify(heap)?;

    let message = (heap.get(call.this_ref))
        .lookup_value("message", heap)
        .unwrap_or_else(|| JSValue::from(""))
        .stringify(heap)?;

    Ok(Interpreted::from(match () {
        _ if message.is_empty() => name,
        _ if name.is_empty() => message,
        _ => JSString::from(name.to_string() + ": " + &message),
    }))
}
