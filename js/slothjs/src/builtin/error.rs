use crate::{
    function::CallContext,
    Heap,
    Interpreted,
    JSObject,
    JSRef,
    JSResult,
    JSValue,
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
        _ => name + ": " + &message,
    }))
}

pub fn init(heap: &mut Heap) -> JSResult<JSRef> {
    let mut error_proto = JSObject::new();
    error_proto.set_hidden("name", "Error")?;
    error_proto.set_hidden("message", "")?;
    error_proto.set_hidden("toString", heap.alloc_func(error_proto_toString))?;

    *heap.get_mut(Heap::ERROR_PROTO) = error_proto;

    let mut the_error = JSObject::from_func(error_constructor);
    the_error.set_system("prototype", Heap::ERROR_PROTO)?;

    let the_error_ref = heap.alloc(the_error);
    heap.get_mut(Heap::ERROR_PROTO)
        .set_hidden("constructor", the_error_ref)?;

    Ok(the_error_ref)
}
