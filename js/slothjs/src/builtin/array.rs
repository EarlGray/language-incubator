use crate::prelude::*;
use crate::{
    object::ObjectValue,
    CallContext,
    Exception,
    Heap,
    Interpreted,
    JSObject,
    JSRef,
    JSResult,
    JSValue,
};

fn array_object_constructor(_call: CallContext, _heap: &mut Heap) -> JSResult<Interpreted> {
    todo!()
}

#[allow(non_snake_case)]
fn array_toString(call: CallContext, heap: &mut Heap) -> JSResult<Interpreted> {
    let array_object = heap.get(call.this_ref);
    let array = array_object.as_array().ok_or_else(|| {
        Exception::TypeErrorInstanceRequired(Interpreted::from(call.this_ref), "Array".to_string())
    })?;
    let array = array.storage.clone();

    let reprs = (array.iter())
        .map(|val| val.stringify(heap))
        .collect::<JSResult<Vec<_>>>()?;

    let s = reprs.join(",");
    Ok(Interpreted::from(s))
}

fn array_proto_push(call: CallContext, heap: &mut Heap) -> JSResult<Interpreted> {
    let arguments = (call.arguments.into_iter())
        .map(|arg| arg.to_value(heap))
        .collect::<JSResult<Vec<JSValue>>>()?;
    let array_object = heap.get_mut(call.this_ref);
    match &mut array_object.value {
        ObjectValue::Array(array) => {
            array.storage.extend(arguments.into_iter());
            let length = array_object
                .get_own_value("length")
                .unwrap_or_else(|| JSValue::from(0));
            Ok(Interpreted::from(length))
        }
        // TODO: generic object path
        _ => Err(Exception::TypeErrorNotArraylike(Interpreted::from(
            call.this_ref,
        ))),
    }
}

fn array_proto_pop(call: CallContext, heap: &mut Heap) -> JSResult<Interpreted> {
    let array_object = heap.get_mut(call.this_ref);
    match &mut array_object.value {
        ObjectValue::Array(array) => {
            let value = array.storage.pop().unwrap_or(JSValue::Undefined);
            Ok(Interpreted::from(value))
        }
        // TODO: generic object path
        _ => Err(Exception::TypeErrorNotArraylike(Interpreted::from(
            call.this_ref,
        ))),
    }
}

pub fn init(heap: &mut Heap) -> JSResult<JSRef> {
    let mut array_proto = JSObject::new();
    array_proto.set_hidden("toString", heap.alloc_func(array_toString))?;
    array_proto.set_hidden("push", heap.alloc_func(array_proto_push))?;
    array_proto.set_hidden("pop", heap.alloc_func(array_proto_pop))?;

    *heap.get_mut(Heap::ARRAY_PROTO) = array_proto;

    let mut array_object = JSObject::from_func(array_object_constructor);

    // Array.prototype
    array_object.set_system("prototype", Heap::ARRAY_PROTO)?;

    let array_ref = heap.alloc(array_object);
    heap.get_mut(Heap::ARRAY_PROTO)
        .set_system("constructor", array_ref)?;

    Ok(array_ref)
}
