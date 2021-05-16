use crate::error::Exception;
use crate::function::CallContext;
use crate::heap::{
    Heap,
    JSRef,
};
use crate::object::{
    Content,
    Interpreted,
    JSObject,
    JSValue,
    ObjectValue,
};

fn array_object_constructor(
    _call: CallContext,
    _heap: &mut Heap,
) -> Result<Interpreted, Exception> {
    todo!()
}

#[allow(non_snake_case)]
fn array_toString(call: CallContext, heap: &mut Heap) -> Result<Interpreted, Exception> {
    let array_object = heap.get(call.this_ref);
    let array = (array_object.as_array()).ok_or(Exception::TypeErrorInstanceRequired(
        Interpreted::from(call.this_ref),
        "Array".to_string(),
    ))?;
    let array = array.storage.clone();

    let reprs = (array.iter())
        .map(|val| val.stringify(heap))
        .collect::<Result<Vec<_>, Exception>>()?;

    let s = reprs.join(",");
    Ok(Interpreted::from(s))
}

fn array_proto_push(call: CallContext, heap: &mut Heap) -> Result<Interpreted, Exception> {
    let arguments = (call.arguments.into_iter())
        .map(|arg| arg.to_value(heap))
        .collect::<Result<Vec<JSValue>, Exception>>()?;
    let array_object = heap.get_mut(call.this_ref);
    match &mut array_object.value {
        ObjectValue::Array(array) => {
            array.storage.extend(arguments.into_iter());
            let length = array_object.get_value("length").unwrap_or(JSValue::from(0));
            Ok(Interpreted::from(length))
        }
        // TODO: generic object path
        _ => Err(Exception::TypeErrorNotArraylike(Interpreted::from(
            call.this_ref,
        ))),
    }
}

pub fn init(heap: &mut Heap) -> Result<JSRef, Exception> {
    let mut array_proto = JSObject::new();

    array_proto.set_hidden("toString", Content::from_func(array_toString, heap))?;
    array_proto.set_hidden("push", Content::from_func(array_proto_push, heap))?;

    *heap.get_mut(Heap::ARRAY_PROTO) = array_proto;

    let mut array_object = JSObject::from_func(array_object_constructor);

    // Array.prototype
    array_object.set_system("prototype", Content::from(Heap::ARRAY_PROTO))?;

    let array_ref = heap.alloc(array_object);
    heap.get_mut(Heap::ARRAY_PROTO)
        .set_system("constructor", Content::from(array_ref))?;

    Ok(array_ref)
}
