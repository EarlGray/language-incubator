use crate::error::TypeError;
use crate::object::HostClass;
use crate::prelude::*;
use crate::{object::ObjectValue, CallContext, Exception, Heap, Interpreted, JSResult};

fn array_object_constructor(_call: CallContext, _heap: &mut Heap) -> JSResult<Interpreted> {
    todo!()
}

#[allow(non_snake_case)]
fn array_toString(call: CallContext, heap: &mut Heap) -> JSResult<Interpreted> {
    let array_object = heap.get(call.this_ref);
    let array = array_object.as_array().ok_or_else(|| {
        Exception::attr_type_error(TypeError::INSTANCE_REQUIRED, call.this_ref, "Array")
    })?;
    let array = array.storage.clone();

    let mut s = String::new();
    for val in array.iter() {
        if !s.is_empty() {
            s += ",";
        }
        s += val.stringify(heap)?.as_ref();
    }
    Ok(Interpreted::from(s))
}

fn array_proto_push(call: CallContext, heap: &mut Heap) -> JSResult<Interpreted> {
    let arguments = (call.arguments.into_iter())
        .map(|arg| arg.to_value(heap))
        .collect::<JSResult<Vec<JSValue>>>()?;
    let array_object = heap.get_mut(call.this_ref);
    match &mut array_object.value {
        ObjectValue::Array(array) => {
            array.storage.extend(arguments);
            let length = array_object
                .get_own_value("length")
                .unwrap_or_else(|| JSValue::from(0));
            Ok(Interpreted::from(length))
        }
        // TODO: generic object path
        _ => Err(Exception::type_error(
            TypeError::NOT_ARRAYLIKE,
            call.this_ref,
        )),
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
        _ => Err(Exception::type_error(
            TypeError::NOT_ARRAYLIKE,
            call.this_ref,
        )),
    }
}

pub static CLASS: HostClass = HostClass {
    name: "Array",
    constructor: array_object_constructor,
    methods: &[
        ("pop", array_proto_pop),
        ("push", array_proto_push),
        ("toString", array_toString),
    ],
    static_methods: &[],
};
