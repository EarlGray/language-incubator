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

pub fn init(heap: &mut Heap) -> Result<JSRef, Exception> {
    let mut array_proto = JSObject::new();

    array_proto.set_hidden("toString", Content::from_func(array_toString, heap))?;

    *heap.get_mut(Heap::ARRAY_PROTO) = array_proto;

    let mut array_object = JSObject::from_func(array_object_constructor);

    // Array.prototype
    array_object.set_system("prototype", Content::from(Heap::ARRAY_PROTO))?;

    let array_ref = heap.alloc(array_object);
    heap.get_mut(Heap::ARRAY_PROTO)
        .set_system("constructor", Content::from(array_ref))?;

    Ok(array_ref)
}
