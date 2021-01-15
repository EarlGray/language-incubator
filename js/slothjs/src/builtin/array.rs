use crate::error::Exception;
use crate::object::{
    Content,
    Interpreted,
    JSObject,
};
use crate::heap::{
    Heap,
    JSRef,
};

fn array_object_constructor(
    _this_ref: JSRef,
    _method_name: String,
    _arguments: Vec<Interpreted>,
    _heap: &mut Heap,
) -> Result<Interpreted, Exception> {
    todo!()
}

#[allow(non_snake_case)]
fn array_toString(
    this_ref: JSRef,
    _method_name: String,
    _arguments: Vec<Interpreted>,
    heap: &mut Heap,
) -> Result<Interpreted, Exception> {
    let array = heap.get(this_ref)
        .as_array().expect("Array.prototype.toString on something that is not Array")
        .storage.clone();
    let reprs = array.iter().map(|val|
        val.stringify(heap)
    ).collect::<Result<Vec<_>, Exception>>()?;

    let s = reprs.join(",");
    Ok(Interpreted::from(s))
}

pub fn init(heap: &mut Heap) -> Result<JSRef, Exception> {
    let mut array_proto = JSObject::new();

    let tostr_ref = heap.alloc(JSObject::from_func(array_toString));
    array_proto.set_hidden("toString", Content::from(tostr_ref));

    *heap.get_mut(Heap::ARRAY_PROTO) = array_proto;

    let mut array_object = JSObject::from_func(array_object_constructor);

    // Array.prototype
    array_object.set_system("prototype", Content::from(Heap::ARRAY_PROTO));

    let array_ref = heap.alloc(array_object);
    heap.get_mut(Heap::ARRAY_PROTO).set_system("constructor", Content::from(array_ref));

    Ok(array_ref)
}
