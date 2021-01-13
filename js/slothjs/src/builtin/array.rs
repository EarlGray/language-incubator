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

pub fn init(heap: &mut Heap) -> Result<JSRef, Exception> {
    /*
    let mut proto_object = JSObject::new();

    *heap.get_mut(Heap::ARRAY_PROTO) = proto_object;
    */

    let mut array_object = JSObject::from_func(array_object_constructor);

    // Array.prototype
    array_object.set_system("prototype", Content::from(Heap::ARRAY_PROTO));

    let array_ref = heap.alloc(array_object);
    heap.get_mut(Heap::ARRAY_PROTO).set_system("constructor", Content::from(array_ref));

    Ok(array_ref)
}
