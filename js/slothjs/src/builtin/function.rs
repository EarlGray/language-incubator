/// The implementation of the Function object.

use crate::error::Exception;
use crate::object::{
    Content,
    Interpreted,
    JSObject,
};
use crate::heap::{
    JSRef,
    Heap,
};

fn function_constructor(
    _this_ref: JSRef,
    _method_name: String,
    _arguments: Vec<Interpreted>,
    _heap: &mut Heap
) -> Result<Interpreted, Exception> {
    todo!()
}


pub fn init(heap: &mut Heap) -> Result<JSRef, Exception> {
    /* the Function.prototype */
    let mut function_proto = JSObject::new();
    function_proto.proto = Heap::OBJECT_PROTO;

    *heap.get_mut(Heap::FUNCTION_PROTO) = function_proto;

    /* the Function object */
    let mut function_object = JSObject::from_func(function_constructor);

    function_object.set_system("prototype", Content::from(Heap::FUNCTION_PROTO))?;

    let the_function_ref = heap.alloc(function_object);
    heap.get_mut(Heap::FUNCTION_PROTO).set_hidden("constructor", Content::from(the_function_ref))?;

    Ok(the_function_ref)
}
