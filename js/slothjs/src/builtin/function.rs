/// The implementation of the Function object.

use crate::error::Exception;
use crate::object::{
    Content,
    Interpreted,
    JSObject,
    JSRef,
    JSValue,
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

pub fn init_proto(heap: &mut Heap, object_proto: JSRef) -> JSRef {
    let mut function_proto = JSObject::new();

    // Function.prototype.__proto__ = Object.prototype
    function_proto.set_system(JSObject::PROTO, Content::Data(object_proto));

    heap.allocate(JSValue::Object(function_proto))
}


pub fn init_object(heap: &mut Heap, proto_ref: JSRef) -> Result<JSRef, Exception> {
    /* the Function object */
    let mut function_object = JSObject::new();

    function_object.set_system(
        "prototype",
        Content::Data(proto_ref),
    );
    function_object.set_system(
        JSObject::VALUE,
        Content::NativeFunction(function_constructor),
    );

    let function_object_ref = heap.allocate(JSValue::Object(function_object));
    Ok(function_object_ref)
}
