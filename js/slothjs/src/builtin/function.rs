/// The implementation of the Function object.

use crate::error::Exception;
use crate::object::{
    Content,
    Interpreted,
    JSObject,
    JSRef,
    JSValue,
    Heap,
    PropertyFlags as Access,
};

fn function_constructor(
    _this_ref: JSRef,
    _method_name: String,
    _arguments: Vec<Interpreted>,
    _heap: &mut Heap
) -> Result<Interpreted, Exception> {
    todo!()
}

fn make_function_prototype(heap: &mut Heap) -> Result<JSRef, Exception> {
    let mut function_proto = JSObject::new();

    let object_prototype_ref = heap.lookup_ref(&["Object", "prototype"])?;
    function_proto.set_property_and_flags(
        "__proto__",
        Content::Data(object_prototype_ref),
        Access::NONE
    );

    Ok(heap.allocate(JSValue::Object(function_proto)))
}


pub fn init(heap: &mut Heap) -> Result<(), Exception> {
    let function_proto_ref = make_function_prototype(heap)?;

    /* the Function object */
    let mut function_object = JSObject::new();

    function_object.set_property_and_flags(
        "prototype",
        Content::Data(function_proto_ref),
        Access::NONE
    );
    function_object.set_property_and_flags(
        JSObject::VALUE,
        Content::NativeFunction(function_constructor),
        Access::NONE,
    );
    let function_object_ref = heap.allocate(JSValue::Object(function_object));

    /* global.Function = ... */
    heap.global_mut().set_property_and_flags(
        "Function",
        Content::Data(function_object_ref),
        Access::HIDDEN
    );

    /* Object.__proto__ = Function.prototype */
    let the_object = heap.lookup_ref(&["Object"])?;
    heap.object_mut(the_object)?.set_property_and_flags(
        "__proto__",
        Content::Data(function_proto_ref),
        Access::NONE
    );

    Ok(())
}
