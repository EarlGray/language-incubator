/// The implementation of the Function object.

use crate::error::Exception;
use crate::object::{
    Content,
    JSObject,
    JSRef,
    JSValue,
    Heap,
    PropertyFlags as Access,
};


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
    let function_object_ref = heap.allocate(JSValue::Object(function_object));


    /* global.Function = ... */
    heap.global_mut().set_property_and_flags(
        "Function",
        Content::Data(function_object_ref),
        Access::HIDDEN
    );

    /* TODO: backpatch the Object to be a Function */

    Ok(())
}
