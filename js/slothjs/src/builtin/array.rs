use crate::error::Exception;
use crate::object::{
    Content,
    Heap,
    Interpreted,
    JSObject,
    JSRef,
    JSValue,
};

fn array_object_constructor(
    _this_ref: JSRef,
    _method_name: String,
    _arguments: Vec<Interpreted>,
    _heap: &mut Heap,
) -> Result<Interpreted, Exception> {
    let object = JSObject::new();
    /*
    let values = arguments.into_iter()
        .map(|arg| arg.to_value(&heap))
        .collect::<Result<Vec<JSValue>, Exception>>()?;

    object.set_system(JSObject::VALUE, Content::Array(values));
    */
    Ok(Interpreted::Value(JSValue::Object(object)))
}

pub fn init(heap: &mut Heap) -> Result<JSRef, Exception> {
    let mut proto_object = JSObject::new();

    // Array.prototype.__proto__ = Object.prototype
    let object_proto_ref = heap.lookup_ref(&["Object", "prototype"])?;
    proto_object.set_system(
        JSObject::PROTO,
        Content::Data(object_proto_ref)
    );

    let proto_ref = heap.allocate(JSValue::Object(proto_object));

    let mut array_object = JSObject::new();

    // Array.__proto__ = Function.prototype
    let function_proto_ref = heap.lookup_ref(&["Function", "prototype"])?;
    array_object.set_system(JSObject::PROTO, Content::Data(function_proto_ref));

    // Array.prototype
    array_object.set_system("prototype", Content::Data(proto_ref));

    // Array()
    array_object.set_system(JSObject::VALUE, Content::NativeFunction(array_object_constructor));

    let object_ref = heap.allocate(JSValue::Object(array_object));
    heap.object_mut(proto_ref)?.set_system("constructor", Content::Data(object_ref));

    Ok(object_ref)
}
