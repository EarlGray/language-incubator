use crate::error::Exception;
use crate::heap::{
    Heap,
    JSRef,
};
use crate::object::{
    Content,
    Interpreted,
    JSObject,
    JSValue,
};

fn error_constructor(
    _this_ref: JSRef,
    _method_name: String,
    arguments: Vec<Interpreted>,
    heap: &'_ mut Heap,
) -> Result<Interpreted, Exception> {
    let message = (arguments.get(0))
        .unwrap_or(&Interpreted::from(""))
        .to_value(heap)?
        .stringify(heap)?;

    let mut error_object = JSObject::new();

    let error_proto = heap
        .lookup_path(&["Error", "prototype"])
        .expect("Error.prototype")
        .to_ref(heap)?;
    error_object.proto = error_proto;

    error_object.set_hidden("message", Content::from(message))?;

    let objref = heap.alloc(error_object);
    Ok(Interpreted::from(objref))
}

#[allow(non_snake_case)]
fn error_proto_toString(
    this_ref: JSRef,
    _method_name: String,
    _arguments: Vec<Interpreted>,
    heap: &'_ mut Heap,
) -> Result<Interpreted, Exception> {
    this_ref.expect_instance("Error", heap)?;

    let name = (heap.get(this_ref))
        .lookup_value("name", heap)
        .map(|v| v.clone())
        .unwrap_or(JSValue::from(""))
        .stringify(heap)?;

    let message = (heap.get(this_ref))
        .lookup_value("message", heap)
        .map(|v| v.clone())
        .unwrap_or(JSValue::from(""))
        .stringify(heap)?;

    Ok(Interpreted::from(match () {
        _ if message.is_empty() => name,
        _ if name.is_empty() => message,
        _ => name + ": " + &message,
    }))
}

pub fn init(heap: &mut Heap) -> Result<(), Exception> {
    let mut error_proto = JSObject::new();
    error_proto.set_hidden("name", Content::from("Error"))?;
    error_proto.set_hidden("message", Content::from(""))?;
    error_proto.set_hidden("toString", Content::from_func(error_proto_toString, heap))?;

    let error_proto_ref = heap.alloc(error_proto);

    let mut the_error = JSObject::from_func(error_constructor);
    the_error.set_system("prototype", Content::from(error_proto_ref))?;

    let the_error_ref = heap.alloc(the_error);
    heap.get_mut(Heap::GLOBAL)
        .set_hidden("Error", Content::from(the_error_ref))?;

    heap.get_mut(error_proto_ref)
        .set_hidden("constructor", Content::from(the_error_ref))?;

    Ok(())
}
