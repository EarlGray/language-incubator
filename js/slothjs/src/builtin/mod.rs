mod array;
mod console;
mod function;
mod global;
mod object;

use crate::error::Exception;
use crate::object::{
    Content,
    Heap,
    JSObject,
};

pub fn init(heap: &mut Heap) -> Result<(), Exception> {
    global::init(heap)?;

    let object_proto = object::init_proto(heap);
    heap.global_mut().set_system(JSObject::PROTO, Content::Data(object_proto));
    let function_proto = function::init_proto(heap, object_proto);

    let the_object = object::init_object(heap, object_proto)?;
    heap.object_mut(the_object)?.set_system(JSObject::PROTO, Content::Data(function_proto));
    heap.global_mut().set_hidden("Object", Content::Data(the_object));
    heap.object_mut(object_proto)?.set_hidden("constructor", Content::Data(the_object));

    let the_function = function::init_object(heap, function_proto)?;
    heap.global_mut().set_hidden("Function", Content::Data(the_function));
    heap.object_mut(function_proto)?.set_hidden("constructor", Content::Data(the_function));

    array::init(heap)?;
    console::init(heap)?;

    Ok(())
}
