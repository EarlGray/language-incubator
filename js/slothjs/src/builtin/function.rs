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

fn function_proto_call(
    this_ref: JSRef,
    method_name: String,
    mut arguments: Vec<Interpreted>,
    heap: &mut Heap
) -> Result<Interpreted, Exception> {
    if arguments.len() == 0 {
        arguments.push(Interpreted::VOID);
    }
    arguments.rotate_left(1);
    let this_arg = arguments.pop().unwrap();
    let bound_this = this_arg.to_ref(heap).unwrap_or(Heap::NULL);
    heap.execute(this_ref, bound_this, &method_name, arguments)
}

fn function_proto_apply(
    this_ref: JSRef,
    method_name: String,
    arguments: Vec<Interpreted>,
    heap: &mut Heap
) -> Result<Interpreted, Exception> {
    let this_arg = arguments.get(0).unwrap_or(&Interpreted::VOID);
    let bound_this = this_arg.to_ref(heap)?;
    let call_args = match arguments.get(1) {
        // TODO: convert from everything array-like.
        Some(object) => {
            let objref = object.to_ref(heap)?;
            if let Some(array) = heap.get(objref).as_array() {
                array.storage.iter()
                    .map(|val| Interpreted::Value(val.clone()))
                    .collect()
            } else {
                return Err(Exception::TypeErrorNotArraylike(object.clone()))
            }
        }
        None => Vec::new()
    };
    heap.execute(this_ref, bound_this, &method_name, call_args)
}

pub fn init(heap: &mut Heap) -> Result<JSRef, Exception> {
    /* the Function.prototype */
    let mut function_proto = JSObject::new();
    function_proto.proto = Heap::OBJECT_PROTO;

    let func_call_ref = heap.alloc(JSObject::from_func(function_proto_call));
    function_proto.set_hidden("call", Content::from(func_call_ref))?;

    let func_apply_ref = heap.alloc(JSObject::from_func(function_proto_apply));
    function_proto.set_hidden("apply", Content::from(func_apply_ref))?;

    *heap.get_mut(Heap::FUNCTION_PROTO) = function_proto;

    /* the Function object */
    let mut function_object = JSObject::from_func(function_constructor);

    function_object.set_system("prototype", Content::from(Heap::FUNCTION_PROTO))?;

    let the_function_ref = heap.alloc(function_object);
    heap.get_mut(Heap::FUNCTION_PROTO).set_hidden("constructor", Content::from(the_function_ref))?;

    Ok(the_function_ref)
}
