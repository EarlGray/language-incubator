use crate::error::Exception;
use crate::function::CallContext;
use crate::heap::{
    Heap,
    JSRef,
};
use crate::object::{
    Content,
    Interpreted,
    JSObject,
};
/// The implementation of the Function object.
use crate::prelude::*;

fn function_constructor(_call: CallContext, _heap: &mut Heap) -> Result<Interpreted, Exception> {
    todo!()
}

fn function_proto_call(mut call: CallContext, heap: &mut Heap) -> Result<Interpreted, Exception> {
    if call.arguments.is_empty() {
        call.arguments.push(Interpreted::VOID);
    }
    call.arguments.rotate_left(1); // [this, arg1, .. , argN] => [arg1, .. , argN, this]
    let this_arg = call.arguments.pop().unwrap();
    let bound_this = this_arg
        .to_ref(heap)
        .or_else(|_| Ok(this_arg.to_value(heap)?.objectify(heap)))?;

    let funcref = call.this_ref;
    call.this_ref = bound_this;

    heap.execute(funcref, call)
}

fn function_proto_apply(call: CallContext, heap: &mut Heap) -> Result<Interpreted, Exception> {
    let this_arg = call.arguments.get(0).unwrap_or(&Interpreted::VOID);
    let bound_this = this_arg.to_ref(heap)?;
    let call_args = match call.arguments.get(1) {
        // TODO: convert from everything array-like.
        Some(object) => {
            let objref = object.to_ref(heap)?;
            let array = (heap.get(objref))
                .as_array()
                .ok_or_else(|| Exception::TypeErrorNotArraylike(object.clone()))?;
            (array.storage.iter())
                .map(|val| Interpreted::Value(val.clone()))
                .collect()
        }
        None => Vec::new(),
    };

    let funcref = call.this_ref;
    heap.execute(funcref, CallContext {
        this_ref: bound_this,
        method_name: call.method_name,
        arguments: call_args,
        loc: heap.loc.clone(),
    })
}

pub fn init(heap: &mut Heap) -> Result<JSRef, Exception> {
    /* the Function.prototype */
    let mut function_proto = JSObject::new();
    function_proto.proto = Heap::OBJECT_PROTO;

    function_proto.set_hidden("call", Content::from_func(function_proto_call, heap))?;
    function_proto.set_hidden("apply", Content::from_func(function_proto_apply, heap))?;

    *heap.get_mut(Heap::FUNCTION_PROTO) = function_proto;

    /* the Function object */
    let mut function_object = JSObject::from_func(function_constructor);

    function_object.set_system("prototype", Content::from(Heap::FUNCTION_PROTO))?;

    let the_function_ref = heap.alloc(function_object);
    heap.get_mut(Heap::FUNCTION_PROTO)
        .set_hidden("constructor", Content::from(the_function_ref))?;

    Ok(the_function_ref)
}
