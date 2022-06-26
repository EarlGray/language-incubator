/// The implementation of the builtin Function object.
use crate::prelude::*;
use crate::{
    function::CallContext,
    Exception,
    Heap,
    Interpreted,
    JSObject,
    JSRef,
    JSResult,
};

fn function_constructor(_call: CallContext, _heap: &mut Heap) -> JSResult<Interpreted> {
    todo!()
}

fn function_proto_call(mut call: CallContext, heap: &mut Heap) -> JSResult<Interpreted> {
    if call.arguments.is_empty() {
        call.arguments.push(Interpreted::VOID);
    }
    call.arguments.rotate_left(1); // [this, arg1, .. , argN] => [arg1, .. , argN, this]
    let this_arg = call.arguments.pop().unwrap();
    let bound_this = this_arg.to_value(heap)?.objectify(heap); // Must wrap primitives too.
    let funcref = call.this_ref;
    call.this_ref = bound_this;

    heap.execute(funcref, call)
}

fn function_proto_apply(call: CallContext, heap: &mut Heap) -> JSResult<Interpreted> {
    let bound_this = call.arg_value(0, heap)?.to_ref()?;
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
    heap.execute(
        funcref,
        CallContext {
            this_ref: bound_this,
            method_name: call.method_name,
            arguments: call_args,
            loc: heap.loc.clone(),
        },
    )
}

pub fn init(heap: &mut Heap) -> JSResult<JSRef> {
    /* the Function.prototype */
    let mut function_proto = JSObject::new();
    function_proto.set_hidden("call", heap.alloc_func(function_proto_call))?;
    function_proto.set_hidden("apply", heap.alloc_func(function_proto_apply))?;

    *heap.get_mut(Heap::FUNCTION_PROTO) = function_proto;

    /* the Function object */
    let mut function_object = JSObject::from_func(function_constructor);

    function_object.set_system("prototype", Heap::FUNCTION_PROTO)?;

    let the_function_ref = heap.alloc(function_object);
    heap.get_mut(Heap::FUNCTION_PROTO)
        .set_hidden("constructor", the_function_ref)?;

    Ok(the_function_ref)
}
