use crate::object::HostClass;
/// The implementation of the builtin Function object.
use crate::prelude::*;
use crate::{
    function::CallContext,
    Exception,
    Heap,
    Interpreted,
    JSResult,
};

pub static CLASS: HostClass = HostClass {
    name: "Function",
    constructor: function_constructor,
    methods: &[
        ("call", function_proto_call),
        ("apply", function_proto_apply),
    ],
    static_methods: &[],
};

fn function_constructor(_call: CallContext, _heap: &mut Heap) -> JSResult<Interpreted> {
    // TODO: eval arguments
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

    heap.execute(
        call.this_ref,
        CallContext::from(call_args)
            .with_this(bound_this)
            .with_name(call.method_name),
    )
}
