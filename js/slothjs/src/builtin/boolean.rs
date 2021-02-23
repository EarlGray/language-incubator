use crate::function::CallContext;
use crate::object::{
    Content,
    Interpreted,
    ObjectValue,
};
use crate::{
    Exception,
    Heap,
    JSObject,
    JSRef,
};

fn boolean_constructor(call: CallContext, heap: &mut Heap) -> Result<Interpreted, Exception> {
    let arg = call.arguments.get(0).unwrap_or(&Interpreted::VOID);
    let arg = arg.to_value(heap)?;
    let arg = arg.boolify(heap);

    if !heap.smells_fresh(call.this_ref) {
        return Ok(Interpreted::from(arg));
    }

    *heap.get_mut(call.this_ref) = JSObject::from_bool(arg);
    Ok(Interpreted::VOID)
}

fn object_to_bool(this_ref: JSRef, heap: &Heap) -> Result<bool, Exception> {
    match heap.get(this_ref).value {
        ObjectValue::Boolean(b) => Ok(b),
        _ => {
            let what = Interpreted::from(this_ref);
            let of = "Boolean".to_string();
            Err(Exception::TypeErrorInstanceRequired(what, of))
        }
    }
}

#[allow(non_snake_case)]
fn boolean_proto_toString(call: CallContext, heap: &mut Heap) -> Result<Interpreted, Exception> {
    let b = object_to_bool(call.this_ref, heap)?;
    Ok(Interpreted::from(if b { "true" } else { "false" }))
}

#[allow(non_snake_case)]
fn boolean_proto_valueOf(call: CallContext, heap: &mut Heap) -> Result<Interpreted, Exception> {
    let b = object_to_bool(call.this_ref, heap)?;
    Ok(Interpreted::from(b))
}

pub fn init(heap: &mut Heap) -> Result<JSRef, Exception> {
    /* Boolean.prototype */
    let mut boolean_proto = JSObject::new();

    boolean_proto.set_hidden("valueOf", Content::from_func(boolean_proto_valueOf, heap))?;
    boolean_proto.set_hidden("toString", Content::from_func(boolean_proto_toString, heap))?;

    *heap.get_mut(Heap::BOOLEAN_PROTO) = boolean_proto;

    /* the Boolean object */
    let mut the_boolean = JSObject::from_func(boolean_constructor);
    the_boolean.set_system("prototype", Content::from(Heap::BOOLEAN_PROTO))?;

    let the_boolean_ref = heap.alloc(the_boolean);

    heap.get_mut(Heap::BOOLEAN_PROTO)
        .set_hidden("constructor", Content::from(the_boolean_ref))?;

    Ok(the_boolean_ref)
}
