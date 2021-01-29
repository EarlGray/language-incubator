use crate::{
    Exception,
    Heap,
    JSObject,
    JSRef,
};
use crate::object::{
    Content,
    Interpreted,
    ObjectValue,
};

fn boolean_constructor(
    this_ref: JSRef,
    _method_name: String,
    arguments: Vec<Interpreted>,
    heap: &mut Heap
) -> Result<Interpreted, Exception> {
    let arg = arguments.get(0).unwrap_or(&Interpreted::VOID);
    let arg = arg.to_value(heap)?;
    let arg = arg.boolify(heap);

    if !heap.smells_fresh(this_ref) {
        return Ok(Interpreted::from(arg));
    }

    *heap.get_mut(this_ref) = JSObject::from_bool(arg);
    Ok(Interpreted::VOID)
}

fn object_to_bool(this_ref: JSRef, heap: &Heap) -> Result<bool, Exception> {
    match heap.get(this_ref).value {
        ObjectValue::Boolean(b) => Ok(b),
        _ => {
            let what = Interpreted::from(this_ref);
            Err(Exception::TypeErrorInstanceRequired(what, "Boolean".to_string()))
        }
    }
}

#[allow(non_snake_case)]
fn boolean_proto_toString(
    this_ref: JSRef,
    _method_name: String,
    _arguments: Vec<Interpreted>,
    heap: &mut Heap
) -> Result<Interpreted, Exception> {
    let b = object_to_bool(this_ref, heap)?;
    Ok(Interpreted::from(if b { "true" } else { "false" }))
}

#[allow(non_snake_case)]
fn boolean_proto_valueOf(
    this_ref: JSRef,
    _method_name: String,
    _arguments: Vec<Interpreted>,
    heap: &mut Heap
) -> Result<Interpreted, Exception> {
    let b = object_to_bool(this_ref, heap)?;
    Ok(Interpreted::from(b))
}

pub fn init(heap: &mut Heap) -> Result<JSRef, Exception> {
    /* Boolean.prototype */
    let mut boolean_proto = JSObject::new();

    let valueof_ref = heap.alloc(JSObject::from_func(boolean_proto_valueOf));
    boolean_proto.set_hidden("valueOf", Content::from(valueof_ref))?;

    let tostr_ref = heap.alloc(JSObject::from_func(boolean_proto_toString));
    boolean_proto.set_hidden("toString", Content::from(tostr_ref))?;

    *heap.get_mut(Heap::BOOLEAN_PROTO) = boolean_proto;

    /* the Boolean object */
    let mut the_boolean = JSObject::from_func(boolean_constructor);
    the_boolean.set_system("prototype", Content::from(Heap::BOOLEAN_PROTO))?;
   
    let the_boolean_ref = heap.alloc(the_boolean);

    heap.get_mut(Heap::BOOLEAN_PROTO).set_hidden("constructor", Content::from(the_boolean_ref))?;

    Ok(the_boolean_ref)
}
