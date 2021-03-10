use crate::{
    CallContext,
    Exception,
    Interpreted,
    JSObject,
    JSRef,
    Heap,
};
use crate::object::Content;

fn string_constructor(
    _call: CallContext,
    _heap: &mut Heap,
) -> Result<Interpreted, Exception> {
    todo!()
}

#[allow(non_snake_case)]
fn string_proto_valueOf(
    _call: CallContext,
    _heap: &mut Heap,
) -> Result<Interpreted, Exception> {
    todo!()
}

#[allow(non_snake_case)]
fn string_proto_toString(
    _call: CallContext,
    _heap: &mut Heap,
) -> Result<Interpreted, Exception> {
    todo!()
}

pub fn init(heap: &mut Heap) -> Result<JSRef, Exception> {
    let mut string_proto = JSObject::new();
    string_proto.set_hidden("valueOf", Content::from_func(string_proto_valueOf, heap))?;
    string_proto.set_hidden("toString", Content::from_func(string_proto_toString, heap))?;

    *heap.get_mut(Heap::STRING_PROTO) = string_proto;

    let mut the_string = JSObject::from_func(string_constructor);
    the_string.set_system("prototype", Content::from(Heap::STRING_PROTO))?;

    let the_string_ref = heap.alloc(the_string);
    heap.get_mut(Heap::STRING_PROTO)
        .set_hidden("constructor", Content::from(the_string_ref))?;

    Ok(the_string_ref)
}
