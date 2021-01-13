/// The implementation of the Object object

//use crate::object;
use crate::object::{
    Content,
    Interpreted,
    JSObject,
    JSValue,
};
use crate::heap::{
    Heap,
    JSRef,
};
use crate::error::Exception;

#[allow(non_snake_case)]
fn object_proto_toString(
    _this_ref: JSRef,
    _method_name: String,
    _arguments: Vec<Interpreted>,
    _heap: &mut Heap
) -> Result<Interpreted, Exception> {
    Ok(Interpreted::Value(JSValue::from("TODO")))
}

fn object_proto_dbg(
    this_ref: JSRef,
    _method_name: String,
    _arguments: Vec<Interpreted>,
    heap: &mut Heap
) -> Result<Interpreted, Exception> {
    dbg!(this_ref);
    dbg!(heap.get(this_ref));
    Ok(Interpreted::VOID)
}


fn object_constructor(
    _this_ref: JSRef,
    _method_name: String,
    _arguments: Vec<Interpreted>,
    _heap: &mut Heap
) -> Result<Interpreted, Exception> {
    todo!()
}

fn object_object_is(
    _this_ref: JSRef,
    _method_name: String,
    arguments: Vec<Interpreted>,
    heap: &mut Heap
) -> Result<Interpreted, Exception> {
    use JSValue::*;

    let left = arguments.get(0).unwrap_or(&Interpreted::VOID);
    let right = arguments.get(1).unwrap_or(&Interpreted::VOID);

    let answer = match (left.to_value(heap), right.to_value(heap)) {
        (Ok(Undefined), Ok(Undefined)) => true,
        (Ok(String(lstr)), Ok(String(rstr))) if &lstr == &rstr => true,
        (Ok(Bool(lb)), Ok(Bool(rb))) if lb == rb => true,
        (Ok(Number(lnum)), Ok(Number(rnum))) =>
            if f64::abs(lnum) == 0.0 && f64::abs(rnum) == 0.0 {
                f64::is_sign_positive(lnum) == f64::is_sign_positive(rnum)
            } else if f64::is_nan(lnum) && f64::is_nan(rnum) {
                true
            } else if lnum == rnum {
                true
            } else {
                false
            },
        _ => match (left.to_ref(heap), right.to_ref(heap)) {
            (Ok(lref), Ok(rref)) => lref == rref,
            _ => false
            }
    };
    Ok(Interpreted::Value(JSValue::Bool(answer)))
}

#[allow(non_snake_case)]
fn object_object_getOwnPropertyDescriptor(
    _this_ref: JSRef,
    _method_name: String,
    arguments: Vec<Interpreted>,
    heap: &mut Heap
) -> Result<Interpreted, Exception> {
    let inspected = arguments.get(0).unwrap_or(&Interpreted::VOID);
    let inspected_ref = inspected.to_ref(heap).map_err(|_|
        Exception::ReferenceNotAnObject(inspected.clone())
    )?;

    let propname = arguments.get(1).unwrap_or(&Interpreted::VOID);
    let propname = propname.to_value(&*heap)?.stringify(&*heap);

    let inspected_object = heap.get_mut(inspected_ref);
    let prop = match inspected_object.properties.get(&propname) {
        Some(prop) => prop.clone(),
        None => return Ok(Interpreted::VOID)
    };

    let descriptor_ref = heap.allocate();
    let descriptor_object = heap.get_mut(descriptor_ref);
    descriptor_object.set_property(
        "configurable",
        Content::from(prop.access.configurable())
    );
    descriptor_object.set_property(
        "enumerable",
        Content::from(prop.access.enumerable())
    );
    descriptor_object.set_property(
        "writable",
        Content::from(prop.access.writable())
    );
    let value = match prop.content {
        Content::Value(val) => val,
    };
    descriptor_object.set_property(
        "value",
        Content::Value(value)
    );

    Ok(Interpreted::from(descriptor_ref))
}

pub fn init(heap: &mut Heap) -> Result<JSRef, Exception> {
    /* Object.prototype */
    let mut object_proto = JSObject::new();
    object_proto.proto = Heap::NULL;

    let to_string_ref = heap.alloc(JSObject::from_func(object_proto_toString));
    object_proto.set_hidden("toString", Content::from(to_string_ref));

    let dbg_ref = heap.alloc(JSObject::from_func(object_proto_dbg));
    object_proto.set_system("dbg", Content::from(dbg_ref));

    *heap.get_mut(Heap::OBJECT_PROTO) = object_proto;

    /* the Object */
    let mut object_object = JSObject::from_func(object_constructor);

    object_object.set_system("prototype", Content::from(Heap::OBJECT_PROTO));

    let is_ref = heap.alloc(JSObject::from_func(object_object_is));
    object_object.set_hidden("is", Content::from(is_ref));

    let gop_ref = heap.alloc(JSObject::from_func(object_object_getOwnPropertyDescriptor));
    object_object.set_hidden(
        "getOwnPropertyDescriptor",
        Content::from(gop_ref),
    );

    let the_object_ref = heap.alloc(object_object);
    heap.get_mut(Heap::OBJECT_PROTO).set_hidden("constructor", Content::from(the_object_ref));

    Ok(the_object_ref)
}
