/// The implementation of the Object object

//use crate::object;
use crate::object::{
    Access,
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
    Ok(Interpreted::from("[object Object]"))
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
    arguments: Vec<Interpreted>,
    heap: &mut Heap
) -> Result<Interpreted, Exception> {
    let argument = arguments.get(0).unwrap_or(&Interpreted::VOID);
    let object_ref = match argument.to_value(heap)? {
        JSValue::Undefined | JSValue::Ref(Heap::NULL) =>
            heap.alloc(JSObject::new()),
        JSValue::Bool(b) =>
            heap.alloc(JSObject::from_bool(b)),
        JSValue::Number(_) => todo!(),
        JSValue::String(_) => todo!(),
        JSValue::Ref(r) => r,
    };
    Ok(Interpreted::from(object_ref))
}

#[allow(non_snake_case)]
fn object_proto_valueOf(
    this_ref: JSRef,
    _method_name: String,
    _arguments: Vec<Interpreted>,
    _heap: &mut Heap
) -> Result<Interpreted, Exception> {
    Ok(Interpreted::from(this_ref))
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
    let propname = propname.to_value(&*heap)?;
    let propname = propname.stringify(heap)?;

    let inspected_object = heap.get_mut(inspected_ref);
    let prop = match inspected_object.properties.get(&propname) {
        Some(prop) => prop.clone(),
        None => return Ok(Interpreted::VOID)
    };

    let mut descriptor_object = JSObject::new();
    descriptor_object.set_property(
        "configurable",
        Content::from(prop.access.configurable())
    )?;
    descriptor_object.set_property(
        "enumerable",
        Content::from(prop.access.enumerable())
    )?;
    descriptor_object.set_property(
        "writable",
        Content::from(prop.access.writable())
    )?;
    let value = prop.content.to_value()?;
    descriptor_object.set_property(
        "value",
        Content::from(value)
    )?;

    let descriptor_ref = heap.alloc(descriptor_object);
    Ok(Interpreted::from(descriptor_ref))
}

#[allow(non_snake_case)]
fn object_object_defineProperty(
    _this_ref: JSRef,
    _method_name: String,
    arguments: Vec<Interpreted>,
    heap: &mut Heap
) -> Result<Interpreted, Exception> {
    let objref = arguments.get(0)
        .unwrap_or(&Interpreted::VOID)
        .to_ref(heap)?;

    let prop = arguments.get(1)
        .unwrap_or(&Interpreted::VOID)
        .to_value(heap)?
        .stringify(heap)?;

    let descref = arguments.get(2)
        .unwrap_or(&Interpreted::VOID)
        .to_ref(heap)?;
    let descriptor = heap.get(descref);

    let configurable = descriptor.get_value("configurable")
        .unwrap_or(&JSValue::from(false))
        .boolify(heap);
    let enumerable = descriptor.get_value("enumerable")
        .unwrap_or(&JSValue::from(false))
        .boolify(heap);
    let has_value = descriptor.get_value("value").is_some()
        || descriptor.get_value("writable").is_some();
    let has_get = descriptor.get_value("get").is_some();
    let has_set = descriptor.get_value("set").is_some();
    if has_get || has_set {
        if has_value {
            let what = Interpreted::from(descref);
            return Err(Exception::TypeErrorInvalidDescriptor(what));
        }

        todo!()
    } else {
        let value = descriptor.get_value("value")
            .unwrap_or(&JSValue::Undefined)
            .clone();
        let writable = descriptor.get_value("writable")
            .unwrap_or(&JSValue::from(false))
            .boolify(heap);
        let mut access = Access::empty();
        if configurable { access = access | Access::CONF }
        if enumerable { access = access | Access::ENUM }
        if writable { access = access | Access::WRITE }

        heap.get_mut(objref)
            .set(&prop, Content::Value(value), access)?;

        Ok(Interpreted::VOID)
    }
}

pub fn init(heap: &mut Heap) -> Result<JSRef, Exception> {
    /* Object.prototype */
    let mut object_proto = JSObject::new();
    object_proto.proto = Heap::NULL;

    object_proto.set_system(
        "dbg",
        Content::from(heap.alloc(JSObject::from_func(object_proto_dbg)))
    )?;
    object_proto.set_hidden(
        "toString",
        Content::from(heap.alloc(JSObject::from_func(object_proto_toString)))
    )?;
    object_proto.set_hidden(
        "valueOf",
        Content::from(heap.alloc(JSObject::from_func(object_proto_valueOf)))
    )?;

    *heap.get_mut(Heap::OBJECT_PROTO) = object_proto;

    /* the Object */
    let mut object_object = JSObject::from_func(object_constructor);

    object_object.set_system("prototype", Content::from(Heap::OBJECT_PROTO))?;

    let is_ref = heap.alloc(JSObject::from_func(object_object_is));
    object_object.set_hidden("is", Content::from(is_ref))?;

    let defprop_ref = heap.alloc(JSObject::from_func(object_object_defineProperty));
    object_object.set_hidden("defineProperty", Content::from(defprop_ref))?;

    let gop_ref = heap.alloc(JSObject::from_func(object_object_getOwnPropertyDescriptor));
    object_object.set_hidden(
        "getOwnPropertyDescriptor",
        Content::from(gop_ref),
    )?;

    let the_object_ref = heap.alloc(object_object);
    heap.get_mut(Heap::OBJECT_PROTO).set_hidden("constructor", Content::from(the_object_ref))?;

    Ok(the_object_ref)
}
