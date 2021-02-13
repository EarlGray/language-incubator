use crate::object::{
    Access,
    Content,
    JSValue,
};
use crate::{
    Exception,
    Heap,
    Interpreted,
    JSObject,
    JSRef,
};

fn object_constructor(
    _this_ref: JSRef,
    _method_name: String,
    arguments: Vec<Interpreted>,
    heap: &mut Heap,
) -> Result<Interpreted, Exception> {
    let argument = arguments.get(0).unwrap_or(&Interpreted::VOID);
    let value = argument.to_value(heap)?;
    let object_ref = match value.objectify(heap) {
        Heap::NULL => heap.alloc(JSObject::new()),
        href => href,
    };
    Ok(Interpreted::from(object_ref))
}

#[allow(non_snake_case)]
fn object_proto_hasOwnProperty(
    this_ref: JSRef,
    _method_name: String,
    arguments: Vec<Interpreted>,
    heap: &mut Heap,
) -> Result<Interpreted, Exception> {
    let argument = arguments.get(0).unwrap_or(&Interpreted::VOID);
    let propname = argument.to_value(heap)?.stringify(heap)?;
    let this_object = heap.get(this_ref);
    let found = this_object.get_value(&propname).is_some(); // TODO: avoid calling getters, if any
    Ok(Interpreted::from(found))
}

#[allow(non_snake_case)]
fn object_proto_toString(
    _this_ref: JSRef,
    _method_name: String,
    _arguments: Vec<Interpreted>,
    _heap: &mut Heap,
) -> Result<Interpreted, Exception> {
    Ok(Interpreted::from("[object Object]"))
}

fn object_proto_dbg(
    this_ref: JSRef,
    _method_name: String,
    _arguments: Vec<Interpreted>,
    heap: &mut Heap,
) -> Result<Interpreted, Exception> {
    dbg!(this_ref);
    dbg!(heap.get(this_ref));
    Ok(Interpreted::VOID)
}

#[allow(non_snake_case)]
fn object_proto_valueOf(
    this_ref: JSRef,
    _method_name: String,
    _arguments: Vec<Interpreted>,
    _heap: &mut Heap,
) -> Result<Interpreted, Exception> {
    // primitive wrappers are handled by their own `.valueOf()`
    Ok(Interpreted::from(this_ref))
}

fn object_object_create(
    _this_ref: JSRef,
    _method_name: String,
    arguments: Vec<Interpreted>,
    heap: &mut Heap,
) -> Result<Interpreted, Exception> {
    let proto = (arguments.get(0))
        .ok_or_else(|| Exception::TypeErrorInvalidPrototype(Interpreted::VOID))?;
    let protoref =
        (proto.to_ref(heap)).map_err(|_| Exception::TypeErrorInvalidPrototype(proto.clone()))?;
    let properties: Option<JSRef> = arguments.get(1).and_then(|props| props.to_ref(heap).ok());

    let mut object = JSObject::new();
    object.proto = protoref;

    let objref = heap.alloc(object);

    if let Some(property_descriptions) = properties {
        define_properties(objref, property_descriptions, heap)?;
    }

    Ok(Interpreted::from(objref))
}

fn object_object_is(
    _this_ref: JSRef,
    _method_name: String,
    arguments: Vec<Interpreted>,
    heap: &mut Heap,
) -> Result<Interpreted, Exception> {
    use JSValue::*;

    let left = arguments.get(0).unwrap_or(&Interpreted::VOID);
    let right = arguments.get(1).unwrap_or(&Interpreted::VOID);

    let answer = match (left.to_value(heap), right.to_value(heap)) {
        (Ok(Undefined), Ok(Undefined)) => true,
        (Ok(String(lstr)), Ok(String(rstr))) if &lstr == &rstr => true,
        (Ok(Bool(lb)), Ok(Bool(rb))) if lb == rb => true,
        (Ok(Number(lnum)), Ok(Number(rnum))) => {
            if f64::abs(lnum) == 0.0 && f64::abs(rnum) == 0.0 {
                f64::is_sign_positive(lnum) == f64::is_sign_positive(rnum)
            } else {
                (f64::is_nan(lnum) && f64::is_nan(rnum)) || lnum == rnum
            }
        }
        _ => match (left.to_ref(heap), right.to_ref(heap)) {
            (Ok(lref), Ok(rref)) => lref == rref,
            _ => false,
        },
    };
    Ok(Interpreted::from(answer))
}

#[allow(non_snake_case)]
fn object_object_getOwnPropertyDescriptor(
    _this_ref: JSRef,
    _method_name: String,
    arguments: Vec<Interpreted>,
    heap: &mut Heap,
) -> Result<Interpreted, Exception> {
    let inspected = arguments.get(0).unwrap_or(&Interpreted::VOID);
    let inspected_ref =
        (inspected.to_ref(heap)).map_err(|_| Exception::ReferenceNotAnObject(inspected.clone()))?;

    let propname = (arguments.get(1).unwrap_or(&Interpreted::VOID))
        .to_value(&*heap)?
        .stringify(heap)?;

    let inspected_object = heap.get_mut(inspected_ref);
    let prop = match inspected_object.properties.get(&propname) {
        Some(prop) => prop.clone(),
        None => return Ok(Interpreted::VOID),
    };

    let value = prop.content.to_value()?;

    let mut descriptor_object = JSObject::new();
    descriptor_object.set_property("value", Content::from(value))?;
    descriptor_object.set_property("configurable", Content::from(prop.access.configurable()))?;
    descriptor_object.set_property("enumerable", Content::from(prop.access.enumerable()))?;
    descriptor_object.set_property("writable", Content::from(prop.access.writable()))?;

    let descriptor_ref = heap.alloc(descriptor_object);
    Ok(Interpreted::from(descriptor_ref))
}

fn define_property(
    objref: JSRef,
    propname: String,
    descref: JSRef,
    heap: &mut Heap,
) -> Result<(), Exception> {
    let get_value = |object: &JSObject, name: &str| {
        object
            .get_value(name)
            .unwrap_or(&JSValue::Undefined)
            .clone()
    };
    let get_bool = |object: &JSObject, name: &str| get_value(object, name).boolify(heap);

    let descriptor = heap.get(descref);

    let configurable = get_bool(descriptor, "configurable");
    let enumerable = get_bool(descriptor, "enumerable");

    let has_writable = descriptor.get_value("writable").is_some();
    let has_value = descriptor.get_value("value").is_some();
    let has_get = descriptor.get_value("get").is_some();
    let has_set = descriptor.get_value("set").is_some();
    if has_get || has_set {
        if has_value || has_writable {
            let what = Interpreted::from(descref);
            return Err(Exception::TypeErrorInvalidDescriptor(what));
        }

        todo!()
    } else {
        let value = get_value(descriptor, "value");
        let writable = get_bool(descriptor, "writable");
        let access = Access::new(configurable, enumerable, writable);

        heap.get_mut(objref)
            .set(&propname, Content::Value(value), access)?;
    }
    Ok(())
}

#[allow(non_snake_case)]
fn object_object_defineProperty(
    _this_ref: JSRef,
    _method_name: String,
    arguments: Vec<Interpreted>,
    heap: &mut Heap,
) -> Result<Interpreted, Exception> {
    let objarg = arguments.get(0).unwrap_or(&Interpreted::VOID);
    let objref = (objarg.to_ref(heap))
        .map_err(|_| Exception::TypeErrorInstanceRequired(objarg.clone(), "Object".to_string()))?;

    let prop = (arguments.get(1).unwrap_or(&Interpreted::VOID))
        .to_value(heap)?
        .stringify(heap)?;

    let descref = (arguments.get(2).unwrap_or(&Interpreted::VOID)).to_ref(heap)?;

    define_property(objref, prop, descref, heap)?;
    Ok(Interpreted::from(objref))
}

fn define_properties(objref: JSRef, descs_ref: JSRef, heap: &mut Heap) -> Result<(), Exception> {
    let mut pairs: Vec<(String, JSRef)> = (heap.get(descs_ref).properties.iter())
        .map(|(prop, desc)| {
            let descref = match desc.to_ref() {
                Some(descref) => descref,
                None => {
                    let value = desc.content.to_value()?;
                    let value = Interpreted::Value(value);
                    return Err(Exception::TypeErrorInvalidDescriptor(value));
                }
            };
            Ok((prop.clone(), descref))
        })
        .collect::<Result<_, Exception>>()?;

    while let Some((prop, descref)) = pairs.pop() {
        define_property(objref, prop, descref, heap)?;
    }
    Ok(())
}

#[allow(non_snake_case)]
fn object_object_defineProperties(
    _this_ref: JSRef,
    _method_name: String,
    arguments: Vec<Interpreted>,
    heap: &mut Heap,
) -> Result<Interpreted, Exception> {
    let obj_arg = arguments.get(0).unwrap_or(&Interpreted::VOID);
    let objref = (obj_arg.to_ref(heap))
        .map_err(|_| Exception::TypeErrorInstanceRequired(obj_arg.clone(), "Object".to_string()))?;

    let desc_arg = arguments.get(1).unwrap_or(&Interpreted::VOID);
    let descs_ref = (desc_arg.to_ref(heap))
        .map_err(|_| Exception::TypeErrorInstanceRequired(obj_arg.clone(), "Object".to_string()))?;

    define_properties(objref, descs_ref, heap)?;
    Ok(Interpreted::from(objref))
}

pub fn init(heap: &mut Heap) -> Result<JSRef, Exception> {
    /* Object.prototype */
    let mut object_proto = JSObject::new();
    object_proto.proto = Heap::NULL;

    object_proto.set_system("dbg", Content::from_func(object_proto_dbg, heap))?;
    object_proto.set_hidden(
        "hasOwnProperty",
        Content::from_func(object_proto_hasOwnProperty, heap),
    )?;
    object_proto.set_hidden("toString", Content::from_func(object_proto_toString, heap))?;
    object_proto.set_hidden("valueOf", Content::from_func(object_proto_valueOf, heap))?;

    *heap.get_mut(Heap::OBJECT_PROTO) = object_proto;

    /* the Object */
    let mut object_object = JSObject::from_func(object_constructor);

    object_object.set_system("prototype", Content::from(Heap::OBJECT_PROTO))?;

    object_object.set_hidden("create", Content::from_func(object_object_create, heap))?;
    object_object.set_hidden("is", Content::from_func(object_object_is, heap))?;
    object_object.set_hidden(
        "defineProperty",
        Content::from_func(object_object_defineProperty, heap),
    )?;
    object_object.set_hidden(
        "defineProperties",
        Content::from_func(object_object_defineProperties, heap),
    )?;
    object_object.set_hidden(
        "getOwnPropertyDescriptor",
        Content::from_func(object_object_getOwnPropertyDescriptor, heap),
    )?;

    let the_object_ref = heap.alloc(object_object);
    heap.get_mut(Heap::OBJECT_PROTO)
        .set_hidden("constructor", Content::from(the_object_ref))?;

    Ok(the_object_ref)
}
