use crate::prelude::*;
use crate::{
    object::Access,
    CallContext,
    Exception,
    Heap,
    Interpreted,
    JSObject,
    JSRef,
    JSResult,
    JSValue,
};

fn object_reference(arg: &Interpreted, heap: &Heap) -> JSResult<JSRef> {
    arg.to_ref(heap)
        .or_else(|_| heap.throw(Exception::instance_required(arg, "Object")))
}

fn object_constructor(call: CallContext, heap: &mut Heap) -> JSResult<Interpreted> {
    let argument = call.arguments.get(0).unwrap_or(&Interpreted::VOID);
    let value = argument.to_value(heap)?;
    let object_ref = match value.objectify(heap) {
        Heap::NULL => heap.alloc(JSObject::new()),
        href => href,
    };
    Ok(Interpreted::from(object_ref))
}

#[allow(non_snake_case)]
fn object_proto_hasOwnProperty(call: CallContext, heap: &mut Heap) -> JSResult<Interpreted> {
    let argument = call.arguments.get(0).unwrap_or(&Interpreted::VOID);
    let propname = argument.to_value(heap)?.stringify(heap)?;
    let this_object = heap.get(call.this_ref);
    let found = this_object.get_own_value(&propname).is_some(); // TODO: avoid calling getters, if any
    Ok(Interpreted::from(found))
}

#[allow(non_snake_case)]
fn object_proto_toString(_call: CallContext, _heap: &mut Heap) -> JSResult<Interpreted> {
    Ok(Interpreted::from("[object Object]"))
}

#[cfg(feature = "std")]
fn object_proto_dbg(call: CallContext, heap: &mut Heap) -> JSResult<Interpreted> {
    dbg!(call.this_ref);
    dbg!(heap.get(call.this_ref));
    Ok(Interpreted::VOID)
}

#[allow(non_snake_case)]
fn object_proto_valueOf(call: CallContext, _heap: &mut Heap) -> JSResult<Interpreted> {
    // primitive wrappers are handled by their own `.valueOf()`
    Ok(Interpreted::from(call.this_ref))
}

fn object_object_create(call: CallContext, heap: &mut Heap) -> JSResult<Interpreted> {
    let proto =
        (call.arguments.get(0)).ok_or(Exception::TypeErrorInvalidPrototype(Interpreted::VOID))?;
    let protoref =
        (proto.to_ref(heap)).map_err(|_| Exception::TypeErrorInvalidPrototype(proto.clone()))?;

    let properties: Option<JSRef> =
        (call.arguments.get(1)).and_then(|props| props.to_ref(heap).ok());

    let mut object = JSObject::new();
    object.proto = protoref;

    let objref = heap.alloc(object);

    if let Some(property_descriptions) = properties {
        define_properties(objref, property_descriptions, heap)?;
    }

    Ok(Interpreted::from(objref))
}

fn object_object_is(call: CallContext, heap: &mut Heap) -> JSResult<Interpreted> {
    use JSValue::*;

    let left = call.arguments.get(0).unwrap_or(&Interpreted::VOID);
    let right = call.arguments.get(1).unwrap_or(&Interpreted::VOID);

    let answer = match (left.to_value(heap), right.to_value(heap)) {
        (Ok(Undefined), Ok(Undefined)) => true,
        (Ok(String(lstr)), Ok(String(rstr))) if lstr == rstr => true,
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
    call: CallContext,
    heap: &mut Heap,
) -> JSResult<Interpreted> {
    let inspected = call.arguments.get(0).unwrap_or(&Interpreted::VOID);
    let inspected_ref =
        (inspected.to_ref(heap)).map_err(|_| Exception::ReferenceNotAnObject(inspected.clone()))?;

    let propname = (call.arguments.get(1).unwrap_or(&Interpreted::VOID))
        .to_value(&*heap)?
        .stringify(heap)?;

    let inspected_object = heap.get(inspected_ref);
    let prop = match inspected_object.properties.get(&propname) {
        Some(prop) => prop.clone(),
        None => return Ok(Interpreted::VOID),
    };

    let value = prop.content.to_value()?;

    let mut descriptor_object = JSObject::new();
    descriptor_object.set_property("value", value)?;
    descriptor_object.set_property("configurable", prop.access.configurable())?;
    descriptor_object.set_property("enumerable", prop.access.enumerable())?;
    descriptor_object.set_property("writable", prop.access.writable())?;

    let descriptor_ref = heap.alloc(descriptor_object);
    Ok(Interpreted::from(descriptor_ref))
}

fn define_property(
    objref: JSRef,
    propname: String,
    descref: JSRef,
    heap: &mut Heap,
) -> Result<(), Exception> {
    let get_value =
        |object: &JSObject, name: &str| object.get_own_value(name).unwrap_or(JSValue::Undefined);
    let get_bool = |object: &JSObject, name: &str| get_value(object, name).boolify(heap);

    let descriptor = heap.get(descref);

    let configurable = get_bool(descriptor, "configurable");
    let enumerable = get_bool(descriptor, "enumerable");

    let has_writable = descriptor.get_own_value("writable").is_some();
    let has_value = descriptor.get_own_value("value").is_some();
    let has_get = descriptor.get_own_value("get").is_some();
    let has_set = descriptor.get_own_value("set").is_some();
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

        let object = heap.get_mut(objref);
        object.define_own_property(&propname, access)?;
        object.set_even_nonwritable(&propname, value)?;
    }
    Ok(())
}

#[allow(non_snake_case)]
fn object_object_defineProperty(call: CallContext, heap: &mut Heap) -> JSResult<Interpreted> {
    let objarg = call.arguments.get(0).unwrap_or(&Interpreted::VOID);
    let objref = object_reference(objarg, heap)?;

    let prop = (call.arguments.get(1).unwrap_or(&Interpreted::VOID))
        .to_value(heap)?
        .stringify(heap)?;

    let descref = (call.arguments.get(2).unwrap_or(&Interpreted::VOID)).to_ref(heap)?;

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
        .collect::<JSResult<_>>()?;

    while let Some((prop, descref)) = pairs.pop() {
        define_property(objref, prop, descref, heap)?;
    }
    Ok(())
}

#[allow(non_snake_case)]
fn object_object_defineProperties(call: CallContext, heap: &mut Heap) -> JSResult<Interpreted> {
    let obj_arg = call.arguments.get(0).unwrap_or(&Interpreted::VOID);
    let objref = object_reference(obj_arg, heap)?;

    let desc_arg = call.arguments.get(1).unwrap_or(&Interpreted::VOID);
    let descs_ref = object_reference(desc_arg, heap)?;

    define_properties(objref, descs_ref, heap)?;
    Ok(Interpreted::from(objref))
}

#[allow(non_snake_case)]
fn object_object_setPrototypeOf(call: CallContext, heap: &mut Heap) -> JSResult<Interpreted> {
    let obj_arg = call.arguments.get(0).unwrap_or(&Interpreted::VOID);
    let objref = object_reference(obj_arg, heap)?;

    let proto_arg = call.arguments.get(1).unwrap_or(&Interpreted::VOID);
    if let Ok(protoref) = proto_arg.to_ref(heap) {
        let object = heap.get_mut(objref);
        object.proto = protoref;
    }

    Ok(Interpreted::from(objref))
}

pub fn init(heap: &mut Heap) -> JSResult<JSRef> {
    /* Object.prototype */
    let mut object_proto = JSObject::new();
    object_proto.proto = Heap::NULL;

    #[cfg(feature = "std")]
    object_proto.set_system("dbg", heap.alloc_func(object_proto_dbg))?;

    object_proto.set_hidden(
        "hasOwnProperty",
        heap.alloc_func(object_proto_hasOwnProperty),
    )?;
    object_proto.set_hidden("toString", heap.alloc_func(object_proto_toString))?;
    object_proto.set_hidden("valueOf", heap.alloc_func(object_proto_valueOf))?;

    *heap.get_mut(Heap::OBJECT_PROTO) = object_proto;

    /* the Object */
    let mut object_object = JSObject::from_func(object_constructor);

    object_object.set_system("prototype", Heap::OBJECT_PROTO)?;

    object_object.set_hidden("create", heap.alloc_func(object_object_create))?;
    object_object.set_hidden("is", heap.alloc_func(object_object_is))?;
    object_object.set_hidden(
        "defineProperty",
        heap.alloc_func(object_object_defineProperty),
    )?;
    object_object.set_hidden(
        "defineProperties",
        heap.alloc_func(object_object_defineProperties),
    )?;
    object_object.set_hidden(
        "getOwnPropertyDescriptor",
        heap.alloc_func(object_object_getOwnPropertyDescriptor),
    )?;
    object_object.set_hidden(
        "setPrototypeOf",
        heap.alloc_func(object_object_setPrototypeOf),
    )?;

    let the_object_ref = heap.alloc(object_object);
    heap.get_mut(Heap::OBJECT_PROTO)
        .set_hidden("constructor", the_object_ref)?;

    Ok(the_object_ref)
}
