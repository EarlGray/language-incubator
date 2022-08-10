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
    object::HostClass,
};

pub static CLASS: HostClass = HostClass {
    name: "Object",
    constructor: object_constructor,
    methods: &[
        ("hasOwnProperty",      object_proto_hasOwnProperty),
        ("toString",            object_proto_toString),
        ("valueOf",             object_proto_valueOf),
    ],
    static_methods: &[
        ("create",                      object_object_create),
        ("defineProperties",            object_object_defineProperties),
        ("defineProperty",              object_object_defineProperty),
        ("getOwnPropertyDescriptor",    object_object_getOwnPropertyDescriptor),
        ("is",                          object_object_is),
        ("setPrototypeOf",              object_object_setPrototypeOf),
    ],
};

fn object_constructor(call: CallContext, heap: &mut Heap) -> JSResult<Interpreted> {
    let value = call.arg_value(0, heap)?;
    let object_ref = match value.objectify(heap) {
        Heap::NULL => heap.alloc(JSObject::new()),
        href => href,
    };
    Ok(Interpreted::from(object_ref))
}

#[allow(non_snake_case)]
fn object_proto_hasOwnProperty(call: CallContext, heap: &mut Heap) -> JSResult<Interpreted> {
    let propname = call.arg_value(0, heap)?.stringify(heap)?;
    // TODO: avoid calling getters, if any
    let found = heap.get(call.this_ref).get_own_value(&propname).is_some();
    Ok(Interpreted::from(found))
}

#[allow(non_snake_case)]
fn object_proto_toString(_call: CallContext, _heap: &mut Heap) -> JSResult<Interpreted> {
    Ok(Interpreted::from("[object Object]"))
}

#[cfg(feature = "std")]
pub fn object_proto_dbg(call: CallContext, heap: &mut Heap) -> JSResult<Interpreted> {
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
    let proto = (call.arg_value(0, heap))
        .map_err(|_| Exception::TypeErrorInvalidPrototype(Interpreted::VOID))?;
    let protoref = (proto.to_ref())
        .map_err(|_| Exception::TypeErrorInvalidPrototype(Interpreted::from(proto)))?;

    let properties: Option<JSRef> = call.arg_value(1, heap).and_then(|val| val.to_ref()).ok();

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

    let left = call.arg_value(0, heap)?;
    let right = call.arg_value(1, heap)?;

    let answer = match (left, right) {
        (Undefined, Undefined) => true,
        (String(lstr), String(rstr)) if lstr == rstr => true,
        (Bool(lb), Bool(rb)) if lb == rb => true,
        (Number(lnum), Number(rnum)) => {
            if f64::abs(lnum) == 0.0 && f64::abs(rnum) == 0.0 {
                f64::is_sign_positive(lnum) == f64::is_sign_positive(rnum)
            } else {
                (f64::is_nan(lnum) && f64::is_nan(rnum)) || lnum == rnum
            }
        }
        (Ref(lref), Ref(rref)) => lref == rref,
        _ => false,
    };
    Ok(Interpreted::from(answer))
}

#[allow(non_snake_case)]
fn object_object_getOwnPropertyDescriptor(
    call: CallContext,
    heap: &mut Heap,
) -> JSResult<Interpreted> {
    let inspected = call.arg_value(0, heap)?;
    let inspected_ref = (inspected.to_ref())
        .map_err(|_| Exception::ReferenceNotAnObject(Interpreted::from(inspected)))?;

    let propname = call.arg_value(1, heap)?.stringify(heap)?;

    let inspected_object = heap.get(inspected_ref);
    let prop = match inspected_object.properties.get(propname.as_str()) {
        Some(prop) => prop.clone(),
        None => return Ok(Interpreted::VOID),
    };

    let value = prop.content.to_value()?;

    let mut descriptor_object = JSObject::new();
    descriptor_object.set_property("value".into(), value)?;
    descriptor_object.set_property("configurable".into(), prop.access.configurable())?;
    descriptor_object.set_property("enumerable".into(), prop.access.enumerable())?;
    descriptor_object.set_property("writable".into(), prop.access.writable())?;

    let descriptor_ref = heap.alloc(descriptor_object);
    Ok(Interpreted::from(descriptor_ref))
}

fn define_property(objref: JSRef, propname: JSString, descref: JSRef, heap: &mut Heap) -> JSResult<()> {
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
        object.define_own_property(propname.clone(), access)?;
        object.set_even_nonwritable(propname, value)?;
    }
    Ok(())
}

#[allow(non_snake_case)]
fn object_object_defineProperty(call: CallContext, heap: &mut Heap) -> JSResult<Interpreted> {
    let objref = call.arg_value(0, heap)?.to_ref()?;
    let prop = call.arg_value(1, heap)?.stringify(heap)?;
    let descref = call.arg_value(2, heap)?.to_ref()?;

    define_property(objref, prop, descref, heap)?;
    Ok(Interpreted::from(objref))
}

fn define_properties(objref: JSRef, descs_ref: JSRef, heap: &mut Heap) -> JSResult<()> {
    let mut pairs: Vec<(JSString, JSRef)> = (heap.get(descs_ref).properties.iter())
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
    let objref = call.arg_value(0, heap)?.to_ref()?;
    let descs_ref = call.arg_value(1, heap)?.to_ref()?;

    define_properties(objref, descs_ref, heap)?;
    Ok(Interpreted::from(objref))
}

#[allow(non_snake_case)]
fn object_object_setPrototypeOf(call: CallContext, heap: &mut Heap) -> JSResult<Interpreted> {
    let objref = call.arg_value(0, heap)?.to_ref()?;

    let proto_arg = call.arg_value(1, heap)?;
    if let Ok(protoref) = proto_arg.to_ref() {
        let object = heap.get_mut(objref);
        object.proto = protoref;
    }

    Ok(Interpreted::from(objref))
}
