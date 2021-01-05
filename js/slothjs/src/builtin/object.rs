/// The implementation of the Object object

use crate::object;
use crate::object::{
    Content,
    Interpreted,
    Heap,
    JSObject,
    JSRef,
    JSValue,
    PropertyFlags as Access,
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

    let inspected_object = heap.get_mut(inspected_ref).to_object()?;
    let prop = match inspected_object.properties.get(&propname) {
        Some(prop) => prop.clone(),
        None => return Ok(Interpreted::VOID)
    };

    let configurable = heap.allocate(JSValue::from(prop.configurable));
    let enumerable = heap.allocate(JSValue::from(prop.enumerable));
    let writable = heap.allocate(JSValue::from(prop.writable));

    let mut descriptor_object = JSObject::new();
    descriptor_object.set_property_ref("configurable", configurable);
    descriptor_object.set_property_ref("enumerable", enumerable);
    descriptor_object.set_property_ref("writable", writable);
    match prop.content {
        object::Content::Data(dataref) =>
            descriptor_object.set_property_ref("value", dataref),
        object::Content::NativeFunction(_func) => {
            let dataref = heap.allocate(JSValue::from("[[native]]"));
            descriptor_object.set_property_ref("value", dataref);
        }
    };

    let descriptor = JSValue::Object(descriptor_object);
    Ok(Interpreted::Value(descriptor))
}


fn init_object_prototype(heap: &mut Heap) -> JSRef {
    /* Object.prototype */
    let mut object_proto = JSObject::new();
    object_proto.set_property_and_flags(
        "toString",
        Content::NativeFunction(object_proto_toString),
        Access::HIDDEN,
    );

    heap.allocate(JSValue::Object(object_proto))
}

pub fn init(heap: &mut Heap) -> Result<(), Exception> {
    let object_proto_ref = init_object_prototype(heap);

    let mut object_object = JSObject::new();

    /* the Object */
    object_object.set_property_and_flags(
        "prototype",
        Content::Data(object_proto_ref),
        Access::NONE
    );
    object_object.set_property_and_flags(
        "getOwnPropertyDescriptor",
        Content::NativeFunction(object_object_getOwnPropertyDescriptor),
        Access::HIDDEN
    );

    let object_value = JSValue::Object(object_object);
    let wrapped_object = Interpreted::Value(object_value);
    heap.property_assign(Heap::GLOBAL, "Object", &wrapped_object)
}
