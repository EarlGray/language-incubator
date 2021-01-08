/// The implementation of the Object object

//use crate::object;
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

fn object_proto_dbg(
    this_ref: JSRef,
    _method_name: String,
    _arguments: Vec<Interpreted>,
    heap: &mut Heap
) -> Result<Interpreted, Exception> {
    dbg!(this_ref);
    dbg!(heap.object(this_ref)?);
    Ok(Interpreted::VOID)
}

fn init_object_prototype(heap: &mut Heap) -> JSRef {
    /* Object.prototype */
    let mut object_proto = JSObject::new();
    object_proto.set_property_and_flags(
        "toString",
        Content::NativeFunction(object_proto_toString),
        Access::HIDDEN,
    );
    object_proto.set_property_and_flags(
        "dbg",
        Content::NativeFunction(object_proto_dbg),
        Access::ALL ^ Access::CONF
    );
    object_proto.set_property_and_flags(
        "__proto__",
        Content::Data(Heap::NULL),
        Access::NONE
    );

    heap.allocate(JSValue::Object(object_proto))
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

    let configurable = heap.allocate(JSValue::from(prop.access.configurable()));
    let enumerable = heap.allocate(JSValue::from(prop.access.enumerable()));
    let writable = heap.allocate(JSValue::from(prop.access.writable()));

    let mut descriptor_object = JSObject::new();
    descriptor_object.set_property_ref("configurable", configurable);
    descriptor_object.set_property_ref("enumerable", enumerable);
    descriptor_object.set_property_ref("writable", writable);
    let dataref = match prop.content {
        Content::Data(dataref) => dataref,
        Content::NativeFunction(_func) => {
            heap.allocate(JSValue::from("[[native]]"))
        }
        Content::Closure(closure) => {
            let repr = format!("{:?}", closure);
            heap.allocate(JSValue::from(repr))
        }
    };
    descriptor_object.set_property_ref("value", dataref);

    let descriptor = JSValue::Object(descriptor_object);
    Ok(Interpreted::Value(descriptor))
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

    let object_ref = heap.allocate(JSValue::Object(object_object));

    heap.object_mut(object_proto_ref)?.set_property_and_flags(
        "constructor",
        Content::Data(object_ref),
        Access::HIDDEN
    );

    heap.global_mut().set_property_and_flags(
        "Object",
        Content::Data(object_ref),
        Access::HIDDEN
    );
    Ok(())
}
