use crate::error::Exception;
use crate::object::{
    Interpreted,
    Heap,
    JSObject,
    JSValue,
};

pub fn init(heap: &mut Heap) -> Result<(), Exception> {
    let object_object = JSObject::new();

    //object_object.set_property("keys", );
    //object_object.set_property("values", );
    //object_object.set_property("prototype", );

    let object_value = JSValue::Object(object_object);
    let wrapped_object = Interpreted::Value(object_value);
    heap.property_assign(Heap::GLOBAL, "Object", &wrapped_object)
}
