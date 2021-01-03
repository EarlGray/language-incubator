use crate::error::Exception;
use crate::object;
use crate::object::{
    Heap,
    Interpreted,
    JSRef,
    JSValue,
};

/*
 *  parseInt
 */
fn parse_int(
    _this_ref: JSRef,
    _method_name: String,
    _arguments: Vec<Interpreted>,
    _heap: &mut Heap
) -> Result<Interpreted, Exception> {
    // TODO: this is not the ultimate question of life, the universe and everything.
    let value = JSValue::from(42.0);
    Ok(Interpreted::Value(value))
}

/*
 *  init
 */
fn make_readonly_property(heap: &mut Heap, name: &str, value: JSValue) {
    let valref = heap.allocate(value);
    let global_object = heap.get_mut(Heap::GLOBAL).to_object_mut().unwrap();
    global_object.set_property_ref(name, valref);
    global_object.properties.get_mut(name).unwrap().writable = false;
}

pub fn init(heap: &mut Heap) -> Result<(), Exception> {
    make_readonly_property(heap, "NaN", JSValue::Number(f64::NAN));
    make_readonly_property(heap, "undefined", JSValue::Undefined);

    let global_object = heap.get_mut(Heap::GLOBAL).to_object_mut()?;

    global_object.set_property("parseInt", object::Content::NativeFunction(parse_int));

    // TODO: detect circular references in JSValue::to_string()
    //   to avoid stack overflow on trying to display `global` in REPL.
    // The `global` self-reference:
    //global_object.set_property("global", Heap::GLOBAL);

    Ok(())
}
