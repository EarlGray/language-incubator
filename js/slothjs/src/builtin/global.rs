use crate::error::Exception;
use crate::object;
use crate::object::{
    Content,
    Heap,
    Interpreted,
    JSRef,
    JSValue,
    PropertyFlags as Access
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
    let content = object::Content::Data(valref);
    heap.global_mut().set_property_and_flags(name, content, Access::READONLY);
}

pub fn init(heap: &mut Heap) -> Result<(), Exception> {
    make_readonly_property(heap, "NaN", JSValue::Number(f64::NAN));
    make_readonly_property(heap, "undefined", JSValue::Undefined);

    heap.global_mut().set_property_and_flags(
        "parseInt",
        Content::NativeFunction(parse_int),
        Access::HIDDEN
    );

    // TODO: detect circular references in JSValue::to_string()
    //   to avoid stack overflow on trying to display `global` in REPL.
    // The `global` self-reference:
    //global_object.set_property("global", Heap::GLOBAL);

    Ok(())
}
