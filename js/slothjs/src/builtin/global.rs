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
pub fn init(heap: &mut Heap) -> Result<(), Exception> {
    // TODO: detect circular references in JSValue::to_string()
    //   to avoid stack overflow on trying to display `global` in REPL.
    // The `global` self-reference:
    //global_object.set_property("global", Heap::GLOBAL);

    // parseInt:
    let parse_int_prop = object::Property { 
        configurable: false,
        writable: false,
        enumerable: true,
        content: object::Content::NativeFunction(parse_int),
    };
    let global_object = heap.get_mut(Heap::GLOBAL).to_object_mut()?;
    global_object.properties.insert("parseInt".to_string(), parse_int_prop);

    /* NaN
    let nan = Interpreted::Value(JSValue::Number(f64::NAN));
    heap.property_assign(Heap::GLOBAL, "NaN", &nan)?;
    */

    Ok(())
}
