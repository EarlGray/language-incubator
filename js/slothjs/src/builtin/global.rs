use crate::error::Exception;
use crate::object::{
    Content,
    Interpreted,
    JSObject,
    JSValue,
};
use crate::heap::{
    Heap,
    JSRef,
};

/*
 *  parseInt
 */
fn parse_int(
    _this_ref: JSRef,
    _method_name: String,
    arguments: Vec<Interpreted>,
    heap: &mut Heap
) -> Result<Interpreted, Exception> {
    let argument = arguments.get(0).unwrap_or(&Interpreted::VOID);
    let value = argument.to_value(heap)?;
    if let JSValue::Number(_) = value {
        return Ok(Interpreted::Value(value))
    }

    let value = value.stringify(heap)?;

    let radix = arguments.get(1).unwrap_or(&Interpreted::VOID);
    let radix = radix.to_value(heap)?.numberify(heap);
    let mut radix = radix.unwrap_or(0.0) as u32;
    if radix == 0 {
        let (c0, c1) = {
            let mut cs = value.chars();
            let c0 = cs.next().unwrap_or('\0');
            let c1 = cs.next().unwrap_or('\0');
            (c0, c1)
        };
        match (c0, c1) {
            ('0', 'x') | ('0', 'X') => radix = 16,
            ('0', _) => radix = 8,
            _ => radix = 10,
        }
    }
    if radix < 2 || radix > 36 {
        return Ok(Interpreted::NAN);
    }

    let result = i64::from_str_radix(&value, radix)
        .map(|num| num as f64)
        .unwrap_or(f64::NAN);
    Ok(Interpreted::from(result))
}

/*
 *  init
 */

pub fn init(heap: &mut Heap) -> Result<(), Exception> {
    let parse_int_ref = heap.alloc(JSObject::from_func(parse_int));

    let global = heap.get_mut(Heap::GLOBAL);

    global.set_readonly("NaN", Content::from(f64::NAN));
    global.set_readonly("undefined", Content::from(JSValue::Undefined));

    global.set_hidden("parseInt", Content::from(parse_int_ref));

    // The `global` self-reference:
    global.set_hidden("global", Content::from(Heap::GLOBAL));

    Ok(())
}
