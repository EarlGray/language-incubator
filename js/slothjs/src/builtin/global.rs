use crate::error::Exception;
use crate::object::{
    Content,
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
    arguments: Vec<Interpreted>,
    heap: &mut Heap
) -> Result<Interpreted, Exception> {
    let argument = arguments.get(0).unwrap_or(&Interpreted::VOID);
    let value = argument.to_value(heap)?;
    if let JSValue::Number(_) = value {
        return Ok(Interpreted::Value(value))
    }

    let value = value.stringify(heap);

    let radix = arguments.get(1).unwrap_or(&Interpreted::VOID);
    let radix = radix.to_value(heap)?.numberify();
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
fn make_readonly_property(heap: &mut Heap, name: &str, value: JSValue) {
    let valref = heap.allocate(value);
    heap.global_mut().set_readonly(name, Content::Data(valref));
}

pub fn init(heap: &mut Heap) -> Result<(), Exception> {
    make_readonly_property(heap, "NaN", JSValue::Number(f64::NAN));
    make_readonly_property(heap, "undefined", JSValue::Undefined);

    heap.global_mut().set_hidden(
        "parseInt",
        Content::NativeFunction(parse_int),
    );

    // The `global` self-reference:
    heap.global_mut().set_hidden(
        "global",
        Content::Data(Heap::GLOBAL),
    );

    Ok(())
}
