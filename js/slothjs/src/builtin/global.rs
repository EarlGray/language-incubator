use crate::prelude::*;
use crate::{CallContext, Heap, Interpreted, JSObject, JSResult, JSValue};

/*
 *  parseInt
 */
#[allow(clippy::manual_range_contains)]
fn parse_int(call: CallContext, heap: &mut Heap) -> JSResult<Interpreted> {
    let argument = call.arguments.first().unwrap_or(&Interpreted::VOID);
    let value = argument.to_value(heap)?;
    if let JSValue::Number(_) = value {
        return Ok(Interpreted::Value(value));
    }
    let value = value.stringify(heap)?;

    let radix = call.arguments.get(1).unwrap_or(&Interpreted::VOID);
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

#[allow(non_snake_case)]
fn global_parseFloat(call: CallContext, heap: &mut Heap) -> JSResult<Interpreted> {
    let argument = call.arguments.first().unwrap_or(&Interpreted::VOID);
    let value = argument.to_value(heap)?.stringify(heap)?;

    // TODO: proper implementation
    let result = f64::from_str(&value).unwrap_or(f64::NAN);
    Ok(Interpreted::from(result))
}

/*
 *  init
 */

pub fn init(heap: &mut Heap) -> JSResult<()> {
    let mut global = JSObject::new();

    global.set_system("NaN", f64::NAN)?;
    global.set_system("undefined", JSValue::Undefined)?;

    global.set_hidden("global", Heap::GLOBAL)?;
    global.set_system(Heap::SCOPE_THIS, Heap::GLOBAL)?;

    global.set_hidden("parseInt", heap.alloc_func(parse_int))?;
    global.set_hidden("parseFloat", heap.alloc_func(global_parseFloat))?;

    *heap.get_mut(Heap::GLOBAL) = global;

    Ok(())
}
