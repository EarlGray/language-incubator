use core::fmt;
use slothjs::{
    Interpretable,
    JSON,
    Heap,
    Program,
};

use wasm_bindgen::prelude::*;

#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[wasm_bindgen]
extern {
    fn alert(s: &str);
}

#[wasm_bindgen(start)]
pub fn run() {
    #[cfg(feature = "console_error_panic_hook")]
    console_error_panic_hook::set_once();
}

fn jserror<E: fmt::Debug>(e: E) -> JsValue {
    JsValue::from(format!("{:?}", e))
}

#[wasm_bindgen]
pub fn interpret(json_ast: &str) -> Result<JsValue, JsValue> {
    let json: JSON = serde_json::from_str(json_ast)
        .map_err(jserror)?;
    let program = Program::parse_from(&json)
        .map_err(jserror)?;

    let mut heap = Heap::new();
    let result = program.interpret(&mut heap).map_err(jserror)?
        .to_value(&heap).map_err(jserror)?
        // TODO: implement serde::Serializer directly for JSValue?
        .to_json(&heap).map_err(jserror)?;
    JsValue::from_serde(&result).map_err(jserror)
}
