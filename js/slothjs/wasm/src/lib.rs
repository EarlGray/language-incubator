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

#[wasm_bindgen]
pub fn interpret(json_ast: &str) -> Result<JsValue, JsValue> {
    let json = serde_json::from_str::<JSON>(json_ast)
        .map_err(|e| JsValue::from(format!("{}", e)))?;
    let program = Program::parse_from(&json)
        .map_err(|e| JsValue::from(format!("{:?}", e)))?;

    let mut heap = Heap::new();
    let result = program.interpret(&mut heap)
        .map_err(|e| JsValue::from(format!("{:?}", e)))?
        .to_value(&heap)
        .map_err(|e| JsValue::from(format!("{:?}", e)))?;
    Ok(match result {
        slothjs::JSValue::Undefined => JsValue::NULL,
        slothjs::JSValue::Number(n) => JsValue::from(n),
        slothjs::JSValue::Bool(b) => JsValue::from(b),
        slothjs::JSValue::String(s) => JsValue::from(s),
        _ => todo!(),
    })
}
