use core::cell::RefCell;
use core::fmt;
use slothjs::{
    Heap,
    Program,
    //Interpretable,
    JSON,
};

use wasm_bindgen::prelude::*;

#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

thread_local! {
    static HEAP: RefCell<Heap> = RefCell::new(Heap::new());
}

#[wasm_bindgen]
extern "C" {
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
pub fn interpret_string(json_ast: &str) -> Result<JsValue, JsValue> {
    let json: JSON = serde_json::from_str(json_ast).map_err(jserror)?;
    let program = Program::parse_from(&json).map_err(jserror)?;

    let mut heap = Heap::new();
    let result = heap
        .evaluate(&program)
        .map_err(jserror)?
        // TODO: implement serde::Serializer directly for JSValue?
        .to_json(&heap)
        .map_err(jserror)?;
    JsValue::from_serde(&result).map_err(jserror)
}

#[wasm_bindgen]
pub fn interpret(jsobject: &JsValue) -> Result<JsValue, JsValue> {
    let json: JSON = jsobject.into_serde().map_err(jserror)?;
    let program = Program::parse_from(&json).map_err(jserror)?;
    let result = HEAP
        .with(|heapcell| {
            let mut heap = heapcell.borrow_mut();
            heap.evaluate(&program)?.to_string(&mut heap)
        })
        .map_err(jserror)?;
    JsValue::from_serde(result.as_str()).map_err(jserror)
}
