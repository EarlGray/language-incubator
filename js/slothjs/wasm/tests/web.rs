//! Test suite for the Web and headless browsers.

#![cfg(target_arch = "wasm32")]

extern crate wasm_bindgen_test;
use wasm_bindgen_test::*;
wasm_bindgen_test_configure!(run_in_browser);

use wasm_bindgen::JsValue;

use slothjs_wasm;


#[wasm_bindgen_test]
fn test_eval() {
    assert_eq!(Ok(JsValue::from(4)), slothjs_wasm::interpret_string(r#"{
      "type": "Program",
      "body": [
        {
          "type": "ExpressionStatement",
          "expression": {
            "type": "BinaryExpression",
            "operator": "+",
            "left": { "type": "Literal", "value": 2, "raw": "2" },
            "right": { "type": "Literal", "value": 2, "raw": "2" }
          }
        }
      ],
      "sourceType": "script"
    }"#));
}
