//! Test suite for the Web and headless browsers.

#![cfg(target_arch = "wasm32")]

extern crate wasm_bindgen_test;
use wasm_bindgen_test::*;
wasm_bindgen_test_configure!(run_in_browser);

use wasm_bindgen::JsValue;

use slothjs::{
    self as sljs,
    ToESTree,
};

#[wasm_bindgen_test]
fn test_interpret_string() {
    use slothjs::ast::expr;

    let two_plus_two = expr::add(expr::lit(2), expr::lit(2));
    let two_plus_two = sljs::Program::from_stmt(two_plus_two).to_estree();
    let two_plus_two = two_plus_two.to_string();
    assert_eq!(
        slothjs_wasm::interpret_string(&two_plus_two),
        Ok(JsValue::from(4)),
    );
}

#[wasm_bindgen_test]
fn test_interpret() {
    #[rustfmt::skip]
    use slothjs::ast::{ expr, stmt };

    let var_x = stmt::var([("x", expr::lit(12))].iter());
    let var_x = sljs::Program::from_stmt(var_x).to_estree();

    let x_plus = expr::add(expr::id("x"), expr::lit(8));
    let x_plus = sljs::Program::from_stmt(x_plus).to_estree();

    let var_x = JsValue::from_serde(&var_x).unwrap();
    slothjs_wasm::interpret(&var_x).unwrap();

    let x_plus = JsValue::from_serde(&x_plus).unwrap();
    assert_eq!(slothjs_wasm::interpret(&x_plus), Ok(JsValue::from(20)));
}
