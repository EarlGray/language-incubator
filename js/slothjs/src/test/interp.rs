use serde_json::json;

use crate::ast::builder::expr;

use super::assert_eval;
//use super::assert_exception;

#[test]
fn binary_addition() {
    assert_eval!( expr::add(2, 2),      4.0);
    assert_eval!( expr::add("1", "2"),  "12");
    assert_eval!( expr::add(json!([1]), json!([2, 3])), "12,3");
    assert_eval!( expr::add(json!([1,2]), json!(null)), "1,2null");
    assert_eval!( expr::add(json!(null), json!(null)),    0.0);
    /*
    assert_eval!("true + null",    1.0);
    assert_eval!( "'' + [1, 2]",   "1,2");
    assert_eval!( "'' + null",     "null");
    assert_eval!( "'' + true",     "true");
    assert_eval!( "'' + {}",       "[object Object]");
    assert_eval!( "({} + {})",     "[object Object][object Object]" );
    assert_eval!( "({} + [])",     "[object Object]"); // expression
    assert_eval!( "{} +[]",         0.0 );             // two statements
    //assert_eval!("undefined + undefined",  (f64::NAN));
    //assert_eval!("5 + undefined",  (f64::NAN));
    assert_eval!("undefined + 5",  "undefined5");
    assert_eval!("1 + {}",         "1[object Object]");
    */
}

