use crate::ast::builder::expr;

use super::assert_eval;
//use super::assert_exception;

#[test]
fn binary_addition() {
    assert_eval!(4.0, expr::add(2, 2));
    assert_eval!("12", expr::add("1", "2"));
    assert_eval!(
        "12,3",
        expr::add(expr::array(vec![1]), expr::array(vec![2, 3]))
    );
    assert_eval!("1,2null", expr::add(expr::array(vec![1, 2]), expr::null()));
    assert_eval!(0.0, expr::add(expr::null(), expr::null()));
    assert_eval!(1.0, expr::add(true, expr::null()));
    assert_eval!("1,2", expr::add("", expr::array(vec![1, 2])));
    assert_eval!("null", expr::add("", expr::null()));
    assert_eval!("true", expr::add("", true));
    assert_eval!(
        "[object Object]",
        expr::add("", expr::empty_object()),
        "js: '' + {} "
    );
    assert_eval!(
        "[object Object][object Object]",
        expr::add(expr::empty_object(), expr::empty_object()),
        "js: {} + {} "
    );
    assert_eval!(
        "[object Object]",
        expr::add(expr::empty_object(), expr::empty_array()),
        "js: {} + [] "
    );
    assert_eval!("undefined5", expr::add(expr::undefined(), "5"));
    assert_eval!("1[object Object]", expr::add(1, expr::empty_object()));
    //assert_eval!((f64::NAN),   expr::add(expr::undefined(), expr::undefined()));
    //assert_eval!((f64::NAN),  expr::add("5", expr::undefined())),  );
}
