use std::io::Write;
use std::process::{Command, Stdio};
use std::convert::TryFrom;

use serde_json::json;

use crate::ast::Program;
use crate::value::JSValue;
use crate::error::{Exception, ParseError};
use crate::interpret::{Interpretable, RuntimeState};

const ESPARSE: &str = "./node_modules/esprima/bin/esparse.js";

fn run_interpreter(input: &str) -> Result<JSValue, Exception> {
    let mut child = Command::new(ESPARSE)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .expect("cannot run: node index.js");

    let stdin = child.stdin.as_mut()
        .expect("failed to open stdin");
    stdin.write_all(input.as_bytes())
        .expect("failed to write stdin");
    std::mem::drop(stdin);  // flush

    let output = child.wait_with_output()
        .expect("failed to read stdout");
    assert!(output.status.success());

    let out = std::str::from_utf8(&output.stdout)
        .unwrap_or("");
    let json = serde_json::from_str(out)
        .map_err(|err| Exception::SyntaxError(ParseError::InvalidJSON{ err }))?;
    let program = Program::try_from(&json)
        .map_err(|e| Exception::SyntaxError(e))?;
    let mut state = RuntimeState::new();
    program.interpret(&mut state)
}

fn eval(input: &str) -> JSValue {
    run_interpreter(input).unwrap()
}

fn evalbool(input: &str) -> bool {
    eval(input).0.as_bool().unwrap()
}


#[test]
fn test_literals() {
    assert_eq!( eval("null"),       JSValue::UNDEFINED);
    assert_eq!( eval("true"),       JSValue::from(json!(true)));
    assert_eq!( eval("42"),         JSValue::from(42));
    assert_eq!( eval("[]"),         JSValue::from(json!([])));
    //assert_eq!( eval("+5"),         JSValue::from(5));
    //assert_eq!( eval("+'5'"),       JSValue::from(5));

    assert_eq!(
        eval("\"hello \\\"world\\\"\""),
        JSValue::from("hello \"world\"".to_string())
    );

    assert_eq!(
        eval("var a = {one:1, two:2}; a"),
        JSValue::from(json!({"one": 1, "two": 2}))
    );

    assert_eq!(
        eval("let x = 'one'; let o = {[x]: 1}; o.one"),
        JSValue::from(1)
    );

    assert_eq!( eval("var undefined = 5; undefined"), JSValue::UNDEFINED );
    assert_eq!( eval("var NaN = 5; NaN"), JSValue::from(5) );
}

#[test]
fn test_binary_operations() {
    // O_o
    assert_eq!( eval("2 + 2"),          JSValue::from(4.0) );
    assert_eq!( eval("'1' + '2'"),      JSValue::from("12") );
    assert_eq!( eval("[1] + [2,3]"),    JSValue::from("12,3") );
    assert_eq!( eval("[1,2] + null"),   JSValue::from("1,2null") );
    assert_eq!( eval("null + null"),    JSValue::from(0.0) );
    assert_eq!( eval("true + null"),    JSValue::from(1.0) );

    // o_O
    assert!( evalbool("2 == 2") );
    assert!( !evalbool("2 == 3") );
    //assert!( evalbool("'2' == 2") );
    assert!( !evalbool("0 == null") );
    //assert!( evalbool("0 == false") );
    //assert!( evalbool("0 == []") );
    //assert!( !evalbool("[] == []") );
    //assert!( !evalbool("0 == {}") );
    assert!( evalbool("null == null") );
    assert!( evalbool("null == undefined") );
    //assert!( !evalbool("NaN == NaN") );

    assert!( !evalbool("'a' < 'a'") );
    assert!( evalbool("1 < 2") );
    assert!( !evalbool("'113' < 13") );
    assert!( evalbool("'0' < '00'") );
    assert!( !evalbool("'0' < 0") );
    assert!( evalbool("'a' < 'b'") );
    assert!( !evalbool("'aa' < 'a'") );
    assert!( evalbool("null < 1") );
    //assert!( !evalbool("undefined < 1") );
    //assert!( evalbool("[1, 1] < [2]") );
    //assert!( !evalbool("NaN < 3") );
    //assert!( !evalbool("NaN < NaN") );
    //assert!( !evalbool("undefined < NaN") );
    //assert!( !evalbool("undefined < 1") );

    //assert!( !evalbool("undefined <= undefined") );
}

#[test]
fn test_member_expression() {
    assert_eq!( eval("['zero', 'one', 'two'][2]"),  JSValue::from("two"));
    assert_eq!( eval("let o = {one: 1}; o.one"),    JSValue::from(1));
    assert_eq!( eval("let o = {'o e': 1}; o['o e']"),    JSValue::from(1));
    assert_eq!(
        eval("let x = 'one'; let o = {[x]: 1}; o"),
        JSValue(json!({"one": 1}))
    );
}

#[test]
fn test_assignment() {
    assert_eq!( eval("var a = 1; a = 2; a"),            JSValue::from(2));
    //assert_eq!( eval("var a = [1]; a[0] = 2; a[0]"),    JSValue::from(2));
    //assert_eq!( eval("var a = {v: 1}; a.v = 2; a.v"),   JSValue::from(2));
}

#[test]
fn test_blocks() {
    assert_eq!( eval(r#"
        var a = 1, b = 2;
        { a = 10; b = 20 };
        a + b
    "#), JSValue::from(30.0));
}

#[test]
fn test_conditionals() {
    assert!( evalbool("'0' ? true : false"));
    assert!( !evalbool("0 ? true : false"));

    assert_eq!( eval("var a; if (a = 1) a = 2; else a = 3; a"), JSValue::from(2) );
    assert_eq!( eval("var a = 1; if (null) { a = 2; }; a"), JSValue::from(1) );
}

#[test]
fn test_for_statements() {
    assert_eq!( eval(r#"
        var a = 0;
        for (var i = 0; i < 5; i = i + 1) {
            a = a + 1;
        }
        a
    "#), JSValue::from(5.0));

    assert_eq!( eval(r#"
        let a = 0;
        for (; 0; ) a = 1;
        a
    "#), JSValue::from(0));
}

#[test] fn test_scratch() { }
