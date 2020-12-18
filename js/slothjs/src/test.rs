use std::io::Write;
use std::process::{Command, Stdio};

use crate::*;

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
        .map_err(|_| Exception::SyntaxError)?;
    let program = Program::from_json(&json)
        .map_err(|_| Exception::SyntaxError)?;
    program.interpret()
}

fn eval(input: &str) -> JSValue {
    run_interpreter(input).unwrap()
}

#[test]
fn test_literals() {
    assert_eq!( eval("null"),       UNDEFINED);
    assert_eq!( eval("true"),       JSValue::from(json!(true)));
    assert_eq!( eval("42"),         JSValue::from(42));
    assert_eq!( eval("[]"),         JSValue::from(json!([])));

    assert_eq!(
        eval("\"hello \\\"world\\\"\""),
        JSValue::from("hello \"world\"".to_string())
    );

    assert_eq!(
        eval("var a = {one:1, two:2}; a"),
        JSValue::from(json!({"one": 1, "two": 2}))
    );
}

#[test]
fn test_binary_operations() {
    assert_eq!( eval("2 + 2"),      JSValue::from(4.0) );
    assert_eq!( eval("2 == 2"),     JSValue::from(json!(true)) );
}
