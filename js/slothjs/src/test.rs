use std::io::Write;
use std::process::{Command, Stdio};
use std::convert::TryFrom;

use serde_json::json;

use crate::ast::Program;
use crate::object;
use crate::object::{JSON, JSValue, Interpreted};
use crate::error::{Exception, ParseError};
use crate::interpret::{Interpretable, RuntimeState};

const ESPARSE: &str = "./node_modules/.bin/esparse";

fn run_interpreter(input: &str, state: &mut RuntimeState) -> Result<Interpreted, Exception> {
    let mut child = Command::new(ESPARSE)
        .arg("--loc")
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
        .map_err(|err| {
            let err = ParseError::InvalidJSON{ err: err.to_string() };
            Exception::SyntaxError(err)
        })?;
    let program = Program::try_from(&json)
        .map_err(|e| Exception::SyntaxError(e))?;
    program.interpret(state)
}

fn interpret(input: &str, state: &mut RuntimeState) -> Result<JSValue, Exception> {
    let result = run_interpreter(input, state)?;
    result.to_value(&state.heap)
}

fn eval(input: &str) -> JSON {
    let mut state = RuntimeState::new();
    match interpret(input, &mut state) {
        Ok(value) => value.to_json(&state.heap),
        Err(e) => {
            let msg = format!("{:?}", e);
            json!({"error": msg})
        }
    }
}

fn evalbool(input: &str) -> bool {
    let mut state = RuntimeState::new();
    match interpret(input, &mut state) {
        Ok(value) => value.boolify(),
        Err(e) => {
            let msg = format!("{:?}", e);
            panic!(msg)
        }
    }
}

/// Runs interpretation of the first argument (a string literal),
/// then compares the result to the second argument (anything that `serde_json::json!`
/// understands).
macro_rules! assert_eval {
    ($js:literal, $json:tt) => {
        let mut state = RuntimeState::new();
        let expected = json!($json);
        match interpret($js, &mut state) {
            Ok(result) => {
                let result = result.to_json(&state.heap);
                assert_eq!( result, expected )
            }
            Err(exc) => {
                panic!(format!("\n     want value: {}\n  got exception: {:?}", expected, exc))
            }
        }
    }
}

/// Run interpretation of the first argument (a string literal),
/// then expects it to fail with a given variant of Exception:
/// ```ignored
/// assert_exception!( "bla", Exception::ReferenceNotFound );
/// ```
// TODO: look if it's possible to match exception arguments as well:
// ```
// assert_exception!( "bla", Exception::ReferenceNotFound("bla") );
// ```
macro_rules! assert_exception {
    ($js:literal, $exc:path) => {
        let mut state = RuntimeState::new();
        match interpret($js, &mut state) {
            Err($exc(_)) => (),
            other => {
                panic!(format!("\n   want {}\n   got: {:?}\n", stringify!($exc), other))
            }
        }
    };
}

#[test]
fn test_literals() {
    assert_eval!( "null",       null);
    assert_eval!( "true",       true);
    assert_eval!( "42",         42.0);
    assert_eval!( "0x2a",       42.0);
    assert_eval!( "052",        42.0);
    //assert_eq!( eval("[]"),         JSValue::from(json!([])));
    //assert_eq!( eval("+5"),         JSValue::from(5));
    //assert_eq!( eval("+'5'"),       JSValue::from(5));

    assert_eval!( "\"hello \\\"world\\\"\"", "hello \"world\"");

    assert_eval!("var a = {one:1, two:2}; a", {"one": 1.0, "two": 2.0});

    assert_eq!(
        eval("let x = 'one'; let o = {[x]: 1}; o.one"),
        JSON::from(1.0)
    );

    assert_eq!( eval("var undefined = 5; undefined"), JSON::Null );
    assert!( evalbool("var NaN = 5; NaN != NaN") );
}

#[test]
fn test_binary_operations() {
    // O_o
    assert_eq!( eval("2 + 2"),          JSON::from(4.0) );
    assert_eq!( eval("'1' + '2'"),      JSON::from("12") );
    //assert_eq!( eval("[1] + [2,3]"),    JSON::from("12,3") );
    //assert_eq!( eval("[1,2] + null"),   JSON::from("1,2null") );
    assert_eq!( eval("null + null"),    JSON::from(0.0) );
    assert_eq!( eval("true + null"),    JSON::from(1.0) );

    assert_eval!( "5 - 3",  2.0 );
    assert_eval!( "3.5 - 5",  (-1.5) );
    assert_eval!( "5 - 'hello'",  (f64::NAN) );
    assert_eval!( "5 - true",  4.0 );

    assert_eval!( "5 * 3",      15.0 );
    assert_eval!( "'lol' * 3",  (f64::NAN) );
    assert_eval!( "'2' * '3'",  6.0 );
    assert_eval!( "3 * null",   0.0 );


    // o_O
    assert!( evalbool("2 == 2") );
    assert!( !evalbool("2 == 3") );
    assert!( evalbool("'2' == 2") );
    assert!( !evalbool("0 == null") );
    assert!( evalbool("0 == false") );
    //assert!( evalbool("0 == []") );
    //assert!( !evalbool("[] == []") );
    //assert!( !evalbool("0 == {}") );
    //assert!( !evalbool("{} == {}") );
    //assert!( evalbool("var a = {}; a == a") );
    assert!( evalbool("null == null") );
    assert!( evalbool("null == undefined") );
    assert!( !evalbool("NaN == NaN") );

    assert!( !evalbool("2 != 2") );
    assert!( evalbool("2 != 3") );
    assert!( !evalbool("'2' != 2") );
    assert!( evalbool("0 != null") );
    assert!( !evalbool("0 != false") );
    //assert!( !evalbool("0 != []") );
    //assert!( evalbool("[] != []") );
    //assert!( evalbool("0 != {}") );
    assert!( !evalbool("null != null") );
    assert!( !evalbool("null != undefined") );
    assert!( evalbool("NaN != NaN") );

    /*
    assert!( evalbool("2 !== 3") );
    assert!( evalbool("2 === 2") );
    assert!( !evalbool("2 === 3") );
    assert!( !evalbool("'2' === 2") );
    assert!( !evalbool("0 === null") );
    assert!( !evalbool("0 === false") );
    assert!( !evalbool("0 == []") );
    assert!( !evalbool("[] === []") );
    assert!( !evalbool("0 === {}") );
    assert!( evalbool("null === null") );
    assert!( !evalbool("null === undefined") );
    assert!( !evalbool("NaN === NaN") );
    */

    assert!( !evalbool("'a' < 'a'") );
    assert!( evalbool("1 < 2") );
    assert!( !evalbool("'113' < 13") );
    assert!( evalbool("'0' < '00'") );
    assert!( !evalbool("'0' < 0") );
    assert!( evalbool("'a' < 'b'") );
    assert!( !evalbool("'aa' < 'a'") );
    assert!( evalbool("null < 1") );
    assert!( !evalbool("undefined < 1") );
    assert!( !evalbool("NaN < 3") );
    assert!( !evalbool("NaN < NaN") );
    assert!( !evalbool("undefined < NaN") );
    //assert!( evalbool("[1, 1] < [2]") );

    /*
    assert!( !evalbool("'a' > 'a'") );
    assert!( !evalbool("1 > 2") );
    assert!( evalbool("'113' > 13") );
    assert!( !evalbool("'0' > '00'") );
    assert!( !evalbool("'0' > 0") );
    assert!( !evalbool("'a' > 'b'") );
    assert!( evalbool("'aa' > 'a'") );
    assert!( !evalbool("null > 1") );
    */

    /*
    assert!( evalbool("1 <= 2") );
    assert!( !evalbool("2 <= 1") );
    assert!( evalbool("2 <= 2") );
    assert!( !evalbool("undefined <= undefined") );
    */

    /*
    assert!( evalbool("1 in [1, 2, 3]") );
    assert!( !evalbool("0 in [1, 2, 3]") );

    assert!( evalbool("{} instanceof Object") );
    assert_eq!( eval("5 - 3"), JSValue::from(2) );
    assert!( evalbool("isNaN('a' - 3)"));

    assert_eq!( eval("6 / 3"), JSValue::from(2) );

    assert_eq!( eval("143 % 12"), JSValue::from(11) );

    assert_eq!( eval("2 ** 8"), JSValue::from(256.0));

    assert_eq!( eval("6 | 9"), JSValue::from(15));
    assert_eq!( eval("0xA0 | 8"), JSValue::from(0xA8));
    assert_eq!( eval("5 ^ 3"), JSValue::from(6) );
    assert_eq!( eval("0xA3 ^ 0xAC"), JSValue::from(0x0F) );
    assert_eq!( eval("6 & 9"), JSValue::from(0));

    assert_eq!( eval("0xA << 4"), JSValue::from(0xA0));
    assert_eq!( eval("0xA5 >> 4"), JSValue::from(0xA));

    assert_eq!( eval("0xA5 >>> 4"), JSValue::from(0xA));
    */

    /*
    assert!( evalbool("true && true") );
    assert!( !evalbool("true && false") );

    assert_eq!( eval("1 && 2"), JSValue::from(2) );
    assert_eq!( eval("0 && 1"), JSValue::from(0) );

    assert!( evalbool("true || false") );
    assert!( evalbool("false || false") );

    assert_eq!( eval("null || 'a'"), JSValue::from("a") );
    assert_eq!( eval("'a' || 'b'"), JSValue::from("a") );
     */
}

#[test]
fn test_member_expression() {
    //assert_eq!( eval("['zero', 'one', 'two'][2]"),      JSON::from("two"));
    assert_eq!( eval("let o = {one: 1}; o.one"),        JSON::from(1.0));
    assert_eq!( eval("var a = {}; a.one = 1; a"),       json!({"one": 1.0}));
    assert_eq!( eval("let o = {'o e': 1}; o['o e']"),   JSON::from(1.0));
    assert_eq!(
        eval("let x = 'one'; let o = {[x]: 1}; o"),
        json!({"one": 1.0})
    );
    assert_eq!(
        eval("let a = {}; a.sub = {}; a.sub.one = 1; a"),
        json!({"sub": {"one": 1.0}})
    );
    assert_exception!(
        "let a = {}; a.sub.one = 1", Exception::ReferenceNotAnObject
    );
}

#[test]
fn test_assignment() {
    assert_eval!( "var a = 1; a = 2; a",    2.0 );
    assert_eval!( "a = b = 1; a + b",       2.0 );
    assert_eval!( "var a = 1; a += 1; a",   2.0 );
    assert_eval!( "var a = 1; a += 1",      2.0 );
    assert_eval!( "var a = 3; a *= a; a",   9.0 );
    assert_eval!( "var a = 1; a -= 1; a",   0.0 );
    /*
    assert_eq!( eval("var a = 3; a **= a; a"),            JSValue::from(27));
    assert_eq!( eval("var a = 3; a /= a; a"),            JSValue::from(1));
    assert_eq!( eval("var a = 13; a %= 8; a"),            JSValue::from(5));
    assert_eq!( eval("var a = 1; a <<= 4; a"),            JSValue::from(16));
    assert_eq!( eval("var a = 32; a >>= 4; a"),            JSValue::from(2));
    assert_eq!( eval("var a = 32; a >>>= 4; a"),            JSValue::from(2));
    assert_eq!( eval("var a = 6; a &= 9; a"),            JSValue::from(0));
    assert_eq!( eval("var a = 6; a ^= 9; a"),            JSValue::from(15));
    assert_eq!( eval("var a = 3; a |= 6; a"),            JSValue::from(7));
    */
}

#[test]
fn test_scope() {
    assert_eq!( eval("a = 1; a"),   JSON::from(1.0) );
    assert_exception!( "b", Exception::ReferenceNotFound );
    assert_exception!( "a = a + 1", Exception::ReferenceNotFound );
    assert_exception!( "a += 1", Exception::ReferenceNotFound );
}

/*
#[test]
fn test_sequence() {
    assert_eq!( eval("let a = 0; a=a+2, a=a+2"), JSValue::from(6) );
}
*/

#[test]
fn test_blocks() {
    assert_eq!( eval(r#"
        var a = 1, b = 2;
        { a = 10; b = 20 };
        a + b
    "#), JSON::from(30.0));
}

#[test]
fn test_conditionals() {
    assert!( evalbool("'0' ? true : false"));
    assert!( !evalbool("0 ? true : false"));
    assert!( evalbool("({} ? true : false)") );

    assert_eq!(
        eval("var a; if (a = 1) a = 2; else a = 3; a"),
        JSON::from(2.0)
    );
    assert_eq!(
        eval("var a = 1; if (null) { a = 2; }; a"),
        JSON::from(1.0)
    );
}

#[test]
fn test_loops() {
    // for (<init>; <test>; <update)
    assert_eq!( eval(r#"
        var a = 0;
        for (var i = 0; i < 5; i += 1) {
            a = a + 1;
        }
        a
    "#), JSON::from(5.0));

    assert!( evalbool(r#"
        let a = true;
        for (; false; ) a = false;
        a
    "#));

    /*
    // while
    assert!( evalbool(r#"
        let a = false;
        while (!a) { a = true }
        a
    "#));

    // do while
    assert!( evalbool(r#"
        let a = false;
        do { a = true } while (0);
        a
    "#));

    // break
    assert!( evalbool(r#"
        for (;;) break;
        true
    "#));

    // continue
    assert!( evalbool(r#"
        let a = false;
        while (!a) {
            a = true;
            continue;
            a = false;
            break;
        }
        a
    "#));

    // labelled break
    assert!( evalbool(r#"
        let a = false;
        label: do {
            a = true;
            for (;;) { break label; }
            a = false;
        } while (0);
        a
    "#));

    assert_eq!( eval(r#"
        let obj = {one:1, two: 2, three: 3};
        let sum = 0;
        for (let prop in obj) {
            sum = sum + obj[prop];
        }
        sum
    "#), JSValue::from(6));
     */
}

/*
#[test]
fn test_exceptions() {
    assert!(evalbool(r#"
        let a = false;
        try {
            throw '';
        } catch (e) {
            a = true;
        }
        a
    "#));
}
*/

#[test]
fn test_unary_operations() {
    assert_eq!( eval("+1"),                 JSON::from(1.0) );
    assert_eq!( eval("+'1'"),               JSON::from(1.0) );
    assert_eq!( eval("+false"),             JSON::from(0.0) );
    assert!( evalbool("let v = +{}; v != v") );         // NaN
    assert!( evalbool("let v = +'false'; v != v") );

    assert_eq!( eval("-'1'"),               JSON::from(-1.0) );

    assert!( evalbool("!false") );
    assert!( !evalbool("!true") );
    assert!( evalbool("!0") );
    assert!( !evalbool("!1") );
    assert!( evalbool("!!'yes'") );
    assert!( evalbool("!!'0'") );
    assert!( evalbool("!!'{}'") );
    assert!( !evalbool("!!''") );
    assert!( evalbool("!undefined") );

    assert_eq!( eval("typeof undefined"),   JSON::from("undefined"));
    assert_eq!( eval("typeof 1"),           JSON::from("number"));
    assert_eq!( eval("typeof ''"),          JSON::from("string"));
    assert_eq!( eval("typeof {}"),          JSON::from("object"));
    assert_eq!( eval("typeof null"),        JSON::from("object"));
    //assert_eq!( eval("typeof []"),          JSON::from("object"));

    assert_eq!( eval("~-1"),                JSON::from(0.0));
    assert_eq!( eval("~-2"),                JSON::from(1.0));
    assert_eq!( eval("~2"),                 JSON::from(-3.0));
    assert_eq!( eval("~2"),                 JSON::from(-3.0));
    assert_eq!( eval("~NaN"),               JSON::from(-1.0));
    assert_eq!( eval("~{}"),                JSON::from(-1.0));
    assert_eq!( eval("~~''"),                JSON::from(0.0));
    assert_eq!( eval("~~'whut'"),            JSON::from(0.0));

    assert_eq!( eval("typeof void 'nope'"), JSON::from("undefined") );
    assert_eq!( eval("typeof void {}"),     JSON::from("undefined") );

    assert_eq!( eval("let a = {one: 1}; delete a.one; a"),   json!({}) );
    assert!( evalbool("let a = {one: 1}; delete a.one") );
    assert!( evalbool("let a = {one: 1}; delete a['one']") );
    assert_eq!( eval("let a = {one: 1}; delete a.two; a"),   json!({"one": 1.0}) );
    assert!( evalbool("let a = {one: 1}; delete a.two") );
    //assert!( !evalbool("delete undefined") );   // global.undefined is not configurable
    //assert!( !evalbool("var a = 1; delete a")); // vars are not configurable
    assert!( evalbool("a = 1; delete a") );     // but these are.
    assert!( evalbool("delete 0") );            // don't ask.
    //assert!( evalbool("delete x") );
    // assert!( evalbool("let a = ['one', 'two']; delete a[2]") );
    // assert!( evalbool("let a = ['one', 'two']; delete a[1]") );
}

#[test]
fn test_functions() {
    // CallExpression for builtin functions
    assert_eq!( eval("parseInt('42')"),     JSON::from(42.0));

    // FunctionExpression
    assert_eq!( eval(r#"
        let twice = function(x) { return x + x; };
        twice(12)
    "#), JSON::from(24.0));

    assert!( evalbool(r#"
        let func = function() { return true; return false; };
        func()
    "#));

    assert_eq!( eval(r#"
        let twice = function(x) { return x + x; };
        twice(12)
        twice('a')
    "#), JSON::from("aa"));

    assert_eq!( eval(r#"
        let x = 1;
        twice = function(x) { return x + x; };
        twice(12)
        x
    "#), JSON::from(1.0));

    assert_eval!("var sqr = function(x) { return x*x; }; sqr.length",  1.0);
    /*
    assert_eval!(r#"
        var gcd = function(a, b) { return (a == b ? a : (a < b ? gcd(a, b-a) : gcd(a-b, b))); };
        gcd(12, 15)
    "#, 3.0);

    assert_eq!( eval(r#"
        (function(x) { return x + x; })(12)
    "#), JSON::from(24.0));
     */

    /*
    // FunctionDeclaration
    assert_eq!(evalbool(r#"
        function sqr(x) { return x * x; };
        sqr(12)
    "#), JSON::from(144.0));
    */

    // TODO: closures and scope
}

#[test]
fn test_global_methods() {
    // parseInt
    assert_eval!( "parseInt('42')",     42.0 );
    assert_eval!( "parseInt(5)",        5.0 );
    assert_eval!( "parseInt('nope')",   (f64::NAN) );
    assert_eval!( "parseInt('1'+'2', 'yep')", 12.0 );
    //assert_eval!( "parseInt('0x10')",   16.0 );
    assert_eval!( "parseInt('22', 38)", (f64::NAN) );
    assert_eval!( "parseInt('22', 1)",  (f64::NAN) );
    assert_eval!( "parseInt('20', 8)",  16.0 );
    assert_eval!( "parseInt('020', 10)", 20.0 );
    assert_eval!( "parseInt('020')",     16.0 );
}

#[test]
fn test_builtin_object() {
    // test its prototype chain
    assert!( evalbool("Object.is(Object.__proto__, Function.prototype)") );
    assert!( evalbool("Object.is(Object.__proto__.__proto__, Object.prototype)") );
    assert!( evalbool("Object.is(Object.__proto__.__proto__.__proto__, null)") );

    // Object.getOwnPropertyDescriptor
    assert_eq!(
        eval("Object.getOwnPropertyDescriptor(Object, 'prototype')"),
        json!({"writable": false, "configurable": false, "enumerable": false, "value": {}})
    );
    assert_eq!(
        eval("Object.getOwnPropertyDescriptor(Object, 'getOwnPropertyDescriptor')"),
        json!({"enumerable": false, "writable": true, "configurable": true, "value": "[[native]]"})
    );

    // Object.is
    assert!( evalbool("Object.is(null, null)") );
    assert!( evalbool("Object.is(undefined, undefined)") );
    assert!( evalbool("Object.is(true, true)") );
    assert!( !evalbool("Object.is(null, undefined)") );
    assert!( evalbool("Object.is(NaN, NaN)") );
    assert!( evalbool("Object.is(-0, -0)") );
    assert!( !evalbool("Object.is(-0, +0)") );
    assert!( !evalbool("Object.is('lol', 'lolnot')") );

    assert!( evalbool("var a = {}; Object.is(a, a)") );
    assert!( evalbool("var a = {}; var b = a; Object.is(a, b)") );
    assert!( !evalbool("Object.is({}, {})") );
    assert!( evalbool("Object.is(global, global)") );
}

#[test]
fn test_builtin_function() {
    assert_eq!(
        eval("Object.getOwnPropertyDescriptor(global, 'Function')"),
        json!({"writable": true, "enumerable": false, "configurable": true,  "value": {}})
    );
    //assert_eval!("var sqr = Function('x', 'return x * x'); sqr(12)",  144.0);
}

#[test]
fn test_objects() {
    //assert_eq!( eval("var a = [1]; a[0] = 2; a[0]"),    JSValue::from(2));
    assert_eq!( eval("var a = {v: 1}; a.v = 2; a.v"),   JSON::from(2.0));
    assert_eq!( eval("var a = {}; a.one = 1; a"),       json!({"one": 1.0}));

    assert_eq!(
        eval("var a = {}, b = {}; a.b = b; b.one = 1; a"),
        json!({"b": {"one": 1.0}})
    );
    assert_eq!(
        eval("var a = {b: 2}; var b = a.b; b = 1; a.b"),
        JSON::from(2.0)
    );
    assert_eq!(
        eval("var a = {b: {}}; var b = a.b; b.one = 1; a.b.one"),
        JSON::from(1.0)
    );

    assert_exception!( "a.one = 1", Exception::ReferenceNotFound );
}

#[test]
fn test_arrays() {
    assert_eval!( "[]",   [] );
    assert_eval!( "[1, 'a', undefined]",   [1.0, "a", null] );

    assert_eval!( "let a = ['zero', 'one']; a[1]",  "one" );
    assert_eval!( "let a = ['zero', 'one']; a[2]",  null );

    //assert_eval!( "let a = ['zero', 'one']; a.length", 2 );
    //assert_eval!( "let a = ['zero', 'one']; a[2] = 'two'; a.length", 3 );

    assert_eval!( "let a = ['zero', 'one']; a[2] = 'two'; a[2]", "two" );
    assert_eval!( "let a = ['zero', 'one']; a[1] = 'один'; a[1]", "один" );

    //assert_eval!( "let a = ['zero']; a.push('one'); a[1]",  "one" );
}

/// ```sh
/// $ cargo -q test --lib sizes -- --nocapture
/// ```
#[test]
fn test_sizes() {
    use std::mem::size_of;
    println!("============================");
    println!("size_of JSRef:  \t{}", size_of::<object::JSRef>());
    println!("size_of JSObject:\t{}", size_of::<object::JSObject>());
    println!("size_of JSValue:\t{}", size_of::<JSValue>());
    println!("size_of Interpreted:\t{}", size_of::<object::Interpreted>());
    println!("size_of Property:\t{}", size_of::<object::Property>());
    println!("size_of   Access:\t{}", size_of::<object::Access>());
    println!("size_of   Content:\t{}", size_of::<object::Content>());
    println!("size_of     JSArray:\t{}", size_of::<object::JSArray>());
    println!("size_of     NativeFunc:\t{}", size_of::<object::NativeFunction>());
    println!("size_of     Closure:\t{}", size_of::<object::Closure>());
    println!("============================");
}

// this is for one-off experiments, don't commit anything here:
#[test] fn test_scratch() {
}
