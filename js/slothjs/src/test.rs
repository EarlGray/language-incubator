use std::io::Write;
use std::process::{Command, Stdio};
use std::convert::TryFrom;

use serde_json::json;

use crate::ast::Program;
use crate::heap;
use crate::heap::Heap;
use crate::object;
use crate::object::{JSON, JSValue, Interpreted};
use crate::error::{Exception, ParseError};
use crate::interpret::Interpretable;

const ESPARSE: &str = "./node_modules/.bin/esparse";

fn run_interpreter(input: &str, heap: &mut Heap) -> Result<Interpreted, Exception> {
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
    program.interpret(heap)
}

fn interpret(input: &str, heap: &mut Heap) -> Result<JSValue, Exception> {
    let result = run_interpreter(input, heap)?;
    result.to_value(heap)
}

fn eval(input: &str) -> JSON {
    let mut heap = Heap::new();
    match interpret(input, &mut heap) {
        Ok(value) => value.to_json(&heap).unwrap(),
        Err(e) => {
            let msg = format!("{:?}", e);
            json!({"error": msg})
        }
    }
}

fn evalbool(input: &str) -> bool {
    let mut heap = Heap::new();
    match interpret(input, &mut heap) {
        Ok(value) => value.boolify(&heap),
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
        let mut heap = Heap::new();
        let expected = json!($json);
        match interpret($js, &mut heap) {
            Ok(result) => {
                let result = result.to_json(&heap)
                    .expect("JSValue::to_json()");
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
        let mut heap = Heap::new();
        match interpret($js, &mut heap) {
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
    assert_eval!( "[]",         []);
    assert_eval!( "+5",         5.0);
    assert_eval!( "+'5'",       5.0);

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
    assert_eq!( eval("[1] + [2,3]"),    JSON::from("12,3") );
    assert_eq!( eval("[1,2] + null"),   JSON::from("1,2null") );
    assert_eq!( eval("null + null"),    JSON::from(0.0) );
    assert_eq!( eval("true + null"),    JSON::from(1.0) );
    assert_eval!( "'' + [1, 2]",    "1,2" );
    assert_eval!( "'' + null",   "null" );
    assert_eval!( "'' + true",   "true" );
    assert_eval!( "'' + {}",     "[object Object]" );
    assert_eval!( "({} + {})",     "[object Object][object Object]" );
    assert_eval!( "({} + [])",     "[object Object]" ); // expression
    assert_eval!( "{} +[]",         0.0 );              // two statements

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
    assert!( evalbool("0 == []") );
    assert!( !evalbool("[] == []") );
    assert!( !evalbool("0 == {}") );
    assert!( !evalbool("({} == {})") );
    assert!( evalbool("var a = {}; a == a") );
    assert!( evalbool("null == null") );
    assert!( evalbool("null == undefined") );
    assert!( !evalbool("NaN == NaN") );

    assert!( !evalbool("2 != 2") );
    assert!( evalbool("2 != 3") );
    assert!( !evalbool("'2' != 2") );
    assert!( evalbool("0 != null") );
    assert!( !evalbool("0 != false") );
    assert!( !evalbool("0 != []") );
    assert!( evalbool("[] != []") );
    assert!( evalbool("0 != {}") );
    assert!( !evalbool("null != null") );
    assert!( !evalbool("null != undefined") );
    assert!( evalbool("NaN != NaN") );

    assert!( evalbool("2 !== 3") );
    assert!( evalbool("2 === 2") );
    assert!( evalbool("+0 === -0") );
    assert!( !evalbool("NaN === NaN") );
    assert!( !evalbool("2 === 3") );
    assert!( !evalbool("'2' === 2") );
    assert!( !evalbool("0 === null") );
    assert!( !evalbool("0 === false") );
    assert!( !evalbool("0 === []") );
    assert!( !evalbool("[] === []") );
    assert!( !evalbool("0 === {}") );
    assert!( evalbool("null === null") );
    assert!( !evalbool("null === undefined") );

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

    assert_eval!( "true && true", true );
    assert_eval!( "true && false", false );
    assert_eval!( "1 && 2",     2.0 );
    assert_eval!( "0 && 1",     0.0 );
    assert_eval!( "var a; (a = 0) && (a = 1)",  0.0);
    assert_eval!( "var a; (a = 1) && (a = 2)",  2.0);

    assert_eval!( "true || false", true );
    assert_eval!( "false || false", false );
    assert_eval!("null || 'a'",     "a" );
    assert_eval!("'a' || 'b'",      "a");
    assert_eval!( "var a; (a = 0) || (a = 1)",  1.0);
    assert_eval!( "var a; (a = 1) || (a = 2)",  1.0);
}

#[test]
fn test_member_expression() {
    assert_eq!( eval("['zero', 'one', 'two'][2]"),      JSON::from("two"));
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
    assert_eval!( "a = 1; a",   1.0 );
    assert_exception!( "b", Exception::ReferenceNotFound );
    assert_exception!( "a = a + 1", Exception::ReferenceNotFound );
    assert_exception!( "a += 1", Exception::ReferenceNotFound );

    assert_eval!( "var a = false; { var a = true; } a",     true );
    assert_eval!( "var a = false; { a = true } a",          true );
    //assert_eval!( "var a = true; { let a = false; } a",     true );
    //assert_eval!( "var a = false; (function() { a = true; })(); a", true );
    assert_eval!( "var a = true; (function(a) { a = false })('nope'); a", true );

    //assert_eval!( "let a = true; { let a = false; { let a = 'whut'; }}; a", true );

    //assert_exception!( "const a = true; a = false; a",  Exception::TypeErrorConstAssign );
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
    assert_eq!( eval("+null"),              JSON::from(0.0) );
    assert!( evalbool("let v = +{}; v != v") );         // NaN
    assert!( evalbool("let v = +[1, 2]; v != v") );
    assert!( evalbool("let v = +'false'; v != v") );
    assert_eval!( "let a = []; +a",          0.0 );
    assert_eval!( "let a = [1]; +a",         1.0 );
    assert_eval!( "let a = +[1, 2]; a != a",  true);

    assert_eq!( eval("-'1'"),               JSON::from(-1.0) );

    assert!( evalbool("!false") );
    assert!( !evalbool("!true") );
    assert!( evalbool("!0") );
    assert!( !evalbool("!1") );
    assert!( evalbool("!!'yes'") );
    assert!( evalbool("!!'0'") );
    assert!( evalbool("!!'{}'") );
    assert!( evalbool("!!{}") );
    assert!( evalbool("!![]") );
    assert!( !evalbool("!!''") );
    assert!( evalbool("!undefined") );

    assert_eval!("typeof undefined",   "undefined");
    assert_eval!("typeof 1",           "number");
    assert_eval!("typeof true",        "boolean");
    assert_eval!("typeof ''",          "string");
    assert_eval!("typeof {}",          "object");
    assert_eval!("typeof null",        "object");
    assert_eval!("typeof []",          "object");
    assert_eval!("typeof new Boolean()",    "object");
    assert_eval!("typeof (function(){})",   "function");
    assert_eval!("typeof parseInt",         "function");

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
    assert_eval!( "parseInt('42')",     42.0);

    // FunctionExpression
    assert_eval!( r#"
        let twice = function(x) { return x + x; };
        twice(12)
    "#, 24.0);

    // a function returns a value
    assert_eval!( "(function () { return true; })()",   true );

    // return returns immediately
    assert!( evalbool(r#"
        let func = function() { return true; return false; };
        func()
    "#));

    // the arguments are always fresh
    assert_eq!( eval(r#"
        let twice = function(x) { return x + x; };
        twice(12)
        twice('a')
    "#), JSON::from("aa"));

    // function scope
    assert_eq!( eval(r#"
        let x = 1;
        twice = function(x) { return x + x; };
        twice(12)
        x
    "#), JSON::from(1.0));

    // Function.length
    assert_eval!("var sqr = function(x) { return x*x; }; sqr.length",  1.0);

    // recursive functions
    assert_eval!(r#"
        var gcd = function(a, b) { return (a == b ? a : (a < b ? gcd(a, b-a) : gcd(a-b, b))); };
        gcd(12, 15)
    "#, 3.0);

    // immediate/anonymous function call
    assert_eval!( "(function(x) { return x + x; })(12)",   24.0);

    // closures
    assert_eval!(r#"
        let adder = function(y) { return function(x) { return x + y; } };
        let add3 = adder(3);
        add3(4)
    "#, 7.0);

    /*
    // FunctionDeclaration
    assert_eq!(evalbool(r#"
        function sqr(x) { return x * x; };
        sqr(12)
    "#), JSON::from(144.0));
    */

}

#[test]
fn test_global_methods() {
    // parseInt:
    assert_eval!( "parseInt('42')",     42.0 );
    assert_eval!( "parseInt(5)",        5.0 );
    assert_eval!( "parseInt('nope')",   (f64::NAN) );
    assert_eval!( "parseInt('1'+'2', 'yep')", 12.0 );
    assert_eval!( "parseInt('22', 38)", (f64::NAN) );
    assert_eval!( "parseInt('22', 1)",  (f64::NAN) );
    assert_eval!( "parseInt('20', 8)",  16.0 );
    assert_eval!( "parseInt('020', 10)", 20.0 );
    assert_eval!( "parseInt('020')",     16.0 );
    //assert_eval!( "parseInt('0x10')",   16.0 );
}

#[test]
fn test_builtin_object() {
    // test its prototype chain
    assert!( evalbool("Object.is(Object.__proto__, Function.prototype)") );
    assert!( evalbool("Object.is(Object.__proto__.__proto__, Object.prototype)") );
    assert!( evalbool("Object.is(Object.__proto__.__proto__.__proto__, null)") );

    // constructor
    assert_eval!( "Object(null)",     {} );
    assert_eval!( "Object(undefined)", {} );
    assert_eval!( "var a = {one: 1}; Object(a) == a", true );
    //assert_eval!( "Object(1) instanceof Number", true );
    //assert_eval!( "Object(false) instanceof Boolean", true );

    assert_eval!( "new Object(null)",     {} );
    assert_eval!( "new Object(undefined)", {} );
    assert_eval!( "new Object({one: 1})", {"one": 1.0} );
    //assert_eval!( "new Object(1) instanceof Number", true );
    //assert_eval!( "new Object(true) instanceof Boolean", true );

    // Object.defineProperty

    // Object.getOwnPropertyDescriptor
    assert_eval!(
        "Object.getOwnPropertyDescriptor(Object, 'prototype')",
        {"writable": false, "configurable": false, "enumerable": false, "value": {}}
    );
    assert_eval!(
        "Object.getOwnPropertyDescriptor(Object, 'getOwnPropertyDescriptor')",
        {"configurable": true, "enumerable": false, "value": {}, "writable": true}
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

    /*
    // Object.assign
    assert_eval!(r#"
        const obj = {one: 1};
        const copy = Object.assign({}, obj);
        obj.one
    "#, 1.0);
     */
    // Object.create()
    // Object.entries()
    // Object.keys()
    // Object.values()
    // Object.freeze()
    // Object.getOwnPropertyDescriptors()
    // Object.getOwnPropertyNames()
    // Object.getPrototypeOf()
    // Object.setPrototypeOf()
    // Object.fromEntries()
    // Object.isExtensible()
    // Object.isFrozen()
    // Object.isSealed()
    // Object.preventExtensions()
    // Object.seal()

    // Object.prototype.hasOwnProperty()
    // Object.prototype.isPrototypeOf()
    // Object.prototype.propertyIsEnumerable()

    // Object.prototype.toString()
    assert_eval!("({}).toString()",   "[object Object]");

    // Object.prototype.valueOf()
    assert_eval!("var obj = {}; obj.valueOf() == obj", true);
    assert_eval!("var obj = {}; obj.valueOf() == {}", false);
    assert_exception!("null.valueOf()",  Exception::TypeErrorNotCallable);
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
fn test_builtin_boolean() {
    //assert_eval!("true instanceof Boolean", false);

    assert_eval!("var b = new Boolean(false); b.valueOf()",     false);
    assert_eval!("var b = new Boolean(); b.valueOf()",          false);
    assert_eval!("var b = new Boolean(undefined); b.valueOf()", false);
    assert_eval!("var b = new Boolean(null); b.valueOf()",      false);
    assert_eval!("var b = new Boolean(NaN); b.valueOf()",       false);

    assert_eval!("var b = new Boolean(true); b.valueOf()",          true);
    assert_eval!("var b = new Boolean(new Boolean()); b.valueOf()", true);
    assert_eval!("var b = new Boolean([]); b.valueOf()",            true);

    assert_eval!("Boolean(false)",  false);
    assert_eval!("Boolean(-0)",     false);
    assert_eval!("Boolean(NaN)",    false);
    assert_eval!("Boolean('')",     false);

    assert_eval!("Boolean([])",             true);
    assert_eval!("Boolean(new Boolean())",  true);
    //assert_eval!("Boolean(new String(''))",  true);

    assert_eval!("+(new Boolean())",    0.0);
    assert_eval!("Object(true).valueOf()",    true);
    assert_eval!("Object(false).valueOf()",    false);

    // Boolean.prototype.toString()
    assert_eval!("new Boolean().toString()",  "false");
    // auto-objectification:
    assert_eval!("true.toString()",   "true");
    assert_eval!("false.toString()",  "false");

    // Boolean.prototype.valueOf()
    assert_eval!("new Boolean().valueOf()", false);
    assert_eval!("new Boolean(1).valueOf()", true);
}

#[test]
fn test_objects() {
    assert_eval!( "var a = [1]; a[0] = 2; a[0]",    2.0);
    assert_eval!( "var a = {v: 1}; a.v = 2; a.v",   2.0);
    assert_eval!( "var a = {}; a.one = 1; a",       {"one": 1.0});

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

    // NewExpression
    assert_eval!("new Object()",    {});
    assert_eval!(r#"
        let HasOne = function() { this.one = 1; }
        let obj = new HasOne()
        obj.one
    "#, 1.0);
    assert_eval!(r#"
        let HasOne = function() { this.one = 1; }
        let obj1 = new HasOne()
        let obj2 = new HasOne()
        obj2.one = 2;
        obj1.one + obj2.one
    "#, 3.0);
    assert_eval!(r#"
        let Class = function() { return {one: 1}; }
        let obj = new Class()
        obj.one
    "#, 1.0);
    /* // TODO: property lookup on the prototype chain
    assert_eval!(r#"
        let HasOne = function() { this.one = 1; }
        let obj1 = new HasOne()
        let obj2 = new HasOne()
        HasOne.prototype.two = 2
        obj1.two + obj2.two
    "#, 4.0);
    */
}

#[test]
fn test_this() {
    assert_eval!("var f = function() { return this; }; f() == global", true );

    assert_eval!(r#"
        const test = { prop: 42, func: function() { return this.prop; } };
        test.func()
    "#, 42.0 );

    // call
    assert_eval!(r#"
        const add = function(c, d) { return this.a + this.b + c + d }
        var o = {a: 1, b: 3}
        add.call(o, 5, 7)
    "#, 16.0);

    // apply
    assert_eval!(r#"
        const f = function(c, d) { return this.a + this.b + c + d }
        var o = {a: 1, b: 3}
        f.apply(o, [5, 7])
    "#, 16.0);

    /*
    assert_eval!(r#"
        var o = {f: function() { return this.a + this.b; }}
        var p = Object.create(o)
        p.a = 1
        p.b = 4
        p.f()
    "#, 5.0 );

    // bind
    assert_eval!(r#"
        var f = function(c, d) { return this.a + this.b + c + d }
        f = f.bind({a: 1, b: 3}, 5)
        f(7)
    "#, 16.0);

    assert_eval!(r#"
        var f = function(c, d) { return this.a + this.b + c + d }
        f = f.bind({a: 1, b: 3})
        f(5, 7)
    "#, 16.0);
    assert_eval!(r#"
        var obj = { a: 1, get one() { return this.a; }};
        obj.one
    "#, 1.0);
    assert_eval!(r#"
        var obj = { a: 1 };
        Object.defineProperty(obj, 'one', {
            get: function() { return this.a; },
            enumerable: true,
            configurable: true
        });
        obj.one
    "#, 1.0);
    */
    assert_eval!(r#"
        let Class = function() { this.prop = true; }
        let obj = new Class();
        obj.prop
    "#, true);
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
    use object::{Access, Content, Interpreted, ObjectValue, Property};

    println!("============================");
    println!("size_of JSRef:  \t{}", size_of::<heap::JSRef>());
    println!("size_of JSValue:\t{}", size_of::<JSValue>());
    println!("size_of Interpreted:\t{}", size_of::<Interpreted>());
    println!("size_of JSObject:\t{}", size_of::<object::JSObject>());
    println!("size_of   HashMap:\t{}", size_of::<std::collections::HashMap<String, Property>>());
    println!("size_of   ObjectValue:\t{}", size_of::<ObjectValue>());
    println!("size_of     JSArray:\t{}", size_of::<object::JSArray>());
    println!("size_of     NativeFunc:\t{}", size_of::<object::NativeFunction>());
    println!("size_of     Closure:\t{}", size_of::<object::Closure>());
    println!("size_of Property:\t{}", size_of::<Property>());
    println!("size_of   Access:\t{}", size_of::<Access>());
    println!("size_of   Content:\t{}", size_of::<Content>());
    println!("============================");
}

// this is for one-off experiments, don't commit anything here:
#[test] fn test_scratch() {
}
