use std::io::Write;
use std::process::{Command, Stdio};

use serde_json::json;

use crate::{
    Heap,
    Interpreted,
    JSON,
    JSValue,
    Program,
};
use crate::interpret::Interpretable;
use crate::object;
use crate::error::{Exception, ParseError};

const ESPARSE: &str = "./node_modules/.bin/esparse";

fn run_interpreter(input: &str, heap: &mut Heap) -> Result<Interpreted, Exception> {
    let mut child = Command::new(ESPARSE)
        //.arg("--loc")
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
    let program = Program::parse_from(&json)
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
        Err(e) => panic!("{:?}", e),
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
                panic!("\n     want value: {}\n  got exception: {:?}", expected, exc)
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
                panic!("\n   want {}\n   got: {:?}\n", stringify!($exc), other)
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
        eval("var x = 'one'; var o = {[x]: 1}; o.one"),
        JSON::from(1.0)
    );

    assert_eq!( eval("var undefined = 5; undefined"), JSON::Null );
    assert!( evalbool("var NaN = 5; NaN != NaN") );
}

#[test]
fn test_binary_addition() {
    assert_eval!("2 + 2",          4.0);
    assert_eval!("'1' + '2'",      "12");
    assert_eval!("[1] + [2,3]",    "12,3");
    assert_eval!("[1,2] + null",   "1,2null");
    assert_eval!("null + null",    0.0);
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
}

#[test]
fn test_binary_numeric_operations() {
    assert_eval!( "5 - 3",  2.0 );
    assert_eval!( "3.5 - 5",  (-1.5) );
    assert_eval!( "5 - 'hello'",  (f64::NAN) );
    assert_eval!( "5 - true",  4.0 );

    assert_eval!( "5 * 3",      15.0 );
    assert_eval!( "'lol' * 3",  (f64::NAN) );
    assert_eval!( "'2' * '3'",  6.0 );
    assert_eval!( "3 * null",   0.0 );

    assert_eval!("12 / 3", 4.0);
    assert_eval!("14.5 % 3", 2.5);
}

#[test]
fn test_binary_bit_operations() {
    assert_eval!("6 | 9", 15.0);
    assert_eval!("0xA0 | 8", (0xA8 as f64));

    assert_eval!("5 ^ 3", 6.0);
    assert_eval!("0xA3 ^ 0xAC", (0x0F as f64));

    assert_eval!("6 & 9", 0.0);
    assert_eval!("7 & 5", 5.0);

    assert_eval!("0xA << 4", (0xA0 as f64));
    assert_eval!("0xA5 >> 4", (0xA as f64));

    assert_eval!("0xA5 >>> 4", (0xA as f64));
}

#[test]
fn test_binary_loose_equality() {
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

    assert_eval!("Object(false) == false", true);
    assert_eval!("Object(undefined) == null", false);
}

#[test]
fn test_binary_strict_equality() {
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
}

#[test]
fn test_binary_compare_less() {
    assert_eval!("1 < 2", true);
    assert_eval!("'0' < '00'", true);
    assert_eval!("'a' < 'b'", true);
    assert_eval!("null < 1", true);
    //assert_eval!("[1, 1] < [2]", true);

    assert_eval!("'a' < 'a'", false);
    assert_eval!("'113' < 13", false);
    assert_eval!("'0' < 0", false);
    assert_eval!("'aa' < 'a'", false);
    assert_eval!("undefined < 1", false);
    assert_eval!("NaN < 3", false);
    assert_eval!("NaN < NaN", false);
    assert_eval!("undefined < NaN", false);

    assert_eval!("1 <= 2", true);
    assert_eval!("2 <= 2", true);
    assert_eval!("2 <= 1", false);
    assert_eval!("undefined <= undefined", false);
}

#[test]
fn test_binary_compare_greater() {
    assert_eval!("'113' > 13", true);
    assert_eval!("'aa' > 'a'", true);

    assert_eval!("'a' > 'a'", false);
    assert_eval!("1 > 2", false);
    assert_eval!("'0' > '00'", false);
    assert_eval!("'0' > 0", false);
    assert_eval!("'a' > 'b'", false);
    assert_eval!("null > 1", false);

    assert_eval!("2 >= 1", true);
    assert_eval!("1 >= 2", false);
    assert_eval!("2 >= 2", true);
    assert_eval!("NaN >= NaN", false);
    assert_eval!("undefined >= undefined", false);
}

#[test]
fn test_binary_instanceof() {
    assert_eval!("({} instanceof Object)", true);
    assert_eval!("[] instanceof Array", true);
    assert_eval!("({} instanceof Boolean)", false);
    assert_eval!("true instanceof Boolean", false);
    assert_eval!("Object(false) instanceof Boolean", true);
    assert_eval!("Object(false) instanceof Array", false);
    assert_eval!(r#"
        var Class = function() {};
        var obj = new Class();
        (obj instanceof Class) && !({} instanceof Class)
    "#, true);
    assert_eval!(r#"
        var Class = function() {};
        var Subclass = function() {};
        Subclass.prototype = new Class();
        var obj = new Class();
        (obj instanceof Class) && !(obj instanceof Subclass)
    "#, true);
    assert_eval!(r#"
        var Class = function() {};
        var Subclass = function() {};
        Subclass.prototype = new Class();
        var obj = new Subclass();
        (obj instanceof Class) && (obj instanceof Subclass)
    "#, true);
}

#[test]
fn test_binary_in() {
    assert_eval!("'one' in {one: 1}", true);
    assert_eval!("'toString' in {}", true); // prototypes are searched too

    assert_eval!("0 in [1, 2]", true);
    assert_eval!("1 in [1, 2]", true);
    assert_eval!("2 in [1, 2]", false);
}

#[test]
fn test_binary_logical() {
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
    assert_eq!( eval("var o = {one: 1}; o.one"),        JSON::from(1.0));
    assert_eq!( eval("var a = {}; a.one = 1; a"),       json!({"one": 1.0}));
    assert_eq!( eval("var o = {'o e': 1}; o['o e']"),   JSON::from(1.0));
    assert_eq!(
        eval("var x = 'one'; var o = {[x]: 1}; o"),
        json!({"one": 1.0})
    );
    assert_eq!(
        eval("var a = {}; a.sub = {}; a.sub.one = 1; a"),
        json!({"sub": {"one": 1.0}})
    );
    assert_exception!(
        "var a = {}; a.sub.one = 1", Exception::ReferenceNotAnObject
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
    assert_eval!("var a = 3; a /= a; a",    1.0);
    assert_eval!("var a = 13; a %= 8; a",   5.0);
    assert_eval!("var a = 1; a <<= 4; a",   16.0);
    assert_eval!("var a = 32; a >>= 4; a",  2.0);
    assert_eval!("var a = 32; a >>>= 4; a", 2.0);
    assert_eval!("var a = 6; a &= 9; a",    0.0);
    assert_eval!("var a = 6; a ^= 9; a",    15.0);
    assert_eval!("var a = 3; a |= 6; a",    7.0);
    //assert_eq!( eval("var a = 3; a **= a; a"),            JSValue::from(27));

    // Assignment of read-only variables:
    assert_eval!("undefined = 5; typeof undefined", "undefined");
    assert_eval!("undefined += 1; typeof undefined", "undefined");
}

#[test]
fn test_scope() {
    assert_eval!( "a = 1; a",   1.0 );
    assert_exception!( "b", Exception::ReferenceNotFound );
    assert_exception!( "a = a + 1", Exception::ReferenceNotFound );
    assert_exception!( "a += 1", Exception::ReferenceNotFound );

    assert_eval!("var a=1, b=a+1; b", 2.0);

    assert_eval!( "var a = false; { var a = true; } a",     true );
    assert_eval!( "var a = false; { a = true } a",          true );
    assert_eval!( "var a = false; (function() { a = true; })(); a", true );
    assert_eval!( "var a = true; (function(a) { a = false; })(); a", true );
    assert_eval!( "var a = true; (function(a) { a = false })('nope'); a", true );
    assert_eval!("(function() { a = true; })(); a", true);

    assert_eval!("var a; Object.getOwnPropertyDescriptor(this, 'a').configurable", false);
    assert_eval!("a = 1; Object.getOwnPropertyDescriptor(this, 'a').configurable", true);

    // block scope
    //assert_eval!( "var a = true; { let a = false; } a",     true );
    //assert_eval!( "let a = true; { let a = false; { let a = 'whut'; }}; a", true );

    // const variables
    //assert_exception!( "const a = true; a = false; a",  Exception::TypeErrorConstAssign );

    // variable hoisting
    assert_eval!("(function() { return a; var a = 12; })()", null);
    assert_eval!(r#"
        a = 1;  // Even a Program hoists its variables
        var a = 2;
        Object.getOwnPropertyDescriptor(this, 'a').configurable
    "#, false);

    // function hoisting
    assert_eval!(r#"
        // unlike variables, functions are declared *and* *initialized* when entering a scope.
        var a = false;
        hoisted();
        function hoisted() { a = true; }
        a
    "#, true);

    // closures
    assert_eval!(r#"
        var adder = function(y) { return function(x) { return x + y + z; } };
        var add3 = adder(3);
        var z = 1;
        add3(4)
    "#, 8.0);
    assert_eval!(r#"
        function adder(y) { return function(x) { return x + y + z; } };
        var z = 1;
        adder(3)(4)
    "#, 8.0);
    assert_eval!(r#"
        var z = 2;
        function adder(y) { var z = 0; return function(x) { return x + y + z; } };
        adder(3)(4)
    "#, 7.0);
    assert_eval!(r#"
        var obj = {z: 0};
        var adder = function(y) { return function(x) { return x + y + obj.z; } };
        var add3 = adder(3);
        obj.z += 1;
        add3(4)
    "#, 8.0);
    assert_exception!(r#"
        function adder(y) { return function(x) { return x + y + obj.z; } };
        adder(3)(4)
    "#, Exception::ReferenceNotFound);
}

#[test]
fn test_sequence() {
    assert_eval!("var a = 0; a += 2, a += 2", 4.0);
}

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
fn test_switch() {
    // does it work?
    assert_eval!("switch (true) {}", null);

    // it uses the right case
    assert_eval!("var a; switch (1) { case 1: a = true; break; default: a = false }; a", true);

    // it uses default
    assert_eval!("var a; switch (-1) { case 1: a = false; break; default: a = true }; a", true);

    // the strict comparison is used
    assert_eval!("var a; switch (1) { case '1': a = false; break; default: a = true }; a", true);
    assert_eval!("var a; switch (NaN) { case NaN: a = false; break; default: a = true }; a", true);

    // fallthrough without break
    assert_eval!("var a = ''; switch (1) { case 1: a += '1'; default: a += 'd' }; a", "1d");

    // default before case
    assert_eval!("var a; switch (-1) { default: a = true; break; case 1: a = false; }; a", true);

    // case understands expressions
    assert_eval!(r#"
        var a, one = 1;
        switch (1) { default: a = false; break; case one: a = true; };
        a
    "#, true);

    // break & switch & for
    assert_eval!(r#"
        var trace = ''; function t(point) { trace += point; };
        for (var i = 0; i < 4; ++i) {
          switch (i) {
            case 0:
            case 1: t('a'); break;
            default: t('d');
            case 2: t('b');
          }
        }
        trace
    "#, "aabdb");
}

#[test]
fn test_loops() {
    // for (<init>; <test>; <update)
    assert_eval!(r#"
        var a = 0;
        for (var i = 0; i < 5; i++) {
            a = a + 1;
        }
        a
    "#, 5.0);
    assert_eval!(r#"
        var a = 0, i = 0;
        for (i += 0; i < 5; i++) ++a;
        a
    "#, 5.0);

    assert_eval!(r#"
        var a = true;
        for (; false; ) a = false;
        a
    "#, true);

    // while
    assert_eval!(r#"
        var a = false;
        while (!a) { a = true }
        a
    "#, true);

    // do while
    assert_eval!(r#"
        var a = false;
        do { a = true } while (0);
        a
    "#, true);

    // break
    assert_eval!("for (;;) break; true", true);
    assert_eval!("c = 0; i = 5; while (--i) { ++c; break; ++c }; c", 1.0);

    // continue
    assert!( evalbool(r#"
        var a = false;
        while (!a) {
            a = true;
            continue;
            a = false;
            break;
        }
        a
    "#));

    // labeled break
    assert_eval!("c = 0; label: { ++c; break label; ++c; }; c", 1.0);
    assert_eval!(r#"
        var a = false;
        label: do {
            a = true;
            for (;;) { break label; }
            a = false;
        } while (0);
        a
    "#, true);

    // labeled continue
    assert_eval!(r#"
        c = 0;
        outer:
        for (var i = 0; i < 2; ++i) {
            for (var j = 0; j < 10; ++j) {
                ++c;
                continue outer;
            }
        }
        c
    "#, 2.0);
    assert_exception!(
        "c = 0; label: { ++c; continue label; ++c; }",
        Exception::SyntaxErrorContinueLabelNotALoop
    );

    // ForInStatement
    assert_eval!("for (var p in null) throw false", null);
    assert_eval!("for (var p in undefined) throw false", null);
    assert_eval!(r#"
        var obj = {one:1, two: 2, three: 3};
        var sum = 0; for (var prop in obj) sum += obj[prop]; sum
    "#, 6.0);
    assert_eval!(r#"
        var obj = {two: 2, three: 3};
        Object.defineProperty(obj, 'one', {value: 1}); // non-enumerable
        var sum = 0; for (var prop in obj) sum += obj[prop]; sum
    "#, 5.0);
    assert_eval!(r#"
        function Class() {};
        Class.prototype.one = 1;
        var obj = new Class();
        obj.two = 2;
        var sum = 0; for (var prop in obj) sum += obj[prop]; sum
    "#, 3.0);
    assert_eval!(r#"
        // non-enumerables do not participate in the loop,
        // but do shadow later prototypes properties, even enumerable.
        function Class() {};
        Class.prototype.one = 1;
        var obj = new Class();
        obj.two = 2;
        Object.defineProperty(obj, 'one', {value: 3});
        var sum = 0; for (var prop in obj) sum += obj[prop]; sum
    "#, 2.0);
    assert_eval!(r#"
        var s = 0; for (var i in [1, 2, 3]) s += i; s
    "#, 3.0);
    // TODO: ForInStatement string iteration
    // TODO: continue, break
    // TODO: labeled continue, break
}

#[test]
fn test_exceptions() {
    assert_exception!("throw ''", Exception::UserThrown);
    assert_eval!("try { nosuch; } catch(e) {}; true", true);
    assert_eval!("try { lol.whut; } catch(e) {}; true", true);
    assert_eval!("try { whut(); } catch(e) {}; true", true);
    assert_eval!("try { Boolean.prototype.valueOf.call({}); } catch(e) {}; true", true);
    assert_exception!(r#"
        try {
            throw 1;
        } catch (e) {
            throw 2;
        };
    "#, Exception::UserThrown);
    assert_eval!(r#"
        var trace = ''; function t(point) { trace += point; }
        try { throw ''; t('b'); }
        catch (e) { t('e'); }
        trace
    "#, "e");
    assert_eval!(r#"
        var trace = ''; function t(point) { trace += point; }
        try {
            for (var i = 0; i < 5; ++i) {
                throw i;
                t('b');
            }
        } catch(e) {
            t('e')
        }
        trace
    "#, "e");
    assert_eval!(r#"
        var trace = ''; function t(point) { trace += point; }
        for (var i = 0; i < 5; ++i) {
            try {
                if (i == 1) { t('1'); continue; }
                if (i == 2) { t('2'); throw '?'; }
                if (i == 4) { t('4'); break; }
                t('b');
            } catch(e) {
                t('e');
            }
        }
        trace
    "#, "b12eb4");
    assert_eval!(r#"
        var trace = ''; function t(point) { trace += point; }
        for (var i = 0; i < 6; ++i) {
            try {
                if (i == 1) { t('1'); continue; }
                if (i == 2) { t('2'); throw '?'; }
                if (i == 4) { t('4'); break; }
                t('b');
            }
            catch(e) { t('e'); }
            finally { t('f'); }
        }
        trace
    "#, "bf1f2efbf4f");
    /* TODO: let-variables and block scope
    assert_eval!(r#"
        function throws() { throw true; }
        var a = 0;
        try { throws(); a = false; }
        catch(e) { a = e };
        a
    "#, true);
    */
    // catch-variable is block scope:
    assert_eval!(r#"
        var e = true;
        try { throw false; } catch(e) {};
        e
    "#, true);
}

#[test]
fn test_unary_operations() {
    assert_eq!( eval("+1"),                 JSON::from(1.0) );
    assert_eq!( eval("+'1'"),               JSON::from(1.0) );
    assert_eq!( eval("+false"),             JSON::from(0.0) );
    assert_eq!( eval("+null"),              JSON::from(0.0) );
    assert!( evalbool("var v = +{}; v != v") );         // NaN
    assert!( evalbool("var v = +[1, 2]; v != v") );
    assert!( evalbool("var v = +'false'; v != v") );
    assert_eval!( "var a = []; +a",          0.0 );
    assert_eval!( "var a = [1]; +a",         1.0 );
    assert_eval!( "var a = +[1, 2]; a != a",  true);

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
    assert_eval!("typeof nosuch",      "undefined");
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

    assert_eq!( eval("var a = {one: 1}; delete a.one; a"),   json!({}) );
    assert!( evalbool("var a = {one: 1}; delete a.one") );
    assert!( evalbool("var a = {one: 1}; delete a['one']") );
    assert_eq!( eval("var a = {one: 1}; delete a.two; a"),   json!({"one": 1.0}) );
    assert!( evalbool("var a = {one: 1}; delete a.two") );
    assert_eval!( "delete undefined",     false ); // global.undefined is not configurable
    assert_eval!( "var a = 1; delete a",  false); // vars are not configurable
    assert_eval!( "a = 1; delete a",      true ); // but these are.
    assert_eval!( "delete 0",             true ); // don't ask.
    assert_eval!( "delete nosuch", true );
    //assert_eval!( "var a = ['one', 'two']; delete a[2]", true );
    //assert_eval!("var a = ['one', 'two']; delete a[1]", true);
    //assert_eval!("var a = ['one', 'two']; delete a[0]; a[0] === undefined", true);
}

#[test]
fn test_update_operations() {
    assert_eval!("var a = 0; ++a", 1.0);
    assert_eval!("var a = 1; ++a; a", 2.0);
    assert_eval!("var a = 0; a++", 0.0);
    assert_eval!("var a = 0; a++; a", 1.0);
    assert_eval!("var a = 1; --a", 0.0);
    assert_eval!("var a = 1; --a; a", 0.0);
    assert_eval!("var a = 1; a--", 1.0);
    assert_eval!("var a = 1; a--; a", 0.0);

    assert_eval!("var a = false; ++a",  1.0);
    assert_eval!("var a = false; ++a; a", 1.0);
    assert_eval!("var a = [2]; ++a",    3.0);
    assert_eval!("var a = {}; ++a",     (f64::NAN));
    assert_eval!("var a = 'nope'; ++a", (f64::NAN));
    assert_eval!("var a = '5'; ++a",    6.0);
    assert_eval!("++undefined",         (f64::NAN));
}

#[test]
fn test_functions() {
    // CallExpression for builtin functions
    assert_eval!( "parseInt('42')",     42.0);

    // FunctionExpression
    assert_eval!( r#"
        var twice = function(x) { return x + x; };
        twice(12)
    "#, 24.0);

    // a function returns a value
    assert_eval!( "(function () { return true; })()",   true );

    // return returns immediately
    assert_eval!(r#"
        var func = function() { return true; return false; };
        func()
    "#, true);

    // the arguments are always fresh
    assert_eval!(r#"
        var twice = function(x) { return x + x; };
        twice(12)
        twice('a')
    "#, "aa");

    // function scope
    assert_eval!(r#"
        var x = 'outer';
        function twice(x) { return x + x; };
        twice('inner')
        x
    "#, "outer");

    // `arguments`
    assert_eval!("(function() { return arguments; })(1, 2)",  [1.0, 2.0]);
    assert_eval!("(function() { return typeof arguments; })()",  "object");
    assert_eval!("(function(arguments){ return arguments; })(true)", true);

    // recursive functions
    assert_eval!(r#"
        var gcd = function(a, b) { return (a == b ? a : (a < b ? gcd(a, b-a) : gcd(a-b, b))); };
        gcd(12, 15)
    "#, 3.0);

    // immediate/anonymous function call
    assert_eval!( "(function(x) { return x + x; })(12)",   24.0);
    assert_eval!(r#"
        var a = [function() { return 8; }, function() { return 12; }];
        a[0]()
    "#, 8.0);

    // closures
    assert_eval!(r#"
        var adder = function(y) { return function(x) { return x + y; } };
        var add3 = adder(3);
        add3(4)
    "#, 7.0);

    // FunctionDeclaration
    assert_eval!(r#"
        function sqr(x) { return x * x; };
        sqr(12)
    "#, 144.0);

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
    //assert_eval!( "parseInt(new Number(64))", 64.0 );
    assert_eval!( "parseInt(new String(64))", 64.0 );
    assert_eval!( "parseInt(true)", (f64::NAN));
    assert_eval!( "parseInt(null)", (f64::NAN));
    assert_eval!( "parseInt({})", (f64::NAN));

    // parseFloat()
    assert_eval!("parseFloat('0')", 0.0);
    assert_eval!("parseFloat('-0')", (-0.0));
    assert_eval!("parseFloat('.1e1')", (1.0));
    //assert_eval!("parseFloat('1.23abc')", 1.23);
    assert_eval!("parseFloat()", (f64::NAN));
    assert_eval!("parseFloat('whut')", (f64::NAN));
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
    assert_eval!( "Object(false) instanceof Boolean", true );
    //assert_eval!( "Object(1) instanceof Number", true );

    assert_eval!( "new Object(null)",     {} );
    assert_eval!( "new Object(undefined)", {} );
    assert_eval!( "new Object({one: 1})", {"one": 1.0} );
    assert_eval!("new Object(true) instanceof Boolean", true);
    //assert_eval!( "new Object(1) instanceof Number", true );

    // Object.defineProperties
    assert_eval!(r#"
        var obj = {};
        Object.defineProperties(obj, {one: {value: 1}, two: {value: 2}});
        obj.one + obj.two
    "#, 3.0);
    //assert_exception!(
    //  "Object.defineProperties({}, 'a')",
    //  Exception::TypeErrorInstanceRequired
    //);
    //assert_exception!(
    //  "Object.defineProperties({}, ['a'])",
    //  Exception::TypeErrorInvalidDescriptor
    //);

    // Object.defineProperty
    //assert_exception!("Object.defineProperty(1)", Exception::TypeErrorInstanceRequired);
    //assert_exception!("Object.defineProperty(null)", Exception::TypeErrorInstanceRequired);
    assert_eval!(r#"
        var obj = {};
        Object.defineProperty(obj, 'prop', {value: 42});
        var d = Object.getOwnPropertyDescriptor(obj, 'prop');
        !d.enumerable && !d.writable && !d.configurable
    "#, true);
    assert_eval!(r#"
        var obj = {};
        Object.defineProperty(obj, 'prop', {});
        // `value` defaults to undefined:
        obj.prop === undefined
    "#, true);
    assert_eval!(r#"
        var obj = {};
        Object.defineProperty(obj, 'prop', {value: 42, enumerable: true});
        var d = Object.getOwnPropertyDescriptor(obj, 'prop');
        d.enumerable && !d.writable && !d.configurable
    "#, true);
    assert_eval!(r#"
        var obj = {};
        Object.defineProperty(obj, 'prop', {enumerable: true});
        var d = Object.getOwnPropertyDescriptor(obj, 'prop');
        d.enumerable && !d.writable && !d.configurable
    "#, true);
    /*
    assert_eval!(r#"
        var obj = { val: 42 };
        Object.defineProperty(obj, 'prop', {
            get: function() { return this.val; },
            enumerable: true
        });
        var d = Object.getOwnPropertyDescriptor(obj, 'prop');
        (obj.prop == 42) && (d.set == undefined) &&
            d.enumerable && !d.writable && !d.configurable
    "#, true);
    assert_eval!(r#"
        var obj = { val: false };
        Object.defineProperty(obj, 'prop', {
            set: function(val) { this.val = val; },
            dreck: true,
        });
        obj.prop = true;
        obj.val
    "#, true);
    */
    assert_exception!(r#"
        var obj = { val: 42 };
        Object.defineProperty(obj, 'prop', {
            get: function() { return this.val; },
            writable: true
        })
    "#, Exception::TypeErrorInvalidDescriptor);

    // Object.getOwnPropertyDescriptor
    assert_eval!(r#"
        var d = Object.getOwnPropertyDescriptor(Object, 'prototype');
        !d.writable && !d.configurable && !d.enumerable
    "#, true);
    assert_eval!(r#"
        var d = Object.getOwnPropertyDescriptor(Object, 'getOwnPropertyDescriptor');
        d.configurable && !d.enumerable && d.writable
    "#, true);

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
        var obj = {one: 1};
        var copy = Object.assign({}, obj);
        obj.one
    "#, 1.0);
     */
    // Object.create()
    assert_eval!("var p = {prop: true}; var o = Object.create(p); o.prop", true);
    assert_eval!("var p = {prop: false}; var o = Object.create(p); p.prop = true; o.prop", true);
    assert_eval!("'toString' in Object.create(null)", false);
    assert_exception!("Object.create(true)", Exception::TypeErrorInvalidPrototype);
    assert_eval!("var o = Object.create(null, {one: {value: 1}}); o.one", 1.0);

    // Object.entries()
    // Object.keys()
    // Object.values()
    // Object.freeze()
    // Object.getOwnPropertyDescriptors()
    // Object.getOwnPropertyNames()
    // Object.getPrototypeOf()

    // Object.setPrototypeOf()
    assert_eval!(r#"
        var obj = Object.create({a: false});
        Object.setPrototypeOf(obj, {a: true});
        obj.a
    "#, true);
    assert_eval!(r#"
        var obj = Object.create({a: true});
        Object.setPrototypeOf(obj, 'do nothing');
        obj.a
    "#, true);
    assert_eval!(r#"
        var obj = Object.create({a: true});
        Object.setPrototypeOf(obj, null);
        obj.a
    "#, null);
    //TODO: check if the object is extensible

    // Object.fromEntries()
    // Object.isExtensible()
    // Object.isFrozen()
    // Object.isSealed()
    // Object.preventExtensions()
    // Object.seal()

    // Object.prototype.hasOwnProperty()
    assert_eval!("Object.hasOwnProperty('create')", true);
    assert_eval!("Object.hasOwnProperty('toString')", false); // it's on the prototype, not own one
    assert_eval!("[1].hasOwnProperty(0)", true);
    assert_eval!("[1].hasOwnProperty(1)", false);
    assert_eval!("this.hasOwnProperty()", true); // yes, global has a property called `undefined`

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

    // Function.length
    assert_eval!("var sqr = function(x) { return x*x; }; sqr.length",  1.0);

    // Function.prototype.call()
    assert_eval!(r#"
        var a = [function() { return 8; }, function() { return 12; }];
        a[1].call()
    "#, 12.0);
}

#[test]
fn test_builtin_boolean() {
    assert_eval!("true instanceof Boolean", false);

    // auto-objectification:
    assert_eval!("true.toString()",   "true");
    assert_eval!("false.toString()",  "false");
    assert_eval!("var a = {b: true}; a.b.toString()",  "true");

    // Boolean()
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

    // Boolean.prototype.valueOf()
    assert_eval!("new Boolean().valueOf()", false);
    assert_eval!("new Boolean(1).valueOf()", true);
}

#[test]
fn test_builtin_string() {
    // auto-objectification:
    assert_eval!("'aaa'.length",   3.0);
    assert_eval!("var a = {b: 'hello'}; a.b.length", 5.0);

    // addition
    assert_eval!("'con' + 'cat'", "concat");
    assert_eval!("var s = 'con'; s += 'cat'; s", "concat");

    // char access
    assert_eval!("'abc'[1]", "b");
    assert_eval!("'привіт'[2]", "и");

    // String()
    assert_eval!("String()", "");
    assert_eval!("String(true)", "true");
    assert_eval!("String(undefined)", "undefined");
    assert_eval!("String({})", "[object Object]");
    assert_eval!("String(NaN)", "NaN");
    assert_eval!("String([1,2])", "1,2");

    // new String()
    assert_eval!("new String().valueOf()", "");
    assert_eval!("new String(true).valueOf()", "true");
    assert_eval!("new String(undefined).valueOf()", "undefined");
    assert_eval!("new String({}).valueOf()", "[object Object]");
    assert_eval!("new String(NaN).valueOf()", "NaN");
    assert_eval!("new String([1,2]).valueOf()", "1,2");

    // String.prototype.valueOf()
    assert_eval!("'str'.valueOf()", "str");
    assert_eval!("'str'.toString()", "str");
    //assert_exception!("String.prototype.valueOf.call(1)", Exception::TypeErrorInstanceRequired);

    // String.prototype.charAt()
    assert_eval!("'abc'.charAt(1)", "b");
    assert_eval!("'abc'.charAt(4)", "");
    assert_eval!("'abc'.charAt(-1)", "");
    assert_eval!("'abc'.charAt()", "a");
    assert_eval!("'привіт'.charAt(2)", "и");
    assert_eval!("String.prototype.charAt.call(true, 1)", "r");

    // String.prototype.charCodeAt()
    assert_eval!("'abc'.charCodeAt(1)", 98.0);
    assert_eval!("'привіт'.charCodeAt(2)", 1080.0);
    assert_eval!("''.charCodeAt(0)", (f64::NAN));
    assert_eval!("String.prototype.charCodeAt.call(false, 1)", 97.0);

    // String.prototype.slice()
    assert_eval!("'abc'.slice()", "abc");
    assert_eval!("'abc'.slice('?')", "abc");
    assert_eval!("'abcde'.slice(3)", "de");
    assert_eval!("'abcde'.slice(-3)", "cde");
    assert_eval!("'abcde'.slice(-4, 2)", "b");
    assert_eval!("'abcde'.slice(-5, 2)", "ab");
    assert_eval!("'abcde'.slice(-8, 2)", "ab");
    assert_eval!("'abcde'.slice(6)", "");
    assert_eval!("'abcde'.slice(1, 3)", "bc");
    assert_eval!("'abc'.slice(1, 10)", "bc");
    assert_eval!("'abcde'.slice('1', '3')", "bc");
    assert_eval!("'abcde'.slice(1, -2)", "bc");
    assert_eval!("'abcde'.slice(0, -5)", "");
    assert_eval!("'abcde'.slice(0, -6)", "");
    assert_eval!("'abcde'.slice(1, '?')", "");
    assert_eval!("'abcde'.slice(3, 1)", "");
    assert_eval!("'abcde'.slice(-1, -3)", "");
    assert_eval!("String.prototype.slice.call(true, 2)", "ue");
    //assert_eval!("String.prototype.slice.call(123, 1)", "23");

    // String.prototype.substr
    assert_eval!("'abcde'.substr(2)", "cde");
    assert_eval!("'abcde'.substr(10)", "");
    assert_eval!("'abcde'.substr(-1)", "e");
    assert_eval!("'abcde'.substr(-6)", "abcde");
    //assert_eval!("'abcde'.substr(Number.NEGATIVE_INFINITY)", "abcde");
    assert_eval!("'abcde'.substr(1, 2)", "bc");
    assert_eval!("'abcde'.substr(3, -2)", "");
    assert_eval!("'abcde'.substr(NaN, 3)", "abc");
    assert_eval!("'abcde'.substr('zz', 3)", "abc");
    assert_eval!("String.prototype.substr.call(true, 2)", "ue");

    // String.prototype.indexOf()
    assert_eval!("'abcde'.indexOf('bc')", 1.0);
    assert_eval!("'abcde'.indexOf('bce')", (-1.0));
    assert_eval!("'абвгд'.indexOf('гд')", 3.0);
    assert_eval!("'abcdeabcd'.indexOf('ab', 3)", 5.0);
    assert_eval!("'abcde'.indexOf('ab', -3)", 0.0);
    assert_eval!("'abcde'.indexOf('ab', 6)", (-1.0));
    assert_eval!("'abcde'.indexOf('')", 0.0);
    assert_eval!("'abcde'.indexOf('', 3)", 3.0);
    assert_eval!("'abcde'.indexOf('', 7)", 5.0);
    assert_eval!("'abcde'.indexOf('Ab')", (-1.0));
    assert_eval!("'abcdefghi'.indexOf('ab', 3)", (-1.0));
    assert_eval!("'undefined'.indexOf()", 0.0);

}

#[test]
fn test_builtin_error() {
    assert_eval!("void new Error()", null);
    assert_eval!("var e = Error('testing'); e.message", "testing");
    assert_eval!("var e = new Error('testing'); e.message", "testing");

    // Error.prototype.toString()
    assert_eval!("new Error('just testing').toString()", "Error: just testing");
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
        var HasOne = function() { this.one = 1; }
        var obj = new HasOne()
        obj.one
    "#, 1.0);
    assert_eval!(r#"
        var HasOne = function() { this.one = 1; }
        var obj1 = new HasOne()
        var obj2 = new HasOne()
        obj2.one = 2;
        obj1.one + obj2.one
    "#, 3.0);
    assert_eval!(r#"
        var Class = function() { return {one: 1}; }
        var obj = new Class()
        obj.one
    "#, 1.0);
    assert_eval!(r#"
        var HasOne = function() { this.one = 1; }
        var obj1 = new HasOne()
        var obj2 = new HasOne()
        HasOne.prototype.two = 2
        obj1.two + obj2.two
    "#, 4.0);
}

#[test]
fn test_this() {
    assert_eval!("var f = function() { return this; }; f() == global", true );

    assert_eval!(r#"
        var test = { prop: 42, func: function() { return this.prop; } };
        test.func()
    "#, 42.0 );

    // call
    assert_eval!(r#"
        function add(c, d) { return this.a + this.b + c + d }
        var o = {a: 1, b: 3}
        add.call(o, 5, 7)
    "#, 16.0);

    // apply
    assert_eval!(r#"
        function f(c, d) { return this.a + this.b + c + d }
        var o = {a: 1, b: 3}
        f.apply(o, [5, 7])
    "#, 16.0);

    assert_eval!(r#"
        var o = {f: function() { return this.a + this.b; }}
        var p = Object.create(o)
        p.a = 1
        p.b = 4
        p.f()
    "#, 5.0 );

    /*
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
        var Class = function() { this.prop = true; }
        var obj = new Class();
        obj.prop
    "#, true);
}

#[test]
fn test_arrays() {
    assert_eval!( "[]",   [] );
    assert_eval!( "[1, 'a', undefined]",   [1.0, "a", null] );

    assert_eval!( "var a = ['zero', 'one']; a[1]",  "one" );
    assert_eval!( "var a = ['zero', 'one']; a[2]",  null );

    assert_eval!( "var a = ['zero', 'one']; a[2] = 'two'; a[2]", "two" );
    assert_eval!( "var a = ['zero', 'one']; a[1] = 'один'; a[1]", "один" );

    // .length
    assert_eval!( "var a = ['zero', 'one']; a.length", 2.0 );
    assert_eval!( "var a = ['zero', 'one']; a[2] = 'two'; a.length", 3.0 );

    // Array.prototype.push()
    assert_eval!("var a = []; a.push(1); a", [1.0]);
    assert_eval!("var a = []; a.push(true); a.length", 1.0);

    assert_eval!("var a = []; a.push(); a", []);
    assert_eval!("var a = []; a.push(1, 2, 3); a", [1.0, 2.0, 3.0]);
    assert_eval!(r#"
      var a = [1, 2];
      var b = [3, 4];
      Array.prototype.push.apply(a, b);
      a
    "#, [1.0, 2.0, 3.0, 4.0]);
    assert_eval!("var a = []; a.push('len++')", 1.0);  // return value is the new length
    /*
    assert_eval!(r#" // generic use
        var obj = {length: 3.2};
        Array.prototype.push.call(obj, 8);
        obj[4]*10 + obj.length
    "#, 84.0);
    */

    // Array.prototype.pop()
    assert_eval!("[].pop()", null);
    assert_eval!("[true].pop()", true);
    assert_eval!("[1, 2].pop()", 2.0);
    assert_eval!("var a = ['one', 'two', 'three']; a.pop(); a.length", 2.0);
    /*
    assert_eval!(r#"
        var obj = {0:'one', 1:'two', length: 2};
        Array.prototype.pop.call(obj);
        a.length
    "#, 1.0);
    */
}

/// ```sh
/// $ cargo -q test --lib sizes -- --nocapture
/// ```
#[test]
fn test_sizes() {
    use std::mem::size_of;

    use crate::heap;
    use crate::function;
    use object::{Access, Content, Interpreted, ObjectValue, Property};

    println!("============================");
    println!("size_of JSRef:  \t{}", size_of::<heap::JSRef>());
    println!("size_of JSValue:\t{}", size_of::<JSValue>());
    println!("size_of Interpreted:\t{}", size_of::<Interpreted>());
    println!("size_of JSObject:\t{}", size_of::<object::JSObject>());
    println!("size_of   HashMap:\t{}", size_of::<std::collections::HashMap<String, Property>>());
    println!("size_of   ObjectValue:\t{}", size_of::<ObjectValue>());
    println!("size_of     JSArray:\t{}", size_of::<object::JSArray>());
    println!("size_of     NativeFunc:\t{}", size_of::<function::NativeFunction>());
    println!("size_of     Closure:\t{}", size_of::<function::Closure>());
    println!("size_of Property:\t{}", size_of::<Property>());
    println!("size_of   Access:\t{}", size_of::<Access>());
    println!("size_of   Content:\t{}", size_of::<Content>());
    println!("============================");
}

// this is for one-off experiments, don't commit anything here:
#[test] fn test_scratch() {
}
