#[rustfmt::skip]
mod parse {

use serde_json::json;

use slothjs::{
    Exception,
    Program,
    runtime::{self, Runtime, EvalError},
    ast::{expr, stmt},
};

type Parser = runtime::NodejsParser;

fn evalbool(input: &str) -> bool {
    let parser = Parser::new();
    let mut js = Runtime::load(Box::new(parser)).unwrap();
    let result = js.evaluate(input).unwrap();
    result.boolify(&js.heap)
}

/// Ensures that the parses produces the given [`Program`]
macro_rules! assert_parse {
    ($code:literal, $program:expr) => {
        assert_parse!($code, $program, "");
    };
    ($code:literal, $program:expr, $desc:literal) => {
        let mut sljs = Runtime::load(Box::new(Parser::new())).expect("Runtime::load");
        let want = Program::from_stmt($program);
        let got = sljs.parse($code).expect("Runtime::parse");
        assert_eq!(got, want, $desc);
    }
}

/// Runs interpretation of the first argument (a string literal),
/// then compares the result to the second argument (anything that `serde_json::json!`
/// understands).
macro_rules! assert_eval {
    ($js:literal, $json:tt) => {
        let mut js = Runtime::load(Box::new(Parser::new())).expect("Runtime::load");
        let expected = json!($json);
        match js.evaluate($js) {
            Ok(result) => {
                assert_eq!( js.json_from(result), expected )
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
/// assert_exception!( "bla", Exception::Reference );
/// ```
// TODO: look if it's possible to match exception arguments as well:
// ```
// assert_exception!( "bla", Exception::Reference("bla") );
// ```
macro_rules! assert_exception {
    ($js:literal, $exc:path) => {
        let mut js = Runtime::load(Box::new(Parser::new())).expect("Runtime::load");
        match js.evaluate($js) {
            Err(EvalError::Exception($exc(_))) => (),
            other => {
                panic!("\n   want {}\n   got: {:?}\n", stringify!($exc), other)
            }
        }
    };
}

#[test]
fn literals() {
    assert_parse!( "null",      expr::null());
    assert_parse!( "true",      true);
    assert_parse!( "42",         expr::lit(42));
    assert_parse!( "0x2a",       expr::lit(42));
    assert_parse!( "052",        expr::lit(42));
    assert_parse!( "[]",         expr::empty_array());
    assert_parse!( "+5",         expr::plus(5));
    assert_parse!( "+'5'",       expr::plus("5"));
    assert_parse!(
        "\"hello \\\"world\\\"\"",
        expr::lit("hello \"world\""),
        "escaped quotes in strings"
    );
    assert_parse!("({one:1, two:2})",
        expr::object(vec![
            (expr::id("one"), expr::lit(1)),
            (expr::id("two"), expr::lit(2)),
        ])
    );
}

#[test]
fn binary_plus() {
    assert_parse!("2 + 2",         expr::add(2, 2));
    assert_parse!("'1' + '2'",     expr::add("1", "2"));
    assert_parse!("[1] + [2,3]",   expr::add(expr::array(vec![1]), expr::array(vec![2, 3])));
    assert_parse!("true + null",   expr::add(true, expr::null()));
    assert_parse!( "'' + [1, 2]",  expr::add("", expr::array(vec![1, 2])));
    assert_parse!( "'' + null",    expr::add("", expr::null()));
    assert_parse!( "'' + {}",      expr::add("", expr::empty_object()));
    assert_parse!( "({} + {})",    expr::add(expr::empty_object(), expr::empty_object()));
    assert_parse!( "({} + [])",    expr::add(expr::empty_object(), expr::empty_array()));
    assert_parse!( "{} +[]",       stmt::block(vec![
            stmt::block(vec![]).into(),
            expr::plus(expr::empty_array()).into()
    ]) );
    assert_parse!("undefined+undefined", expr::add(expr::undefined(), expr::undefined()));
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
    assert_eval!( "['zero', 'one', 'two'][2]",      "two" );
    assert_eval!( "var o = {one: 1}; o.one",        1.0 );
    assert_eval!( "var a = {}; a.one = 1; a",       {"one": 1.0});
    assert_eval!( "var o = {'o e': 1}; o['o e']",   1.0);
    assert_eval!( "var x = 'one'; var o = {[x]: 1}; o", {"one": 1.0});
    assert_eval!( "var a = {}; a.sub = {}; a.sub.one = 1; a", {"sub": {"one": 1.0}});
    assert_exception!(
        "var a = {}; a.sub.one = 1", Exception::Reference
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
    //assert_eval!("var a = 3; a **= a; a",   27.0);

    // Assignment of read-only variables:
    assert_eval!( "var NaN = 5; NaN != NaN", true );
    assert_eval!("undefined = 5; typeof undefined", "undefined");
    assert_eval!("undefined += 1; typeof undefined", "undefined");

    assert_exception!("var var = 'var'",      Exception::Syntax);

    // let-bindings
    assert_eval!( "let a = 1; a = 2; a",    2.0 );
    assert_eval!( "let a = 1; a += 1; a",   2.0 );
    assert_eval!( "let a = 1; a += 1",      2.0 );
    assert_eval!( "let a = 3; a *= a; a",   9.0 );
    assert_eval!( "let a = 1; a -= 1; a",   0.0 );
    assert_eval!("let a = 3; a /= a; a",    1.0);
    assert_eval!("let a = 13; a %= 8; a",   5.0);
    assert_eval!("let a = 1; a <<= 4; a",   16.0);
    assert_eval!("let a = 32; a >>= 4; a",  2.0);
    assert_eval!("let a = 32; a >>>= 4; a", 2.0);
    assert_eval!("let a = 6; a &= 9; a",    0.0);
    assert_eval!("let a = 6; a ^= 9; a",    15.0);
    assert_eval!("let a = 3; a |= 6; a",    7.0);
    //assert_eval!("let a = 3; a **= a; a",   27.0);

    // const-bindings
    /*
    assert_exception!( "const a = 1; a = 2; a",    Exception::Type ); // "const assign"
    assert_exception!( "const a = 1; a += 1; a",   Exception::Type ); // "const assign"
    assert_exception!( "const a = 1; a += 1",      Exception::Type ); // "const assign"
    assert_eval!( "const a = 1; if (0) a = 2; a",  1.0 );  // not a syntax error
    */

    // TODO: destructuring assignment
    //assert_eval!( "let obj = { key: 42 }; let { k } = obj; k", 42.0 );
}

#[test]
fn test_global_scope() {
    assert_eval!( "a = 1; a",   1.0 );
    assert_exception!( "b", Exception::Reference );
    assert_exception!( "a = a + 1", Exception::Reference );
    assert_exception!( "a += 1", Exception::Reference );
}

#[test]
fn test_function_scope() {
    assert_eval!("var a=1, b=a+1; b", 2.0);

    assert_eval!( "var a = false; { var a = true; } a",     true );
    assert_eval!( "var a = false; { a = true } a",          true );
    assert_eval!( "var a = false; (function() { a = true; })(); a", true );
    assert_eval!( "var a = true; (function(a) { a = false; })(); a", true );
    assert_eval!( "var a = true; (function(a) { a = false })('nope'); a", true );
    assert_eval!("(function() { a = true; })(); a", true);

    assert_eval!("var a; Object.getOwnPropertyDescriptor(this, 'a').configurable", false);
    assert_eval!("a = 1; Object.getOwnPropertyDescriptor(this, 'a').configurable", true);

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
    "#, Exception::Reference);

    // eval
    // TODO: eval can change function scope
}

#[test]
fn test_block_scope() {
    assert_eval!( "let a; a", null );
    assert_eval!( "let a, b = '2', c; a = '1'; c = '3'; a + b + c", "123" );
    assert_eval!( "var a = true; { let a = false; } a",     true );
    assert_eval!( "let a = true; { let a = false; } a",     true );
    assert_eval!( "let a = false; { a = true; } a",         true );
    assert_eval!( "let a = true; { let a = false; { let a = 'whut'; }}; a", true );

    assert_exception!("{ let a = 'should not leak'; }; a", Exception::Reference );
    /*
    assert_exception!("typeof x; let x = 54; x", Exception::Reference);
    assert_exception!("var a; { a = b; let b = 2; }; a", Exception::Reference);
    assert_exception!(r#"{
        let a = 'a1';
        { a = b;   /* ReferenceError: b is not initialized yet */ };
        let b = 'b1';
    }"#, Exception::Reference);
    assert_exception!(  // "Cannot access 'a' before initialization"
        "var a = 'a', b; { b = a; let a = 'A'; }; b",
        Exception::Reference);


    assert_exception!("let foo; let foo;", Exception::SyntaxErrorAlreadyDeclared);
    assert_exception!("let foo; var foo;", Exception::SyntaxErrorAlreadyDeclared);
    assert_exception!("var foo; let foo;", Exception::SyntaxErrorAlreadyDeclared);
    assert_exception!("function f() { var a; let a; }", Exception::SyntaxErrorAlreadyDeclared);
    */

    assert_eval!(       "var let = true; let",  true);
    assert_exception!(  "let let = 'let'; let", Exception::Syntax);
    assert_exception!(  "let var = 'var'; var", Exception::Syntax);

    assert_exception!("if (true) let a = 1", Exception::Syntax); //valid only in a block

    assert_eval!(r#"
        // let-bindings used from a function are evaluated on call site.
        let func = function() { return letvar; };
        let letvar = true;
        func()
    "#, true);
    // TODO: let-bindings in for (let i=0; ...)
    // TODO: eval introduces its own block scope

    // Closures capture let-bindings:
    /*
    assert_eval!(r#"
        let a=0; function inca() { a += 1 };
        function callinc(f) { f(); }; callinc(inca);
        a
    "#, 1.0);
    */
    assert_eval!(r#"
        function f() {     // captures variables of the same name from different blocks
            let a = 0; var c0 = {add: function(b) { a += b }, get: function() { return a }};
            { let a = 0; var c1 = {add: function(b) { a += b }, get: function() { return a }}};
            return [c0, c1];
        }
        cs = f(); cs[0].add(2); cs[1].add(5);
        [cs[0].get(), cs[1].get()]
    "#, [2.0, 5.0]);
}


#[test]
fn test_sequence() {
    assert_eval!("var a = 0; a += 2, a += 2", 4.0);
}

#[test]
fn test_blocks() {
    assert_eval!(r#"
        var a = 1, b = 2;
        { a = 10; b = 20 };
        a + b
    "#, 30.0);
}

#[test]
fn test_conditionals() {
    assert!( evalbool("'0' ? true : false"));
    assert!( !evalbool("0 ? true : false"));
    assert!( evalbool("({} ? true : false)") );

    assert_eval!( "var a; if (a = 1) a = 2; else a = 3; a", 2.0 );
    assert_eval!( "var a = 1; if (null) { a = 2; }; a",     1.0 );
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
        Exception::Syntax
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
    assert_eval!(r#"
        var a = 0;
        try {
            throw true;
            a = false;
        } catch(e) {
            a = e;
        };
        a
    "#, true);
    // catch-variable is block scope:
    assert_eval!(r#"
        var e = true;
        try { throw false; } catch(e) {};
        e
    "#, true);
}

#[test]
fn test_unary_operations() {
    assert_eval!( "+1",                 1.0 );
    assert_eval!( "+'1'",               1.0 );
    assert_eval!( "+false",             0.0 );
    assert_eval!( "+null",              0.0 );
    assert!( evalbool("var v = +{}; v != v") );         // NaN
    assert!( evalbool("var v = +[1, 2]; v != v") );
    assert!( evalbool("var v = +'false'; v != v") );
    assert_eval!( "var a = []; +a",          0.0 );
    assert_eval!( "var a = [1]; +a",         1.0 );
    assert_eval!( "var a = +[1, 2]; a != a",  true);

    assert_eval!( "-'1'",               (-1.0));

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

    assert_eval!( "~-1",                0.0 );
    assert_eval!( "~-2",                1.0 );
    assert_eval!( "~2",                 (-3.0));
    assert_eval!( "~2",                 (-3.0));
    assert_eval!( "~NaN",               (-1.0));
    assert_eval!( "~{}",                (-1.0));
    assert_eval!( "~~''",               0.0 );
    assert_eval!( "~~'whut'",           0.0 );

    assert_eval!( "typeof void 'nope'", "undefined" );
    assert_eval!( "typeof void {}",     "undefined" );

    assert_eval!( "var a = {one: 1}; delete a.one; a",   {} );
    assert!( evalbool("var a = {one: 1}; delete a.one") );
    assert!( evalbool("var a = {one: 1}; delete a['one']") );
    assert_eval!( "var a = {one: 1}; delete a.two; a",   {"one": 1.0} );
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
    //  Exception::Type  // InstanceRequired
    //);
    //assert_exception!(
    //  "Object.defineProperties({}, ['a'])",
    //  Exception::Type  // InvalidDescriptor
    //);

    // Object.defineProperty
    //assert_exception!("Object.defineProperty(1)", Exception::Type);  // InstanceRequired
    //assert_exception!("Object.defineProperty(null)", Exception::Type);  // InstanceRequired
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
    "#, Exception::Type); // InvalidDescriptor

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
    assert_exception!("Object.create(true)", Exception::Type);
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
    assert_exception!("null.valueOf()",  Exception::Type); // not callable
}

#[test]
fn test_builtin_function() {
    assert_eval!(
        "Object.getOwnPropertyDescriptor(global, 'Function')",
        {"writable": true, "enumerable": false, "configurable": true,  "value": {}}
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
    //assert_exception!("String.prototype.valueOf.call(1)", Exception::Type);

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

    // String.prototype.replace()
    assert_eval!("'test'.replace('t', 'T')", "Test");
    assert_eval!("'test'.replace('es', '')", "tt");
    assert_eval!("'test'.replace('', 'passed ')", "passed test");
    assert_eval!(r#"
        'слава Україні'.replace('', 'героям ').replace(' Україні', '')
    "#, "героям слава");
    //assert_eval!("String.prototype.replace.call(202, '2', '3')", "302");
    assert_eval!("String.prototype.replace.call(true, '', 'un')", "untrue");  // generic over this
    assert_eval!("'test'.replace('t', 1)", "1est"); // replaceValue.toString() is used
    assert_eval!("'aaa'.replace('a', '$$')", "$aa");
    assert_eval!("'test'.replace('es', '$&$&')", "tesest");
    assert_eval!(r#"'test'.replace('es', "$$'$$")"#, "t$'$t");
    assert_eval!(r#"'abc'.replace('b', "$`")"#, "aac");
    assert_eval!(r#"'abc'.replace('b', "$'")"#, "acc");
    /*
    assert_eval!("'test'.replace('t', function() { return 1 })", "1est"); // replaceValue can be a function.
    assert_eval!(r#"
        "__test__".replace("t", function(match, offset, str) {
            return '<'+match+','+offset+','+str+'>';
        })
    "#, "__t<t,2,__test__>est__");
    */
    //TODO: replace(RegExp, ...)
    //assert_eval!(r"'$1,$2'.replace(/(\$(\d))/g, '$$1-$1$2')", "$1-$11,$1-$22");
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
    assert_eval!( "var x = 'one'; var o = {[x]: 1}; o.one", 1.0);
    assert_eval!( "var a = [1]; a[0] = 2; a[0]",    2.0);
    assert_eval!( "var a = {v: 1}; a.v = 2; a.v",   2.0);
    assert_eval!( "var a = {}; a.one = 1; a",       {"one": 1.0});

    assert_eval!(
        "var a = {}, b = {}; a.b = b; b.one = 1; a",
        {"b": {"one": 1.0}}
    );
    assert_eval!( "var a = {b: 2}; var b = a.b; b = 1; a.b", 2.0 );
    assert_eval!( "var a = {b: {}}; var b = a.b; b.one = 1; a.b.one", 1.0 );

    assert_exception!( "a.one = 1", Exception::Reference );

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

    // Array.prototype.indexOf()
    /* TODO
    assert_eval!("[].indexOf()", (-1.0));
    assert_eval!("[undefined].indexOf()", 0.0);
    assert_eval!("['a', 'b', 'c'].indexOf('b')", 1.0);
    assert_eval!("['a', 'b', 'c'].indexOf('d')", (-1.0));

    assert_eval!("['a', 'b', 'a'].indexOf('a', 1)", (2.0));  // second argument: fromIndex
    assert_eval!("['a', 'b', 'a'].indexOf('a', -2)", (2.0)); // second argument: fromIndex from end
    assert_eval!(r#"
        let arraylike = {length: 3, 0: 'a', 1: 'b', 2: 'a'};
        Array.prototype.indexOf.call(arraylike, 'a', 1)
    "#, 2.0);
    */
}

#[test]
fn test_eval() {
    assert_eval!("eval('2 + 2')",  4.0);
}

}
