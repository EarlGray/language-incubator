slothjs - a naïve, primitive, savage JavaScript interpreter library.

## Getting started

slothjs does not contains a lexical JavaScript parser: it consumes an AST tree supplied in the
[ESTree format](https://github.com/estree/estree/blob/master/es5.md).

When compiled with `feature = ["std"]` (used by default), sljs provides [`runtime::Runtime`]
abstraction that bundles an external JavaScript parser, a way to execute it and the interpreter
itself.

There are two [`runtime::Parser`]s at the moment, both using a JavaScript parser written in
JavaScript called [Esprima](https://esprima.org/):

- [`runtime::NodejsParser`]: it runs Esprima in an external nodejs runtime to parse input into
  ESTree-structured JSON abstract syntax tree (AST), loaded and executed by the interpreter.

- experimental [`runtime::EsprimaParser`]: it runs a JSON dump of Esprima's AST bundled within
  the interpreter in the interpreter itself. It takes input and produces a AST directly on
  the heap of the interpreter. It is obviously slow and unstable at the moment: many methods
  of the builtin JavaScript objects like String/Object/Number/etc are not implemented yet.

```
use slothjs::JSValue;
use slothjs::runtime::{Runtime, EsprimaParser};

let parser = Box::new(EsprimaParser::new());
let mut runtime = Runtime::load(parser).expect("Runtime::load");

runtime.evaluate("var x = 2, y = 2").expect("eval: var x, y");
runtime.evaluate("function add(a, b) { return a + b }").expect("eval: add");
let result: JSValue = runtime.evaluate("add(x, y)").expect("eval: x + y");

assert_eq!(result, JSValue::from(4));
```

## A more detailed usage example

If you don't use the default features (i.e. `features = ["std"]`), [`runtime::Runtime`] is not
available. In that case you have to do the grunt work yourself:

- supply an ESTree AST
- parse it into a [`Program`]
- execute the program on a [`Heap`].

For example:
```
use slothjs::{JSON, Heap};      // an alias for `serde_json::Value`

// JavaScript: "2 + 2"
let estree: JSON = serde_json::from_str(r#"{
  "type": "Program",
  "body": [
    {
      "type": "ExpressionStatement",
      "expression": {
        "type": "BinaryExpression",
        "operator": "+",
        "left": { "type": "Literal", "value": 2 },
        "right": { "type": "Literal", "value": 2 }
      }
    }
  ]
}"#).expect("JSON");
```

[`Program::parse_from`] can parse any representation of ESTree that implements the
[`SourceNode`] trait; a reference implementation is provided for `slothjs::JSON` (an alias for
`serde_json::Value`):
```
# use slothjs::*;
# let source = r#"
# { "type": "Program", "body": [
#   { "type": "ExpressionStatement", "expression": {
#       "type": "BinaryExpression", "operator": "+",
#       "left": { "type": "Literal", "value": 2 },
#       "right": { "type": "Literal", "value": 2 } } } ] }
# "#;
# let estree: JSON = serde_json::from_str(source).expect("JSON");
let program = Program::parse_from(&estree).expect("ESTree");
```

To run a [`Program`], create a [`Heap`] (it's roughly a vector of
[`JSObject`]s with its indexes encapsulated in [`JSRef`]s) and interpret
the `program` on this `heap`:
```
# use slothjs::*;
# let source = r#"
# { "type": "Program", "body": [
#   { "type": "ExpressionStatement", "expression": {
#       "type": "BinaryExpression", "operator": "+",
#       "left": { "type": "Literal", "value": 2 },
#       "right": { "type": "Literal", "value": 2 } } } ] }
# "#;
# let estree: JSON = serde_json::from_str(source).expect("JSON error");
# let program = Program::parse_from(&estree).expect("ESTree error");
// builtin objects like Object, String, etc, are initialized here:
let mut heap = Heap::new();

let result: JSValue = match heap.evaluate(&program) {
    Ok(result) => result,
    Err(exc) => {
        eprintln!("Exception: {:?}", exc);
        return;
    }
};
assert_eq!(result, JSValue::from(4));
```

A value of type [`Interpreted`] is usually a wrapper for a [`JSValue`],
although it might represent an object member that does not exist yet (which evaluates to
`JSValue::Undefined` when reading):
```
# use slothjs::*;
# use slothjs::interpret::Interpretable;
# let source = r#"
# { "type": "Program", "body": [
#   { "type": "ExpressionStatement", "expression": {
#       "type": "BinaryExpression", "operator": "+",
#       "left": { "type": "Literal", "value": 2 },
#       "right": { "type": "Literal", "value": 2 } } } ] }
# "#;
# let estree: JSON = serde_json::from_str(source).expect("JSON error");
# let program = Program::parse_from(&estree).expect("ESTree error");
# let mut heap = Heap::new();
let result: Interpreted = program.interpret(&mut heap).expect("interpret()");
let value: JSValue = result.to_value(&heap).expect("JSValue");
assert_eq!(value, JSValue::from(4));

let output = value.to_string(&mut heap).unwrap();
assert_eq!(&output, "4");
```

## More about objects

[`JSObject`] contains:
- a [`JSRef`] to its prototype object
- optionally an [`object::ObjectValue`] encapsulatng a primitive value,
  or a [`function::VMCall`]/[`function::Closure`], or an optimized object storage
  (e.g. [`object::JSArray`])
- a hashmap from `String` keys to [`object::Property`]

Each [`object::Property`] consists of:
- [`object::Access`] that determines if property is enumerable, writable, configurable;
- [`object::Content`] that encapsulates a way to get and/or set a [`JSValue`].

TODO: more about JSObject API

## Functions and closures

An example of calling a JavaScript closure from Rust using [`Heap::execute`]:

```
use slothjs::*;

// function zoom(size) { return 1.1 * size; }
let estree: JSON = serde_json::from_str(r#"{
  "type": "Program",
  "body": [{
      "type": "FunctionDeclaration",
      "id": { "type": "Identifier", "name": "zoom" },
      "params": [{"type": "Identifier", "name": "size"}],
      "body": {
        "type": "BlockStatement",
        "body": [{
          "type": "ReturnStatement",
          "argument": {
            "type": "BinaryExpression",
            "operator": "*",
            "left": { "type": "Literal", "value": 1.1 },
            "right": { "type": "Identifier", "name": "size" }
          }
        }]
      }
  }]
}"#).expect("JSON");
let zoom_decl = Program::parse_from(&estree).expect("ESTree");

let mut heap = Heap::new();

// Create a function object for `zoom`:
zoom_decl.interpret(&mut heap).unwrap();

// Get a reference to the `zoom` function object:
let zoomref: JSRef = heap
    .lookup_path(&["zoom"])     // or `&["global", "zoom"]` for `global.zoom`
    .expect("zoom function")    // : Interpreted
    .to_ref(&heap).expect("reference");

// Finally, call `zoom(10)`:
let call = CallContext::from(vec![Interpreted::from(10)])
    .with_this(Heap::GLOBAL)
    .with_name("zoom");
let result: Interpreted = heap.execute(zoomref, call).expect("call result");

let result: JSValue = result.to_value(&heap).unwrap();
assert_eq!(result, JSValue::from(11.0));
```

## Errors and exceptions

Most functions return a [`JSResult<T>`], an alias for `Result<T, Exception>`. An [`Exception`]
can wrap:

- [`Exception::UserThrown`] produced by a `throw` in JavaScript
- [`Exception::SyntaxTreeError`] wrapping a [`error::ParseError`]
- builtin errors, e.g. [`Exception::ReferenceNotFound`], etc.

[`runtime::EvalError`] is a wrapper over [`Exception`], serde (de)serialization errors, I/O
errors when executing an external parser.

## What it can do?
See [src/test.rs](../src/slothjs/test.rs.html), all uncommented tests in there should work.

## What is not done yet?
- garbage collection;
- any kind of meaningful performance optimization;
