//! slothjs - a naïve, primitive, savage Javascript interpreter library.
//!
//! ## Usage and main data types
//! slothjs does not contains a lexical Javascript parser: an AST tree must be supplied in
//! [ESTree-compatible format](https://github.com/estree/estree/blob/master/es5.md). For example:
//! ```
//! use slothjs::*;
//!
//! // Javascript: "2 + 2"
//! let source = r#"{
//!   "type": "Program",
//!   "body": [
//!     {
//!       "type": "ExpressionStatement",
//!       "expression": {
//!         "type": "BinaryExpression",
//!         "operator": "+",
//!         "left": { "type": "Literal", "value": 2 },
//!         "right": { "type": "Literal", "value": 2 }
//!       }
//!     }
//!   ]
//! }"#;
//! let estree: JSON = serde_json::from_str(source).expect("JSON error");
//! ```
//!
//! Access to ESTree nodes is provided by implementing the [`SourceNode`] trait;
//! a reference implementation is provided for `slothjs::JSON` (an alias for `serde_json::Value`):
//! ```
//! # use slothjs::*;
//! # let source = r#"
//! # { "type": "Program", "body": [
//! #   { "type": "ExpressionStatement", "expression": {
//! #       "type": "BinaryExpression", "operator": "+",
//! #       "left": { "type": "Literal", "value": 2 },
//! #       "right": { "type": "Literal", "value": 2 } } } ] }
//! # "#;
//! # let estree: JSON = serde_json::from_str(source).expect("JSON error");
//! let program = Program::parse_from(&estree).expect("ESTree error");
//! ```
//!
//! To run a [`Program`], create a [`Heap`] (it's roughly a vector of
//! [`JSObject`]s with its indexes encapsulated in [`JSRef`]s) and interpret
//! the `program` on this `heap`:
//! ```
//! # use slothjs::*;
//! # let source = r#"
//! # { "type": "Program", "body": [
//! #   { "type": "ExpressionStatement", "expression": {
//! #       "type": "BinaryExpression", "operator": "+",
//! #       "left": { "type": "Literal", "value": 2 },
//! #       "right": { "type": "Literal", "value": 2 } } } ] }
//! # "#;
//! # let estree: JSON = serde_json::from_str(source).expect("JSON error");
//! # let program = Program::parse_from(&estree).expect("ESTree error");
//! // builtin objects like Object, String, etc, are initialized here:
//! let mut heap = Heap::new();
//!
//! let result: Interpreted = match program.interpret(&mut heap) {
//!     Ok(result) => result,
//!     Err(exc) => {
//!         eprintln!("Exception: {:?}", exc);
//!         return;
//!     }
//! };
//! ```
//!
//! A value of type [`Interpreted`] is usually a wrapper for a [`JSValue`],
//! although it might represent an object member that does not exist yet (which evaluates to
//! `JSValue::Undefined` when reading):
//! ```
//! # use slothjs::*;
//! # use slothjs::interpret::Interpretable;
//! # let source = r#"
//! # { "type": "Program", "body": [
//! #   { "type": "ExpressionStatement", "expression": {
//! #       "type": "BinaryExpression", "operator": "+",
//! #       "left": { "type": "Literal", "value": 2 },
//! #       "right": { "type": "Literal", "value": 2 } } } ] }
//! # "#;
//! # let estree: JSON = serde_json::from_str(source).expect("JSON error");
//! # let program = Program::parse_from(&estree).expect("ESTree error");
//! # let mut heap = Heap::new();
//! # let result = program.interpret(&mut heap).expect("interpret()");
//! let value = result.to_value(&heap).expect("JSValue");
//! assert_eq!(value, JSValue::Number(4.0));
//!
//! let output = value.to_string(&mut heap).unwrap();
//! println!("Evaluation result: {}", &output);
//!
//! ```
//!
//! ## More about objects
//!
//! [`JSObject`] contains:
//! - a [`JSRef`] to its prototype object
//! - optionally an [`object::ObjectValue`] encapsulatng a primitive value,
//!   or a [`function::VMCall`]/[`function::Closure`], or an optimized object storage
//!   (e.g. [`object::JSArray`])
//! - a hashmap from `String` keys to [`object::Property`]
//!
//! Each [`object::Property`] consists of:
//! - [`object::Access`] that determines if property is enumerable, writable, configurable;
//! - [`object::Content`] that encapsulates a way to get and/or set a [`JSValue`].
//!
//!
//! ## Functions and closures
//!
//! An example of calling a Javascript closure from Rust using [`Heap::execute`]:
//!
//! ```
//! use slothjs::*;
//!
//! // function zoom(size) { return 1.1 * size; }
//! let estree: JSON = serde_json::from_str(r#"{
//!   "type": "Program",
//!   "body": [{
//!       "type": "FunctionDeclaration",
//!       "id": { "type": "Identifier", "name": "zoom" },
//!       "params": [{"type": "Identifier", "name": "size"}],
//!       "body": {
//!         "type": "BlockStatement",
//!         "body": [{
//!           "type": "ReturnStatement",
//!           "argument": {
//!             "type": "BinaryExpression",
//!             "operator": "*",
//!             "left": { "type": "Literal", "value": 1.1 },
//!             "right": { "type": "Identifier", "name": "size" }
//!           }
//!         }]
//!       }
//!   }]
//! }"#).expect("JSON");
//! let zoom_decl = Program::parse_from(&estree).expect("ESTree");
//!
//! let mut heap = Heap::new();
//!
//! // Create a function object for `zoom`:
//! zoom_decl.interpret(&mut heap).unwrap();
//!
//! // Get a reference to the `zoom` function object:
//! let zoomref: JSRef = heap
//!     .lookup_path(&["zoom"])     // or `&["global", "zoom"]` for `global.zoom`
//!     .expect("zoom function")    // : Interpreted
//!     .to_ref(&heap).expect("reference");
//!
//! // Finally, call `zoom(10)`:
//! let result: Interpreted = heap.execute(zoomref, function::CallContext{
//!     this_ref: Heap::GLOBAL,
//!     method_name: String::from("zoom"),  // TODO: get zoom.name
//!     arguments: vec![Interpreted::from(10.0)],
//!     loc: None,
//! }).expect("call result");
//!
//! let result: JSValue = result.to_value(&heap).unwrap();
//! assert_eq!(result, JSValue::Number(11.0));
//! ```
//!
//! ## What it can do?
//! See [src/test.rs](../src/slothjs/test.rs.html), all uncommented tests in there should work.
//!
//! ## What is not done yet?
//! - garbage collection;
//! - any kind of meaningful performance optimization;

pub mod ast;
mod builtin;
pub mod error;
pub mod function;
pub mod heap;
pub mod interpret;
pub mod object;
mod parse;
pub mod runtime;
pub mod source;
#[cfg(test)]
#[rustfmt::skip]
mod test;

pub use ast::Program;
pub use error::Exception;
pub use function::CallContext;
pub use heap::{
    Heap,
    JSRef,
};
pub use interpret::Interpretable;
pub use object::{
    Interpreted,
    JSObject,
    JSValue,
    JSON,
};
pub use parse::{
    HeapNode,
    SourceNode,
};
