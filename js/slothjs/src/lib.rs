pub mod ast;
mod builtin;
pub mod error;
pub mod function;
pub mod heap;
pub mod interpret;
pub mod object;
mod parse;
pub mod source;
#[cfg(test)]
#[rustfmt::skip]
mod test;

pub type Program = ast::Program;
pub type HeapNode<'a> = parse::HeapNode<'a>;

pub type Interpreted = object::Interpreted;
pub type JSON = object::JSON;
pub type JSObject = object::JSObject;
pub type JSValue = object::JSValue;

pub type Heap = heap::Heap;
pub type JSRef = heap::JSRef;

pub type CallContext = function::CallContext;

pub type Exception = error::Exception;
