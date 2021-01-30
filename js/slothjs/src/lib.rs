pub mod ast;
mod builtin;
pub mod error;
pub mod heap;
pub mod interpret;
pub mod object;
mod parse;
#[cfg(test)]
#[rustfmt::skip]
mod test;


pub type Program = ast::Program;

pub type JSON = object::JSON;
pub type JSObject = object::JSObject;
pub type JSValue = object::JSValue;

pub type Heap = heap::Heap;
pub type JSRef = heap::JSRef;

pub type Exception = error::Exception;
