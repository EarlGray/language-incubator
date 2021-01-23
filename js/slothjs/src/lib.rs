pub mod ast;
pub mod object;
pub mod heap;
pub mod error;
pub mod interpret;
mod parse;
mod builtin;
#[cfg(test)]
mod test;


pub type Program = ast::Program;

pub type JSON = object::JSON;
pub type JSObject = object::JSObject;
pub type JSValue = object::JSValue;

pub type Heap = heap::Heap;
pub type JSRef = heap::JSRef;

pub type Exception = error::Exception;
