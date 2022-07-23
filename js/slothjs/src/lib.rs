#![cfg_attr(not(feature = "std"), no_std)]

#![doc = include_str!("../docs/DOC.md")]

extern crate alloc;

pub mod ast;
mod builtin;
pub mod error;
pub mod function;
pub mod heap;
pub mod interpret;
pub mod object;
mod parse;
mod prelude;
pub mod source;

#[cfg(feature = "std")]
pub mod runtime;

#[cfg(test)]
#[rustfmt::skip]
mod test;

pub use ast::Program;
pub use error::{
    Exception,
    JSResult,
};
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
    estree::ToESTree,
};
