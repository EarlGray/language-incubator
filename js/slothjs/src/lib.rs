#![cfg_attr(not(feature = "std"), no_std)]
#![feature(let_else)]
#![doc = include_str!("../docs/DOC.md")]

extern crate alloc;

pub mod ast;
mod builtin;
pub mod error;
pub mod function;
pub mod heap;
pub mod interpret;
mod parse;
mod prelude;
pub mod source;

#[cfg(feature = "std")]
pub mod runtime;

#[cfg(test)]
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
pub use parse::{
    estree::ToESTree,
    HeapNode,
    SourceNode,
};

pub type JSON = serde_json::Value;
