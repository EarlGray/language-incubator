#![cfg_attr(not(feature = "std"), no_std)]
#![feature(let_else)]
#![feature(maybe_uninit_uninit_array)]
#![feature(maybe_uninit_array_assume_init)]
#![doc = include_str!("../docs/DOC.md")]

#![allow(unused_imports)]   // TODO
#![allow(unused_variables)] // TODO

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
    Realm,
    Heap,
    JSRef,
    Ref,
};
pub use heap::value;
pub use interpret::Interpretable;
pub use parse::{
    estree::ToESTree,
    SourceNode,
    //HeapNode,
};

pub type JSON = serde_json::Value;

// STUBS
pub type JSValue = ();       // TODO
pub type Interpreted = ();   // TODO
