#[cfg(feature = "std")]
use std::io;

use crate::prelude::*;

use crate::{
    JSON,
    ast::Identifier
};


pub type JSResult<T> = Result<T, Exception>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseError {
    InvalidJSON { err: String },
    ObjectWithout { attr: String, value: JSON },
    UnexpectedValue { want: &'static str, value: JSON },
    UnknownNodeType { value: JSON },
    ReferencedBeforeDeclaration {},
    BindingRedeclared {},
}

impl ParseError {
    pub fn no_attr(attr: &str, at: JSON) -> Self {
        ParseError::ObjectWithout {
            attr: attr.to_string(),
            value: at,
        }
    }

    pub fn want(want: &'static str, got: JSON) -> Self {
        Self::UnexpectedValue { want, value: got }
    }

    pub fn invalid_ast<E: fmt::Debug>(e: E) -> Self {
        let err = format!("{:?}", e);
        ParseError::InvalidJSON { err }
    }
}

#[derive(Debug, PartialEq)]
pub struct Exception(pub Exc);

impl Exception {
    pub fn type_error(message: String) -> Self {
        Exception(Exc::Type(TypeError{message}))
    }
}

impl From<ParseError> for Exception {
    fn from(err: ParseError) -> Self {
        Exception(Exc::Syntax(SyntaxError::Parse(err)))
    }
}

#[cfg(feature = "std")]
impl From<Exception> for io::Error {
    fn from(exc: Exception) -> io::Error {
        // TODO: impl Display for Exception
        let msg = format!("{:?}", exc);
        io::Error::new(io::ErrorKind::Other, msg)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Exc {
    Thrown,
    Syntax(SyntaxError),
    Type(TypeError),
}

#[derive(Debug, Clone, PartialEq)]
pub enum SyntaxError {
    Parse(ParseError),
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeError{
    message: String,
}
