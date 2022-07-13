#[cfg(feature = "std")]
use std::io;

use crate::ast::Identifier;
use crate::object::{
    Interpreted,
    JSValue,
    JSON,
};
use crate::prelude::*;

pub type JSResult<T> = Result<T, Exception>;

#[derive(Debug, PartialEq, Eq)]
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
pub enum Exception {
    SyntaxTreeError(ParseError),
    SyntaxErrorForInMultipleVar(),
    SyntaxErrorContinueLabelNotALoop(Identifier),

    ReferenceNotAnObject(Interpreted),
    ReferenceNotFound(Identifier),
    TypeErrorSetReadonly(Interpreted, String),
    TypeErrorNotConfigurable(Interpreted, String),
    TypeErrorGetProperty(Interpreted, String),
    TypeErrorCannotAssign(Interpreted),
    TypeErrorConstAssign(Interpreted),
    TypeErrorNotCallable(Interpreted),
    TypeErrorNotArraylike(Interpreted),
    TypeErrorInstanceRequired(Interpreted, String),
    TypeErrorInvalidDescriptor(Interpreted),
    TypeErrorInvalidPrototype(Interpreted),

    // nonlocal transfers of control, "abrupt completions"
    JumpReturn(Interpreted),
    JumpBreak(Option<Identifier>),
    JumpContinue(Option<Identifier>),

    UserThrown(JSValue),
}

// TODO: impl Display for Exception
// TODO: #[cfg(feature = "std"] impl Error for Exception
// TODO: capture JavaScript stack trace in Exception

impl Exception {
    pub fn instance_required<V>(arg: V, of: &str) -> Exception
    where
        Interpreted: From<V>,
    {
        let arg = Interpreted::from(arg);
        Exception::TypeErrorInstanceRequired(arg, of.to_string())
    }
}

impl From<ParseError> for Exception {
    fn from(err: ParseError) -> Self {
        Self::SyntaxTreeError(err)
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

pub fn ignore_set_readonly(e: Exception) -> JSResult<()> {
    match e {
        Exception::TypeErrorSetReadonly(_, _) => Ok(()),
        _ => Err(e),
    }
}
