use crate::ast::Identifier;
use crate::object::{
    Interpreted,
    JSValue,
    JSON,
};
use crate::prelude::*;

#[derive(Debug, PartialEq)]
pub enum ParseError<V> {
    InvalidJSON { err: String },
    ShouldBeBool { value: V },
    ShouldBeString { value: V },
    ShouldBeArray { value: V },
    //ShouldBeObject{ value: V },
    ObjectWithout { attr: String, value: V },
    UnexpectedValue { want: &'static str, value: V },
    UnknownType { value: V },
}

impl<V> ParseError<V> {
    pub fn no_attr(attr: &str, at: V) -> ParseError<V> {
        ParseError::ObjectWithout {
            attr: attr.to_string(),
            value: at,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Exception {
    SyntaxTreeError(ParseError<JSON>),
    SyntaxErrorForInMultipleVar(),
    SyntaxErrorContinueLabelNotALoop(Identifier),

    ReferenceNotAnObject(Interpreted),
    ReferenceNotFound(Identifier),
    TypeErrorSetReadonly(Interpreted, String),
    TypeErrorNotConfigurable(Interpreted, String),
    TypeErrorGetProperty(Interpreted, String),
    TypeErrorCannotAssign(Interpreted),
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

impl Exception {
    pub fn instance_required(arg: &Interpreted, of: &str) -> Exception {
        Exception::TypeErrorInstanceRequired(arg.clone(), of.to_string())
    }

    pub fn invalid_ast<E: fmt::Debug>(e: E) -> Self {
        let err = format!("{:?}", e);
        Exception::SyntaxTreeError(ParseError::InvalidJSON { err })
    }
}

pub fn ignore_set_readonly(e: Exception) -> Result<(), Exception> {
    match e {
        Exception::TypeErrorSetReadonly(_, _) => Ok(()),
        _ => Err(e),
    }
}
