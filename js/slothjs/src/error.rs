use crate::object::{Interpreted, JSON};
use crate::ast::Identifier;

#[allow(dead_code)]
#[derive(Debug, PartialEq)]
pub enum ParseError<V> {
    InvalidJSON{ err: String },
    ShouldBeBool{ value: V },
    ShouldBeString{ value: V },
    ShouldBeArray{ value: V },
    //ShouldBeObject{ value: V },
    ObjectWithout{ attr: &'static str, value: V},
    UnexpectedValue{ want: &'static str, value: V},
    UnknownType{ value: V },
}

#[allow(dead_code)]
#[derive(Debug, PartialEq)]
pub enum Exception {
    SyntaxError(ParseError<JSON>),
    SyntaxErrorContinueLabelNotALoop(Identifier),
    ReferenceNotAnObject(Interpreted),
    ReferenceNotFound(String),
    TypeErrorSetReadonly(Interpreted, String),
    TypeErrorNotConfigurable(Interpreted, String),
    TypeErrorGetProperty(Interpreted, String),
    TypeErrorCannotAssign(Interpreted),
    TypeErrorNotCallable(Interpreted),
    TypeErrorNotArraylike(Interpreted),
    TypeErrorInstanceRequired(Interpreted, String),
    TypeErrorInvalidDescriptor(Interpreted),

    JumpReturn(Interpreted),
    JumpBreak(Option<Identifier>),
    JumpContinue(Option<Identifier>),
}

pub fn ignore_set_readonly(e: Exception) -> Result<(), Exception> {
    match e {
        Exception::TypeErrorSetReadonly(_, _) => Ok(()),
        _ => Err(e),
    }
}
