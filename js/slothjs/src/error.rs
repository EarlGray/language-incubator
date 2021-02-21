use crate::ast::Identifier;
use crate::object::{
    Interpreted,
    JSValue,
    JSON,
};

/*
use crate::heap::JSRef;

pub type Label = Identifier;

#[derive(Debug, PartialEq)]
pub struct Exception {
    //pub callstack: Vec<(Identifier, Identifier, source::Location)>,
    pub content: Jump,
}


#[derive(Debug, PartialEq)]
pub enum Jump {
    Break(Option<Label>),
    Continue(Option<Label>),
    Return(Interpreted),
    Catch(JSRef),
}
*/

#[derive(Debug, PartialEq)]
pub enum ParseError<V> {
    InvalidJSON { err: String },
    ShouldBeBool { value: V },
    ShouldBeString { value: V },
    ShouldBeArray { value: V },
    //ShouldBeObject{ value: V },
    ObjectWithout { attr: &'static str, value: V },
    UnexpectedValue { want: &'static str, value: V },
    UnknownType { value: V },
}

#[derive(Debug, PartialEq)]
pub enum Exception {
    SyntaxError(ParseError<JSON>),
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

pub fn ignore_set_readonly(e: Exception) -> Result<(), Exception> {
    match e {
        Exception::TypeErrorSetReadonly(_, _) => Ok(()),
        _ => Err(e),
    }
}
