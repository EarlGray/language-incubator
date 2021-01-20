use crate::object::{Interpreted, JSON};

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
    ReferenceNotAnObject(Interpreted),
    ReferenceNotFound(String),
    TypeErrorSetReadonly(Interpreted, String),
    TypeErrorNotConfigurable(Interpreted, String),
    TypeErrorGetProperty(Interpreted, String),
    TypeErrorCannotAssign(Interpreted),
    TypeErrorNotCallable(Interpreted),
    TypeErrorNotArraylike(Interpreted),

    JumpReturn(Interpreted),
}
