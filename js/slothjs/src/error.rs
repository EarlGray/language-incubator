//use std::fmt;

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
    TypeErrorSetProperty(Interpreted, String),
    TypeErrorGetProperty(Interpreted, String),
    TypeErrorCannotAssign(Interpreted),
    TypeErrorNotCallable(Interpreted),

    JumpReturn(Interpreted),
}

impl Exception {
    pub fn kind_eq(&self, other: &Exception) -> bool {
        // meh, could the compiler write this for me?
        use Exception::*;
        match (self, other) {
            (SyntaxError(_), SyntaxError(_))
                | (ReferenceNotAnObject(_), ReferenceNotAnObject(_))
                | (ReferenceNotFound(_), ReferenceNotFound(_))
                | (TypeErrorSetProperty(_, _), TypeErrorSetProperty(_, _))
                | (TypeErrorGetProperty(_, _), TypeErrorGetProperty(_, _))
                | (TypeErrorCannotAssign(_), TypeErrorCannotAssign(_))
                | (TypeErrorNotCallable(_), TypeErrorNotCallable(_))
                => true,
            _ => false,
        }
    }
}

/*
impl Display for Exception {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Exception::*;
        match self {
        }
    }
}
*/
