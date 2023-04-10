#[cfg(feature = "std")]
use std::io;

use crate::prelude::*;
use crate::{
    ast::Identifier,
    Interpreted,
    JSValue,
    JSON,
};

pub type JSResult<T> = Result<T, Exception>;

/// nonlocal transfers of control, "abrupt completions"
#[derive(Debug, PartialEq)]
pub enum Jump {
    Return(Interpreted),
    Break(Option<Identifier>),
    Continue(Option<Identifier>),
}

/*
// TODO: switch Exception to this
#[derive(Debug, PartialEq)]
pub enum JSCompletion {
    Jump(Jump),
    Error(JSError),
}

#[derive(Debug, PartialEq)]
pub enum JSError {
    UserThrown(JSValue),
    Type(TypeError),
    Reference(ReferenceError),
    Syntax(SyntaxError),
    Deprecated(Exception),
}

impl From<Exception> for JSError {
    fn from(value: Exception) -> Self {
        Self::Deprecated(value)
    }
}

#[derive(Debug, PartialEq)]
pub struct TypeError {
    tag: &'static str,
    value: Interpreted,
    attr: JSString,
}

impl TypeError {
    pub const SET_READONLY: &str = "";
    pub const NONCONFIGURABLE_PROPERTY: &str = "";
    pub const NO_PROPERTY: &str = "";
    /*
    SetReadonly(Interpreted, JSString),
    NotConfigurable(Interpreted, JSString),
    GetProperty(Interpreted, JSString),
    CannotAssign(Interpreted),
    ConstAssign(Interpreted),
    NotCallable(Interpreted),
    NotArraylike(Interpreted),
    InstanceRequired(Interpreted, JSString),
    InvalidDescriptor(Interpreted),
    InvalidPrototype(Interpreted),
    */
}

#[derive(Debug, PartialEq)]
pub enum ReferenceError {
}

#[derive(Debug, PartialEq)]
pub enum SyntaxError {
}
*/

#[derive(Debug, PartialEq, Eq)]
pub enum ParseError {
    InvalidJSON { err: String },
    ObjectWithout { attr: String, value: JSON },
    UnexpectedValue { want: &'static str, value: JSON },
    UnknownNodeType { value: JSON },
    BindingRedeclared {},
    ForInMultipleVar(),
    ContinueLabelNotALoop(Identifier),
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

impl From<&str> for ParseError {
    fn from(err: &str) -> Self {
        // This assumes nodejs-formatted Esprima error output.
        let mut err = unescape(err);
        if err.ends_with('}') {
            if let Some(at) = err.rfind('{') {
                // let jerr = err.split_off(at); // TODO: parse the error JSON
                err.truncate(at)
            }
        }
        Self::InvalidJSON { err }
    }
}

#[derive(Debug, PartialEq)]
pub enum Exception {
    Syntax(ParseError),

    ReferenceNotAnObject(Interpreted),
    ReferenceNotFound(Identifier),
    TypeErrorSetReadonly(Interpreted, JSString),
    TypeErrorNotConfigurable(Interpreted, JSString),
    TypeErrorGetProperty(Interpreted, JSString),
    TypeErrorCannotAssign(Interpreted),
    TypeErrorConstAssign(Interpreted),
    TypeErrorNotCallable(Interpreted),
    TypeErrorNotArraylike(Interpreted),
    TypeErrorInstanceRequired(Interpreted, JSString),
    TypeErrorInvalidDescriptor(Interpreted),
    TypeErrorInvalidPrototype(Interpreted),

    // nonlocal transfers of control, "abrupt completions"
    Jump(Jump),

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
        Exception::TypeErrorInstanceRequired(arg, of.into())
    }

    pub(crate) fn no_loop_for_continue_label(label: Identifier) -> Self {
        Self::Syntax(ParseError::ContinueLabelNotALoop(label))
    }
}

impl From<ParseError> for Exception {
    fn from(err: ParseError) -> Self {
        Self::Syntax(err)
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

pub fn unescape(input: &str) -> String {
    struct Unescape<C>(C);

    impl<C> Iterator for Unescape<C>
    where
        C: Iterator<Item = char>,
    {
        type Item = char;

        fn next(&mut self) -> Option<Self::Item> {
            match self.0.next() {
                Some('\\') => {
                    let c = self.0.next();
                    match c {
                        Some('n') => Some('\n'),
                        Some('t') => Some('\t'),
                        Some(_) => c,
                        None => Some('\\'),
                    }
                }
                other => other,
            }
        }
    }

    let input = input.trim_matches('"').trim();
    String::from_iter(Unescape(input.chars()))
}
