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

#[derive(Debug, PartialEq)]
pub enum Exception {
    /// nonlocal transfers of control, "abrupt completions"
    Jump(Jump),

    /// exceptions thrown by the user and propagated up the call stack
    UserThrown(JSValue),

    /// SyntaxError
    Syntax(ParseError),

    /// ReferenceError
    Reference(ReferenceError),

    /// TypeError
    Type(TypeError),
}

// TODO: impl Display for Exception
// TODO: #[cfg(feature = "std"] impl Error for Exception
// TODO: capture JavaScript stack trace in Exception

impl Exception {
    pub fn instance_required<V>(arg: V, of: &str) -> Exception
    where
        Interpreted: From<V>,
    {
        Exception::attr_type_error(TypeError::INSTANCE_REQUIRED, arg, of)
    }

    pub(crate) fn no_loop_for_continue_label(label: Identifier) -> Self {
        Self::Syntax(ParseError::ContinueLabelNotALoop(label))
    }

    pub(crate) fn no_reference<Id>(id: Id) -> Self
    where Identifier: From<Id>
    {
        Self::Reference(ReferenceError::not_found(id.into()))
    }

    pub(crate) fn not_an_object<V>(value: V) -> Self
    where Interpreted: From<V>
    {
        let referr = ReferenceError {
            tag: ReferenceError::NOT_OBJECT,
            to: Identifier::from(""),
            value: Interpreted::from(value),
        };
        Self::Reference(referr)
    }

    pub(crate) fn attr_type_error<V, S>(tag: &'static str, what: V, attr: S) -> Exception
    where Interpreted: From<V>, JSString: From<S> {
        Self::Type(TypeError{
            tag,
            value: Interpreted::from(what),
            attr: JSString::from(attr),
        })
    }

    pub(crate) fn type_error<V>(tag: &'static str, what: V) -> Exception
    where Interpreted: From<V> {
        Self::Type(TypeError{
            tag,
            value: Interpreted::from(what),
            attr: JSString::from(""),
        })
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
        Exception::Type(TypeError{ tag: TypeError::SET_READONLY, ..}) => Ok(()),
        _ => Err(e),
    }
}
/// nonlocal transfers of control, "abrupt completions"
#[derive(Debug, PartialEq)]
pub enum Jump {
    Return(Interpreted),
    Break(Option<Identifier>),
    Continue(Option<Identifier>),
}

#[derive(Debug, PartialEq)]
pub struct TypeError {
    tag: &'static str,
    value: Interpreted,
    attr: JSString,
}

impl TypeError {
    pub const SET_READONLY: &str = "cannot set a readonly property";
    pub const NONCONFIGURABLE_PROPERTY: &str = "the property is not configuratble";
    pub const CANNOT_GET_PROPERTY: &str = "property is not gettable";
    pub const CANNOT_SET_PROPERTY: &str = "property is not settable";
    pub const CONST_ASSIGN: &str = "cannot assign to const";
    pub const NOT_CALLABLE: &str = "not callable";
    pub const NOT_ARRAYLIKE: &str = "not array-like";
    pub const INSTANCE_REQUIRED: &str = "an instance required";
    pub const INVALID_DESCRIPTOR: &str = "invalid descriptor";
    pub const INVALID_PROTO: &str = "invalid prototype";
}

#[derive(Debug, PartialEq)]
pub struct ReferenceError {
    tag: &'static str,
    to: Identifier,
    value: Interpreted,
}

impl ReferenceError {
    pub const NOT_FOUND: &str = "Reference not found";
    pub const NOT_OBJECT: &str = "Reference is not an object";

    pub fn not_found<Id>(id: Id) -> Self where Identifier: From<Id> {
        Self {
            tag: Self::NOT_FOUND,
            to: Identifier::from(id),
            value: Interpreted::VOID,
        }
    }
}


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
