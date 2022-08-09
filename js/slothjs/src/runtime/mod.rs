mod esprima;
mod nodejs;

use core::str::Utf8Error;
use std::io;

use crate::function::HostFn;
use crate::{
    error,
    Exception,
    Heap,
    JSValue,
    Program,
    JSON,
};
use crate::{
    prelude::*,
    CallContext,
    Interpreted,
    JSResult,
};

pub use self::esprima::EsprimaParser;
pub use self::nodejs::NodejsParser;

#[derive(Debug)]
pub enum EvalError {
    /// Javascript exception
    Exception(Exception),

    /// Serde error
    Serialization(serde_json::Error),

    /// I/O error
    Io(io::Error),
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            EvalError::Exception(exc) => match exc {
                Exception::SyntaxTreeError(error::ParseError::InvalidJSON { err }) => {
                    writeln!(f, "Syntax error:{}", err.as_str())
                }
                _ => write!(f, "Error: {:?}", exc),
            },
            EvalError::Serialization(e) => writeln!(f, "Serialization error:\n{}", e),
            EvalError::Io(e) => writeln!(f, "{}", e),
        }
    }
}

impl From<Exception> for EvalError {
    fn from(exc: Exception) -> Self {
        EvalError::Exception(exc)
    }
}

impl From<serde_json::Error> for EvalError {
    fn from(err: serde_json::Error) -> Self {
        EvalError::Serialization(err)
    }
}

impl From<Utf8Error> for EvalError {
    fn from(err: Utf8Error) -> Self {
        let err = io::Error::new(io::ErrorKind::InvalidData, err);
        EvalError::Io(err)
    }
}

impl From<io::Error> for EvalError {
    fn from(err: io::Error) -> Self {
        EvalError::Io(err)
    }
}

impl From<std::string::FromUtf8Error> for EvalError {
    fn from(e: std::string::FromUtf8Error) -> EvalError {
        EvalError::Io(io::Error::new(io::ErrorKind::InvalidData, e))
    }
}

impl From<EvalError> for io::Error {
    fn from(err: EvalError) -> io::Error {
        match err {
            EvalError::Io(err) => err,
            _ => io::Error::new(io::ErrorKind::Other, err.to_string()),
        }
    }
}

impl From<EvalError> for Exception {
    fn from(err: EvalError) -> Exception {
        match err {
            EvalError::Exception(exc) => exc,
            EvalError::Serialization(e) => Exception::UserThrown(JSValue::from(e.to_string())),
            EvalError::Io(e) => Exception::UserThrown(JSValue::from(e.to_string())),
        }
    }
}

pub type EvalResult<T> = Result<T, EvalError>;

/// Describes anything that can parse JS code.
pub trait Parser: fmt::Debug {
    /// Creates a Parser
    fn load(&mut self, heap: &mut Heap) -> EvalResult<()>;

    /// Parses an input into a `Program` (potentially using the `heap`)
    fn parse(&self, input: &str, heap: &mut Heap) -> EvalResult<Program>;

    /// Get the native callback for `eval()` in JavaScript provided by this parser
    fn eval_func(&self) -> HostFn;
}

/// The sljs JavaScript runtime.
///
/// It should be parameterized by a [`Parser`] implementation, e.g. [`NodejsParser`] or
/// [`EsprimaParser`]:
///
/// ```
/// use slothjs::JSON;
/// use slothjs::runtime::{Runtime, NodejsParser};
///
/// let parser = Box::new(NodejsParser::new());
/// let mut sljs = Runtime::load(parser)
///     .expect("Runtime::load");
/// ```
///
/// The [`Runtime`] evaluates JavaScript source code and creates/stores [`crate::JSObject`]s.
///
/// ```
/// # use slothjs::JSON;
/// # use slothjs::runtime::{Runtime, NodejsParser};
/// # let parser = Box::new(NodejsParser::new());
/// # let mut sljs = Runtime::load(parser).expect("Runtime::load");
///
/// sljs.evaluate("var x = 12")
///     .expect("eval: var x");
/// let x = sljs.evaluate("x")
///     .expect("eval: x");
///
/// assert_eq!(sljs.json_from(x), JSON::from(12.0));
/// ```
///
pub struct Runtime {
    pub heap: Heap,
    parser: Box<dyn Parser>,
}

impl Runtime {
    /// Creates a sljs runtime.
    pub fn load(mut parser: Box<dyn Parser>) -> EvalResult<Self> {
        let mut heap = Heap::new();
        parser.load(&mut heap)?;

        let eval_ref = heap.alloc_func(parser.eval_func());
        heap.get_mut(Heap::GLOBAL).set_hidden("eval", eval_ref)?;

        Ok(Runtime { heap, parser })
    }

    /// Exposes the configured parser.
    pub fn parse(&mut self, input: &str) -> EvalResult<Program> {
        self.parser.parse(input, &mut self.heap)
    }

    /// Takes an `input` and evaluates it.
    pub fn evaluate(&mut self, input: &str) -> EvalResult<JSValue> {
        let program = self.parse(input)?;
        self.heap.evaluate(&program).map_err(EvalError::Exception)
    }

    /// Turn a [`JSValue`] into [`JSON`]
    pub fn json_from(&mut self, value: JSValue) -> JSON {
        value.to_json(&self.heap).expect("JSValue.to_json()")
    }

    /// Turn a [`JSValue`] into a human-readable string.
    pub fn string_from(&mut self, value: JSValue) -> String {
        value
            .to_string(&mut self.heap)
            .expect("JSValue.to_string()")
    }

    pub fn dbg(&mut self, refstr: &str) {
        if let Ok(refnum) = usize::from_str(refstr) {
            match self.heap.get_index(refnum) {
                Some(object) => {
                    dbg!(object);
                }
                None => {
                    eprintln!("No object at #{}", refnum);
                }
            };
        } else {
            // TODO: evaluate an expression to a reference
            eprintln!("Command error: expected a number");
        }
    }
}

/// Not-really-a-Parser implementation that just deserializes a JSON ESTree.
#[derive(Debug)]
pub struct JSONParser;

impl JSONParser {
    fn json_eval(_: CallContext, _: &mut Heap) -> JSResult<Interpreted> {
        unimplemented!("JSONParser does not support eval()")
    }
}

impl Parser for JSONParser {
    fn load(&mut self, _heap: &mut Heap) -> EvalResult<()> {
        Ok(())
    }

    fn parse(&self, input: &str, _heap: &mut Heap) -> EvalResult<Program> {
        // It's not much, but it's honest work.
        let estree: JSON = serde_json::from_str(input)?;
        let program = Program::parse_from(&estree).map_err(Exception::from)?;
        Ok(program)
    }

    fn eval_func(&self) -> HostFn {
        Self::json_eval
    }
}
