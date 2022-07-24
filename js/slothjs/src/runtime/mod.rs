mod esprima;
mod nodejs;

use std::io;
use std::io::prelude::*;

use crate::prelude::*;
use crate::{
    error,
    source,
    CallContext,
    Exception,
    Heap,
    //Interpretable,
    Interpreted,
    JSResult,
    JSValue,
    Program,
    JSON,
};

pub use self::nodejs::NodejsParser;
pub use self::esprima::EsprimaParser;

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
                    writeln!(f, "syntax error:\n{}", err)
                }
                _ => write!(f, "sljs error: {:?}", exc),
            },
            EvalError::Serialization(e) => writeln!(f, "serializaiton error:\n{}", e),
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
pub trait Parser: Sized {
    /// Creates a Parser
    fn load(heap: &mut Heap) -> EvalResult<Self>;

    /// Parses an input into a `Program` (potentially using the `heap`)
    fn parse(&self, input: &str, heap: &mut Heap) -> EvalResult<Program>;

    /// The native callback for `eval()` in JavaScript provided by this parser
    fn eval(call: CallContext, heap: &mut Heap) -> JSResult<Interpreted>;
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
/// let mut sljs = Runtime::<NodejsParser>::load()
///     .expect("Runtime::load");
/// ```
///
/// The [`Runtime`] evaluates JavaScript source code and creates/stores [`crate::JSObject`]s.
///
/// ```
/// # use slothjs::JSON;
/// # use slothjs::runtime::{Runtime, NodejsParser};
/// # let mut sljs = Runtime::<NodejsParser>::load().expect("Runtime::load");
///
/// sljs.evaluate("var x = 12")
///     .expect("eval: var x");
/// let x = sljs.evaluate("x")
///     .expect("eval: x");
///
/// assert_eq!(sljs.json_from(x), JSON::from(12.0));
/// ```
///
pub struct Runtime<P> {
    pub heap: Heap,
    parser: P,
}

impl<P: Parser> Runtime<P> {
    /// Creates a sljs runtime.
    pub fn load() -> EvalResult<Self> {
        let mut heap = Heap::new();
        let parser = P::load(&mut heap)?;

        let eval_ref = heap.alloc_func(P::eval);
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

    fn dbg(&mut self, refstr: &str) {
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

/// Reads stdin, parses and interprets it as one block.
pub fn batch_main<P: Parser>(sljs: &mut Runtime<P>) -> io::Result<()> {
    let mut input = String::new();
    io::stdin().lock().read_to_string(&mut input)?;

    let result = sljs.evaluate(&input)?;
    println!("{}", sljs.string_from(result));
    Ok(())
}

/// Provides a simple command line using stdin/stdout.
pub fn repl_main<P: Parser>(sljs: &mut Runtime<P>) -> io::Result<()> {
    let stdin = io::stdin();
    let mut input_iter = stdin.lock().lines();

    loop {
        // prompt
        eprint!("sljs> ");
        io::stderr().flush()?;

        // get input
        let maybe_input = input_iter.next();
        let input = match maybe_input {
            None => break,
            Some(input) => input?,
        };
        if input.is_empty() {
            continue;
        }

        if let Some(refstr) = input.strip_prefix(":dbg ") {
            sljs.dbg(refstr);
            continue;
        }

        match sljs.evaluate(&input) {
            Ok(result) => println!("{}", sljs.string_from(result)),
            Err(err) => {
                eprintln!("{}", err);
                if let Err(e) = source::print_callstack(&sljs.heap) {
                    eprintln!("   Exception thrown while getting stack trace: {:?}", e);
                }
            }
        }
    }

    Ok(())
}
