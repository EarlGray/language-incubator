mod esprima;
mod nodejs;

use std::io;
use std::io::prelude::*;

use crate::prelude::*;
use crate::{
    error,
    source,
    Exception,
    Heap,
    Interpretable,
    JSValue,
    Program,
    JSON,
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

pub type EvalResult<T> = Result<T, EvalError>;

pub trait Parser: Sized {
    fn load(heap: &mut Heap) -> EvalResult<Self>;
    fn parse(&self, input: &str, heap: &mut Heap) -> EvalResult<Program>;
}

pub struct Runtime<P> {
    pub heap: Heap,
    parser: P,
}

impl<P: Parser> Runtime<P> {
    pub fn load() -> EvalResult<Self> {
        let mut heap = Heap::new();
        let parser = P::load(&mut heap)?;
        Ok(Runtime { heap, parser })
    }

    pub fn evaluate(&mut self, input: &str) -> EvalResult<JSValue> {
        let program = self.parser.parse(input, &mut self.heap)?;

        let result = program.interpret(&mut self.heap)?;
        let value = result.to_value(&self.heap)?;
        Ok(value)
    }

    pub fn json_from(&mut self, value: JSValue) -> JSON {
        value.to_json(&self.heap).expect("JSValue.to_json()")
    }

    pub fn string_from(&mut self, value: JSValue) -> String {
        value
            .to_string(&mut self.heap)
            .expect("JSValue.to_string()")
    }

    fn dbg(&mut self, refstr: &str) {
        if let Ok(refnum) = usize::from_str(refstr) {
            match self.heap.get_index(refnum) {
                Some(object) => { dbg!(object); }
                None => { eprintln!("No object at #{}", refnum); }
            };
        } else {
            // TODO: evaluate an expression to a reference
            eprintln!("Command error: expected a number");
        }
    }

    pub fn batch_main(&mut self) -> io::Result<()> {
        let mut input = String::new();
        io::stdin().lock().read_to_string(&mut input)?;

        match self.evaluate(&input) {
            Ok(result) => println!("{}", self.string_from(result)),
            Err(err) => eprintln!("{}", err),
        }

        Ok(())
    }

    pub fn repl_main(&mut self) -> io::Result<()> {
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
                self.dbg(refstr);
                continue;
            }

            match self.evaluate(&input) {
                Ok(result) => println!("{}", self.string_from(result)),
                Err(err) => {
                    eprintln!("{}", err);
                    if let Err(e) = source::print_callstack(&self.heap) {
                        eprintln!("   Exception thrown while getting stack trace: {:?}", e);
                    }
                }
            }
        }

        Ok(())
    }
}