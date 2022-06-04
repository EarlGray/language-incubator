use std::fmt;
use std::io;
use std::io::prelude::*;
use std::str::FromStr;

use atty::Stream;

use slothjs::{
    CallContext,
    Exception,
    Heap,
    HeapNode,
    Interpreted,
    Interpretable,
    JSRef,
    JSValue,
    Program,
    ast::Identifier,
};


#[derive(Debug)]
enum EvalError {
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
            EvalError::Exception(exc) =>
                match exc {
                    Exception::SyntaxTreeError(slothjs::error::ParseError::InvalidJSON{ err }) =>
                        writeln!(f, "syntax error:\n{}", err),
                    _ => write!(f, "sljs error: {:?}", exc),
                }
            EvalError::Serialization(e) => writeln!(f, "serializaiton error:\n{}", e),
            EvalError::Io(e) => writeln!(f, "{}", e),
        }
    }
}

impl From<Exception> for EvalError {
    fn from(exc: Exception) -> Self { EvalError::Exception(exc) }
}

impl From<serde_json::Error> for EvalError {
    fn from(err: serde_json::Error) -> Self { EvalError::Serialization(err) }
}

impl From<io::Error> for EvalError {
    fn from(err: io::Error) -> Self { EvalError::Io(err) }
}

impl From<std::string::FromUtf8Error> for EvalError {
    fn from(e: std::string::FromUtf8Error) -> EvalError {
        EvalError::Io(io::Error::new(io::ErrorKind::InvalidData, e))
    }
}

type EvalResult<T> = Result<T, EvalError>;


struct Runtime {
    heap: Heap,
    parser: EsprimaParser,
}

impl Runtime {
    pub fn load() -> EvalResult<Self> {
        let mut heap = Heap::new();
        let parser = EsprimaParser::load(&mut heap)?;
        Ok(Runtime{ heap, parser })
    }

    pub fn evaluate(&mut self, input: &str) -> EvalResult<JSValue> {
        let estree = self.parser.parse(input, &mut self.heap)?;

        let program = Program::parse_from(estree)
            .map_err(Exception::invalid_ast)?;

        let result = program.interpret(&mut self.heap)?;
        let value = result.to_value(&self.heap)?;
        Ok(value)
    }

    pub fn string_from(&mut self, value: JSValue) -> String {
        value.to_string(&mut self.heap).expect("JSValue.to_string()")
    }

    fn dbg(&mut self, refstr: &str) {
        if let Ok(refnum) = usize::from_str(refstr) {
            let href = unsafe { JSRef::from_index(refnum) };
            let object = self.heap.get(href);
            dbg!(object);
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

    fn repl_main(&mut self) -> io::Result<()> {
        let stdin = io::stdin();
        let mut input_iter = stdin.lock().lines();

        loop {
            // prompt
            eprint!("sljs> ");
            io::stderr().flush()?;

            // get input
            let input = match input_iter.next() {
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
                Err(err) => eprintln!("{}", err),
            }
        }

        Ok(())
    }
}


struct EsprimaParser {
    object: JSRef,
    esparse: JSRef,
}

impl EsprimaParser {
    const ESPRIMA: &'static str = include_str!("../../tmp/esprima.json");

    fn load(heap: &mut Heap) -> EvalResult<Self> {
        let esprima_json = serde_json::from_str::<serde_json::Value>(Self::ESPRIMA)?;

        let esprima = Program::parse_from(&esprima_json)
            .map_err(Exception::SyntaxTreeError)?;

        esprima.interpret(heap)?;

        let object: JSRef = heap.lookup_path(&["esprima"])?.to_ref(heap)?;
        let esparse = (heap.get(object))
            .get_value("parse")
            .ok_or_else(|| Exception::ReferenceNotFound(Identifier::from("esprima.parse")))?
            .to_ref()?;
        Ok(EsprimaParser{ object, esparse })
    }

    fn parse<'heap>(&self, input: &str, heap: &'heap mut Heap) -> EvalResult<HeapNode<'heap>> {
        let estree: Interpreted = heap.execute(
            self.esparse,
            CallContext {
                this_ref: self.object,
                method_name: "parse".to_string(),
                arguments: vec![ Interpreted::from(input), /*{ loc: true },*/ ],
                loc: None,
            },
        )?;
        let node = estree.to_ref(heap)?;
        Ok(HeapNode { heap, node })
    }
}



fn die<E: fmt::Debug>(msg: &str, err: E, errcode: i32) -> ! {
    eprintln!("{}: {:?}", msg, err);
    std::process::exit(errcode);
}

fn main() -> io::Result<()> {
    let interactive = atty::is(Stream::Stdin);
    if interactive {
        eprint!("Loading...");
    }

    let mut runtime = Runtime::load()
        .unwrap_or_else(|e| die("Runtime::load failed", e, 127));

    if interactive {
        eprintln!("\rWelcome to sljs!");
        runtime.repl_main()?;
    } else {
        runtime.batch_main()?;
    }

    Ok(())
}
