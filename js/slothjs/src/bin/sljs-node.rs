//! This is the Javascript interpreter and REPL binary:
//!
//! ```sh
//! $ echo "2 + 2" | cargo run -q --bin sljs-node
//! 4
//!
//! // REPL mode (rlwrap is recommended):
//! $ $(which rlwrap) cargo run -q
//! sljs> var a = {one: 1}
//! sljs> a.one + 2
//! 3
//! ```
//!
//! It bundles Esrpima for parsing and relies on `node` executable
//! to execute the Esprima parser. It uses `$TMPDIR/sljs/` to unpack
//! its helper files.

use std::env;
use std::fmt;
use std::str::FromStr;
use std::fs;
use std::io;
use std::io::prelude::*;
use std::path::PathBuf;
use std::process as proc;

use atty::Stream;

use slothjs::{
    Exception,
    Interpretable,
    Heap,
    JSRef,
    JSValue,
    Program,
    JSON,
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
    parser: NodejsParser,
}

impl Runtime {
    pub fn load() -> EvalResult<Self> {
        let parser = NodejsParser::load()?;
        let heap = Heap::new();
        Ok(Runtime{ heap, parser })
    }

    pub fn evaluate(&mut self, input: &str) -> EvalResult<JSValue> {
        let estree = self.parser.parse(input, &mut self.heap)?;

        let program = Program::parse_from(&estree)
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
            Err(e) => eprintln!("{}", e),
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


struct NodejsParser {
    espath: PathBuf,
}

impl NodejsParser {
    const TMPDIRNAME: &'static str = "sljs";
    const ESPRIMA: &'static str = include_str!("../../node_modules/esprima/dist/esprima.js");
    const ESPARSE: &'static str = include_str!("../../node_modules/esprima/bin/esparse.js");
    const NODE: &'static str = if cfg!(target_os = "windows") { "node.exe" } else { "node" };

    pub fn load() -> EvalResult<Self> {
        let espath = Self::prepare_espath()?;
        Ok(NodejsParser{ espath })
    }

    pub fn parse(&self, input: &str, _heap: &mut Heap) -> EvalResult<JSON> {
        let stdout = self.run_esprima(input)?;
        let json = serde_json::from_str(&stdout)?;
        Ok(json)
    }

    /// Prepare a temporary directory, e.g. /tmp/sljs/
    fn prepare_espath() -> io::Result<PathBuf> {
        let tmpdir = env::temp_dir().join(Self::TMPDIRNAME);

        let tmpdir = tmpdir.as_path();
        if !tmpdir.exists() {
            fs::create_dir(&tmpdir)?;
        }

        let esprima_path = tmpdir.join("esprima.js");
        if !esprima_path.exists() {
            fs::File::create(&esprima_path)?.write_all(Self::ESPRIMA.as_bytes())?;
        }

        let esparse_path = tmpdir.join("esparse.js");
        if !esparse_path.exists() {
            fs::File::create(&esparse_path)?.write_all(Self::ESPARSE.as_bytes())?;
        }
        //dbg!(&esparse_path);
        Ok(esparse_path)
    }

    fn run_esprima(&self, input: &str) -> EvalResult<String> {
        let tmpdir = self.espath.parent()
            .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, format!("{:?}", self.espath)))?;
        let mut esparse = proc::Command::new(Self::NODE)
            .arg(&self.espath)
            .arg("--loc")
            .env("NODE_PATH", tmpdir)
            .stdin(proc::Stdio::piped())
            .stdout(proc::Stdio::piped())
            .stderr(proc::Stdio::piped())
            .spawn()?;

        {
            let stdin = esparse.stdin.as_mut().expect("failed to open stdin");
            stdin.write_all(input.as_bytes())?;
        }

        let esparse_output = esparse.wait_with_output()?;

        let stdout = String::from_utf8(esparse_output.stdout)?;
        let stderr = String::from_utf8(esparse_output.stderr)?;
        let status = esparse_output.status;
        if !status.success() {
            let serr = slothjs::error::ParseError::InvalidJSON{ err: stderr };
            let serr = Exception::SyntaxTreeError(serr);
            return Err(EvalError::from(serr));
        }
        if !stderr.is_empty() {
            eprintln!("{}", stderr);
        }
        Ok(stdout)
    }
}


fn die<E: fmt::Debug>(msg: &str, err: E, errcode: i32) -> ! {
    eprintln!("{}: {:?}", msg, err);
    std::process::exit(errcode);
}

fn main() -> io::Result<()> {
    let mut runtime = Runtime::load()
        .unwrap_or_else(|e| die("Runtime::load failed", e, 1));

    if atty::is(Stream::Stdin) {
        runtime.repl_main()?;
    } else {
        runtime.batch_main()?;
    }

    Ok(())
}
