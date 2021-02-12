//! This is the Javascript interpreter:
//! ```sh
//! $ echo "2 + 2" | cargo run --bin sljs
//! 4
//!
//! $ cargo run
//! var a = {one: 1}
//! a.one
//! ^D
//! 1
//! ```
//!
//! It bundles Esrpima for parsing and still relies on nodejs
//! to execute the Esprima parser. It uses $TMPDIR/sljs/ to unpack
//! its auxiliary files.

use std::env;
use std::fmt::Debug;
use std::fs;
use std::io;
use std::io::prelude::*;
use std::path::{
    Path,
    PathBuf,
};
use std::process::{
    Command,
    Stdio,
};

use atty::Stream;

use slothjs::error::ParseError;
use slothjs::interpret::Interpretable;
use slothjs::{
    Exception,
    Heap,
    JSValue,
    Program,
    JSON,
};

const TMPDIRNAME: &str = "sljs";
const ESPRIMA: &str = include_str!("../../node_modules/esprima/dist/esprima.js");
const ESPARSE: &str = include_str!("../../node_modules/esprima/bin/esparse.js");

fn die<E: Debug>(msg: &str, err: E, errcode: i32) -> ! {
    eprintln!("{}: {:?}", msg, err);
    std::process::exit(errcode);
}

fn die_io(msg: &str, err: io::Error, errcode: i32) -> ! {
    eprintln!("{}: {:?}", msg, err.kind());
    eprintln!("{}", err.get_ref().unwrap());
    std::process::exit(errcode);
}

/// Prepare a temporary directory, e.g. /tmp/sljs/
fn prepare_temporary_directory() -> io::Result<PathBuf> {
    let tmpdir = env::temp_dir().join(TMPDIRNAME);

    let tmpdir = tmpdir.as_path();
    if !tmpdir.exists() {
        if let Err(e) = fs::create_dir(&tmpdir) {
            die("Failed to create: ", e, 1);
        }
    }

    let esprima_path = tmpdir.join("esprima.js");
    if !esprima_path.exists() {
        fs::File::create(&esprima_path)?.write_all(ESPRIMA.as_bytes())?;
    }

    let esparse_path = tmpdir.join("esparse.js");
    if !esparse_path.exists() {
        fs::File::create(&esparse_path)?.write_all(ESPARSE.as_bytes())?;
    }
    //eprintln!("esparse_path = {:?}", &esparse_path);
    Ok(esparse_path)
}

fn run_esprima(esparse_path: &Path, input: &str) -> io::Result<(String, String)> {
    let tmpdir = esparse_path.parent().unwrap();
    let mut esparse = Command::new("node")
        .arg(&esparse_path)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .env("NODE_PATH", tmpdir)
        .spawn()?;

    {
        let stdin = esparse.stdin.as_mut().expect("failed to open stdin");
        stdin.write_all(input.as_bytes())?;
    }

    let esparse_output = esparse.wait_with_output()?;

    let stdout = String::from_utf8(esparse_output.stdout)
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;
    let stderr = String::from_utf8(esparse_output.stderr)
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;
    let status = esparse_output.status;
    if !status.success() {
        //eprintln!("{}", stderr);
        if stderr.contains("Unexpected end of input") {
            return Err(io::Error::new(io::ErrorKind::UnexpectedEof, stderr));
        } else {
            let e = format!("Esprima exit code {:?}", status.code());
            return Err(io::Error::new(io::ErrorKind::Other, e));
        }
    }
    Ok((stdout, stderr))
}

enum EvalError {
    /// Esprima was not able to produce AST JSON
    Syntax(io::Error),

    /// JSON from Esprima cannot be parsed by serde_json
    JSON(serde_json::Error),

    /// slothjs parser failed to read Esprima JSON structure
    Parser(ParseError<JSON>),

    /// A run-time exception thrown by the interpreter
    Exception(Exception),

    /// Interpreted value cannot be serialized back to JSON
    Value(Exception),
}

fn evaluate_input(esparse_path: &Path, input: &str, heap: &mut Heap) -> Result<JSValue, EvalError> {
    let (stdout, _stderr) = run_esprima(esparse_path, input).map_err(|e| EvalError::Syntax(e))?;
    let json = serde_json::from_str(&stdout).map_err(|e| EvalError::JSON(e))?;
    let ast = Program::parse_from(&json).map_err(|e| EvalError::Parser(e))?;

    let result = ast.interpret(heap).map_err(|e| EvalError::Exception(e))?;

    result.to_value(heap).map_err(|e| EvalError::Value(e))
}

#[allow(unreachable_code)]
fn batch_main(esparse_path: &Path) -> io::Result<()> {
    let mut input = String::new();
    io::stdin().lock().read_to_string(&mut input)?;

    let mut heap = Heap::new();
    let value = evaluate_input(esparse_path, &input, &mut heap).map_err(|e| {
        match e {
            EvalError::Syntax(e) => die_io("SyntaxError", e, 1),
            EvalError::JSON(e) => die("JSONError", e, 2),
            EvalError::Parser(e) => die("ParseError", e, 3),
            EvalError::Exception(e) => die("Exception", e, 4),
            EvalError::Value(e) => die("ValueError", e, 5),
        };
        io::Error::from(io::ErrorKind::Other)
    })?;
    let output = value
        .to_string(&mut heap)
        .unwrap_or_else(|e| die("Exception", e, 6));
    println!("{}", output);

    Ok(())
}

fn repl_main(esparse_path: &Path) -> io::Result<()> {
    let mut heap = Heap::new();

    let stdin = io::stdin();
    let mut input_iter = stdin.lock().lines();

    loop {
        // prompt
        print!("sljs> ");
        io::stdout().flush().unwrap();

        // get input
        let input = input_iter.next();
        if input.is_none() {
            break;
        }
        let input = input.unwrap().unwrap();
        if input.len() == 0 {
            continue;
        }

        // evaluate
        match evaluate_input(esparse_path, &input, &mut heap) {
            Ok(value) => match value.to_string(&mut heap) {
                Ok(output) => println!("{}", output),
                Err(e) => eprintln!("Exception: {:?}", e),
            },
            Err(EvalError::Syntax(err)) => {
                eprintln!("SyntaxError: {:?}", err.kind());
                eprintln!("{}", err.get_ref().unwrap());
            }
            Err(EvalError::JSON(e)) => die("JSONError", e, 2),
            Err(EvalError::Parser(e)) => {
                eprintln!("ParseError: {:?}", e);
            }
            Err(EvalError::Exception(e)) => {
                eprintln!("Exception: {:?}", e);
            }
            Err(EvalError::Value(e)) => die("ValueError", e, 5),
        };
    }

    Ok(())
}

fn main() -> io::Result<()> {
    let esparse_path = prepare_temporary_directory()?;

    if atty::is(Stream::Stdin) {
        repl_main(&esparse_path)
    } else {
        batch_main(&esparse_path)
    }
}
