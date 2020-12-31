/// This is the Javascript interpreter:
/// ```sh
/// $ echo "2 + 2" | cargo run --bin sljs
/// 4
///
/// $ cargo run
/// var a = {one: 1}
/// a.one
/// ^D
/// 1
/// ```
///
/// It bundles Esrpima for parsing and still relies on nodejs
/// to execute the Esprima parser. It uses $TMPDIR/sljs/ to unpack
/// its auxiliary files.

use std::fmt::Debug;
use std::convert::TryFrom;
use std::env;
use std::process::{Command, Stdio};
use std::fs;
use std::io;
use std::io::prelude::*;
use std::path::{Path, PathBuf};

use slothjs::ast::Program;
use slothjs::interpret::{
    Interpretable,
    RuntimeState,
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
        fs::File::create(&esprima_path)?
            .write_all(ESPRIMA.as_bytes())?;
    }

    let esparse_path = tmpdir.join("esparse.js");
    if !esparse_path.exists() {
        fs::File::create(&esparse_path)?
            .write_all(ESPARSE.as_bytes())?;
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
        let stdin = esparse.stdin.as_mut()
            .expect("failed to open stdin");
        stdin.write_all(input.as_bytes())?;
    }

    let esparse_output = esparse.wait_with_output()?;

    let stdout = String::from_utf8(esparse_output.stdout).map_err(|e| {
        io::Error::new(io::ErrorKind::InvalidData, e)
    })?;
    let stderr = String::from_utf8(esparse_output.stderr).map_err(|e| {
        io::Error::new(io::ErrorKind::InvalidData, e)
    })?;
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

fn main() -> io::Result<()> {
    // TODO: REPL mode
    let esparse_path = prepare_temporary_directory()?;

    let mut input  = String::new();
    io::stdin().lock().read_to_string(&mut input)?;

    let (stdout, _stderr) = run_esprima(&esparse_path, &input)
        .unwrap_or_else(|e| die_io("Syntax error", e, 1) );
    let json = serde_json::from_str(&stdout)
        .unwrap_or_else(|e| die("JSON error", e, 2) );
    let ast = Program::try_from(&json)
        .unwrap_or_else(|e| die("Parser error", e, 3) );

    let mut state = RuntimeState::new();
    let result = ast.interpret(&mut state)
        .unwrap_or_else(|e| die("Error", e, 4));

    let value = result.to_value(&state.heap)
        .unwrap_or_else(|e| die("Value error", e, 5));
    let output = value.to_string(&state.heap);
    println!("{}", output);

    Ok(())
}
