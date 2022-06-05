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
use std::fs;
use std::io;
use std::io::prelude::*;
use std::path::PathBuf;
use std::process as proc;

use atty::Stream;

use slothjs::{
    runtime::{
        EvalError,
        EvalResult,
        Runtime,
    },
    Exception,
    Heap,
    Program,
};

struct NodejsParser {
    espath: PathBuf,
}

impl NodejsParser {
    const TMPDIRNAME: &'static str = "sljs";
    const ESPRIMA: &'static str = include_str!("../../node_modules/esprima/dist/esprima.js");
    const ESPARSE: &'static str = include_str!("../../node_modules/esprima/bin/esparse.js");
    const NODE: &'static str = if cfg!(target_os = "windows") {
        "node.exe"
    } else {
        "node"
    };

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
        let tmpdir = self
            .espath
            .parent()
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
            let serr = slothjs::error::ParseError::InvalidJSON { err: stderr };
            let serr = Exception::SyntaxTreeError(serr);
            return Err(EvalError::from(serr));
        }
        if !stderr.is_empty() {
            eprintln!("{}", stderr);
        }
        Ok(stdout)
    }
}

impl slothjs::runtime::Parser for NodejsParser {
    fn load(_: &mut Heap) -> EvalResult<Self> {
        let espath = Self::prepare_espath()?;
        Ok(NodejsParser { espath })
    }

    fn parse(&self, input: &str, _heap: &mut Heap) -> EvalResult<Program> {
        let stdout = self.run_esprima(input)?;
        let json: slothjs::JSON = serde_json::from_str(&stdout)?;

        let program = Program::parse_from(&json).map_err(Exception::invalid_ast)?;
        Ok(program)
    }
}

fn die<E: fmt::Debug>(msg: &str, err: E, errcode: i32) -> ! {
    eprintln!("{}: {:?}", msg, err);
    std::process::exit(errcode);
}

fn main() -> io::Result<()> {
    let mut runtime =
        Runtime::<NodejsParser>::load().unwrap_or_else(|e| die("Runtime::load failed", e, 1));

    if atty::is(Stream::Stdin) {
        runtime.repl_main()?;
    } else {
        runtime.batch_main()?;
    }

    Ok(())
}
