use std::env;
use std::fs;
use std::io;
use std::io::prelude::*;
use std::path::PathBuf;
use std::process as proc;

use crate::runtime::{
    self,
    EvalError,
    EvalResult,
};
use crate::{
    error::ParseError,
    CallContext,
    Exception,
    Heap,
    Interpretable,
    Interpreted,
    JSResult,
    Program,
    JSON,
};

pub struct NodejsParser {
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

    fn run_esprima(&self, input: &str) -> EvalResult<String> {
        let tmpdir = (self.espath.parent())
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
            return Err(EvalError::from(Exception::from(ParseError::invalid_ast(
                stderr,
            ))));
        }
        if !stderr.is_empty() {
            eprintln!("{}", stderr);
        }
        Ok(stdout)
    }
}

impl runtime::Parser for NodejsParser {
    fn load(_: &mut Heap) -> EvalResult<Self> {
        let tmpdir = env::temp_dir().join(Self::TMPDIRNAME);

        let tmpdir = tmpdir.as_path();
        fs::create_dir_all(&tmpdir)?;

        let esprima_path = tmpdir.join("esprima.js");
        if !esprima_path.exists() {
            fs::File::create(&esprima_path)?.write_all(Self::ESPRIMA.as_bytes())?;
        }

        let espath = tmpdir.join("esparse.js");
        if !espath.exists() {
            fs::File::create(&espath)?.write_all(Self::ESPARSE.as_bytes())?;
        }

        Ok(NodejsParser { espath })
    }

    fn parse(&self, input: &str, _heap: &mut Heap) -> EvalResult<Program> {
        let stdout = self.run_esprima(input)?;
        let json: JSON = serde_json::from_str(&stdout)?;

        let program = Program::parse_from(&json).map_err(Exception::SyntaxTreeError)?;
        Ok(program)
    }

    fn eval(call: CallContext, heap: &mut Heap) -> JSResult<Interpreted> {
        let code = call.arg_value(0, heap)?.stringify(heap)?;

        let tmpdir = env::temp_dir().join(NodejsParser::TMPDIRNAME);
        let espath = tmpdir.join("esparse.js");
        let parser = NodejsParser { espath };

        let program = parser.parse(&code, heap)?;
        program.interpret(heap)
    }
}
