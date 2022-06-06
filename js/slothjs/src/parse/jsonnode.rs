use std::env;
use std::io;
use std::io::prelude::*;
use std::fs;
use std::process as proc;
use std::path::PathBuf;

use serde_json::json;

use crate::ast::*;
use crate::error::ParseError;
use crate::parse::{
    ParseResult,
    SourceNode,
};
use crate::runtime::{self, EvalError, EvalResult};
use crate::source;

use crate::{
    Exception,
    Heap,
    JSON,
};

impl SourceNode for JSON {
    type Error = JSON;

    fn to_error(&self) -> Self::Error {
        self.clone()
    }

    fn get_location(&self) -> Option<source::Location> {
        let start = match self.map_node("start", |child| Ok(child.clone())) {
            Err(_) => return None,
            Ok(node) => node,
        };
        let end = match self.map_node("end", |child| Ok(child.clone())) {
            Err(_) => return None,
            Ok(node) => node,
        };
        let jloc = json!({"start": start, "end": end});
        serde_json::from_value::<source::Location>(jloc).ok()
    }

    fn get_literal(&self, property: &str) -> ParseResult<Literal, Self::Error> {
        let node =
            (self.get(property)).ok_or_else(|| ParseError::no_attr(property, self.clone()))?;
        Ok(Literal(node.clone()))
    }

    fn map_node<T, F>(&self, property: &str, mut action: F) -> ParseResult<T, Self::Error>
    where
        F: FnMut(&Self) -> ParseResult<T, Self::Error>,
    {
        let child = self
            .get(property)
            .ok_or_else(|| ParseError::no_attr(property, self.clone()))?;
        if child.is_null() {
            return Err(ParseError::no_attr(property, self.clone()))?;
        }
        action(child)
    }

    fn get_bool(&self, property: &str) -> ParseResult<bool, Self::Error> {
        let child = self
            .get(property)
            .ok_or_else(|| ParseError::no_attr(property, self.clone()))?;
        child.as_bool().ok_or_else(|| ParseError::ShouldBeBool {
            value: self.clone(),
        })
    }

    fn get_str(&self, property: &str) -> ParseResult<String, Self::Error> {
        let child = self
            .get(property)
            .ok_or_else(|| ParseError::no_attr(property, self.to_error()))?;
        let s = child.as_str().ok_or_else(|| ParseError::ShouldBeString {
            value: self.to_error(),
        })?;
        Ok(s.to_string())
    }

    fn map_array<T, F>(&self, property: &str, func: F) -> ParseResult<Vec<T>, Self::Error>
    where
        F: FnMut(&Self) -> ParseResult<T, Self::Error>,
    {
        let jarray =
            (self.get(property)).ok_or_else(|| ParseError::no_attr(property, self.to_error()))?;
        let array = (jarray.as_array()).ok_or_else(|| ParseError::ShouldBeArray {
            value: JSON::Null.to_error(),
        })?;
        array.iter().map(func).collect()
    }
}



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
            let serr = Exception::invalid_ast(stderr);
            return Err(EvalError::from(serr));
        }
        if !stderr.is_empty() {
            eprintln!("{}", stderr);
        }
        Ok(stdout)
    }
}

impl runtime::Parser for NodejsParser {
    fn load(_: &mut Heap) -> EvalResult<Self> {
        let espath = Self::prepare_espath()?;
        Ok(NodejsParser { espath })
    }

    fn parse(&self, input: &str, _heap: &mut Heap) -> EvalResult<Program> {
        let stdout = self.run_esprima(input)?;
        let json: JSON = serde_json::from_str(&stdout)?;

        let program = Program::parse_from(&json).map_err(Exception::invalid_ast)?;
        Ok(program)
    }
}