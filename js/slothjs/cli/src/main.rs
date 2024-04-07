//! A command-line REPL for sljs.
//!
//! Selecting a parser:
//! - `-E`, `--esprima` select [`EsprimaParser`]
//! - `-N`, `--nodejs` select [`NodejsParser`]
//! - `-J`, `--json` select [`JSONParser`] (deserialization of JSON ESTree)
//! - without flags: if [`NodejsParser::NODE`] works, [`NodejsParser`] is used,
//!   otherwise it falls back to [`EsprimaParser`]. Check the parser with `--debug`.

// TODO: `-e` to evaluate snippets from command line
// TODO: interpret the sources
// TODO: `-j` for JSON output
// TODO: readline, more human-friendly editing
// TODO: tab completion?
// TODO: register native `console.log` and alike

use std::io;
use std::io::prelude::*;
use std::path::PathBuf;

use atty::{self, Stream};

use clap::Parser;
use slothjs::runtime::{EsprimaParser, JSONParser, NodejsParser, Parser as JSParser, Runtime};
use slothjs::source;

/// Reads stdin, parses and interprets it as one block.
pub fn batch_main(sljs: &mut Runtime) -> io::Result<()> {
    let mut input = String::new();
    io::stdin().lock().read_to_string(&mut input)?;

    let result = sljs.evaluate(&input)?;
    println!("{}", sljs.string_from(result));
    Ok(())
}

/// Provides a simple command line using stdin/stdout.
pub fn repl_main(sljs: &mut Runtime) -> io::Result<()> {
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

#[derive(clap::Parser)]
#[clap(author, version, about)]
#[clap(name = "sljs")]
struct Args {
    /// The source files to read, may be empty for stdin.
    sources: Vec<PathBuf>,

    /// Debug output
    #[clap(short, long, action)]
    debug: bool,

    /// Parse sources using Esprima in an external Nodejs process
    #[clap(short = 'N', long, action)]
    nodejs: bool,

    /// Parse sources using an internal Esprima instance \[experimental\]
    #[clap(short = 'E', long, action)]
    esprima: bool,

    /// Expect ESTree AST as JSON input
    #[clap(short = 'J', long, action)]
    json: bool,
}

impl Args {
    fn select_parser(&self) -> io::Result<Box<dyn JSParser>> {
        Ok(match (self.esprima, self.nodejs, self.json) {
            (true, false, false) => Box::new(EsprimaParser::new()),
            (false, true, false) => Box::new(NodejsParser::new()),
            (false, false, true) => Box::new(JSONParser),
            (false, false, false) => {
                if NodejsParser::works()? {
                    Box::new(NodejsParser::new())
                } else {
                    Box::new(EsprimaParser::new())
                }
            }
            _ => {
                return Err(io::Error::new(
                    io::ErrorKind::Other,
                    "Only one of -E/-N/-J can be set",
                ))
            }
        })
    }
}

fn main() -> io::Result<()> {
    let args = Args::parse();
    let parser = args.select_parser()?;
    if args.debug {
        dbg!(&parser);
        dbg!(&args.sources);
    }

    let mut sljs = Runtime::load(parser)?;

    if atty::is(Stream::Stdin) {
        repl_main(&mut sljs)
    } else {
        batch_main(&mut sljs)
    }
}
