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

use std::fmt;
use std::io;

use atty::Stream;

use slothjs::runtime::{
    NodejsParser,
    Runtime,
};

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
