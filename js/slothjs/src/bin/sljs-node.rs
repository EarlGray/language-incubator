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

use std::io;

use atty::Stream;

use slothjs::runtime::{
    self,
    NodejsParser,
    Runtime,
};

fn main() -> io::Result<()> {
    let mut sljs = Runtime::<NodejsParser>::load()?;

    if atty::is(Stream::Stdin) {
        runtime::repl_main(&mut sljs)?;
    } else {
        runtime::batch_main(&mut sljs)?;
    }

    Ok(())
}
