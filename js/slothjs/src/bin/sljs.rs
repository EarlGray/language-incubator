//! A wrapper for executing [`slothjs::runtime::Runtime`] with [`slothjs::runtime::EsprimaParser`]
//! on stdin/stdout.
//!
//! ```console
//! // REPL mode (rlwrap is recommended):
//! $ alias sljs="$(which rlwrap) cargo run -q --bin sljs"
//! $ sljs
//! sljs> var a = {one: 1}
//! sljs> a.one + 2
//! 3
//! ```

// TODO: readline, more human-friendly editing.
// TODO: tab-completion?

use std::io;

use atty::Stream;

use slothjs::runtime::{
    self,
    EsprimaParser,
    Runtime,
};

fn main() -> io::Result<()> {
    // TODO: parse arguments, if they are files, interpret the files.
    let interactive = atty::is(Stream::Stdin);
    if interactive {
        eprint!("Loading...");
    }

    let mut sljs = Runtime::<EsprimaParser>::load()?;

    if interactive {
        eprintln!("\rWelcome to sljs!");
        runtime::repl_main(&mut sljs)?;
    } else {
        runtime::batch_main(&mut sljs)?;
    }

    Ok(())
}
