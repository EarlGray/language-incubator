use std::fmt;
use std::io;

use atty::Stream;

use slothjs::runtime::{
    EsprimaParser,
    Runtime,
};

fn die<E: fmt::Debug>(msg: &str, err: E, errcode: i32) -> ! {
    eprintln!("{}: {:?}", msg, err);
    std::process::exit(errcode);
}

fn main() -> io::Result<()> {
    let interactive = atty::is(Stream::Stdin);
    if interactive {
        eprint!("Loading...");
    }

    let mut runtime =
        Runtime::<EsprimaParser>::load().unwrap_or_else(|e| die("Runtime::load failed", e, 127));

    if interactive {
        eprintln!("\rWelcome to sljs!");
        runtime.repl_main()?;
    } else {
        runtime.batch_main()?;
    }

    Ok(())
}
