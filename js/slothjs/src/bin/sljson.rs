//! This program reads and interprets a JSON-encoded ESTree.
//!
//! ```console
//! $ echo "var a = {one: 1}; a.one" \
//!     | npx esparse \
//!     | cargo run --bin sljson
//! 1
//!
//! # or, the same:
//! $ echo "2 + 2" \
//!     | ./node_modules/.bin/esparse \
//!     | ./target/debug/sljson
//! 4
//! ```

use std::io;

use serde_json::Value as JSON;

use slothjs::interpret::Interpretable;
use slothjs::{
    Exception,
    Heap,
    Program,
};

fn main() -> io::Result<()> {
    let stdin = io::stdin();
    let stdin = stdin.lock();

    // thanks to https://stackoverflow.com/a/55797976/621719
    let deserializer = serde_json::Deserializer::from_reader(stdin);
    for json in deserializer.into_iter::<JSON>() {
        let json = json?;

        let ast = Program::parse_from(&json).map_err(Exception::SyntaxTreeError)?;

        let mut heap = Heap::new();
        let result = ast.interpret(&mut heap)?;

        // TODO: JSON output, optionally
        let value = result.to_value(&heap)?;
        let output = value.to_string(&mut heap)?;
        println!("{}", output);
    }
    Ok(())
}
