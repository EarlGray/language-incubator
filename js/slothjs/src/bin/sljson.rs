/// This program interprets the JSON output of esprima:
///
/// ```sh
/// $ echo "var a = {one: 1}; a.one" \
///     | npm run --silent esparse \
///     | cargo run --bin sljson
/// 1
///
/// # or, the same:
/// $ echo "2 + 2" \
///     | ./node_modules/.bin/esparse \
///     | ./target/debug/sljson
/// 4
/// ```

use std::io;
use std::fmt::Debug;
use std::convert::TryFrom;

use serde_json::Value as JSON;

use slothjs::{Heap, Program};
use slothjs::interpret::Interpretable;

fn die<E: Debug>(msg: &str, err: E, errcode: i32) -> ! {
    eprintln!("{}: {:?}", msg, err);
    std::process::exit(errcode);
}

fn main() {
    let stdin = io::stdin();
    let stdin = stdin.lock();

    // thanks to https://stackoverflow.com/a/55797976/621719
    let deserializer = serde_json::Deserializer::from_reader(stdin);
    for json in deserializer.into_iter::<JSON>() {
        let json = json
            .unwrap_or_else(|e| die("JSON error", e, 1));

        let ast = Program::try_from(&json)
            .unwrap_or_else(|e| die("Parse error", e, 2));

        let mut heap = Heap::new();
        let result = ast.interpret(&mut heap)
            .unwrap_or_else(|e| die("Interpretation error", e, 3));

        // TODO: JSON output, optionally
        let value = result.to_value(&heap).unwrap();
        println!("{}", value.to_string(&heap));
    }
}
