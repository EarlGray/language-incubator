use std::io;

use serde_json::{Value as JSON};
use serde_json::json;

fn read_json_ast<T: io::Read>(input_channel: &mut T) -> io::Result<JSON> {
    let mut input = String::new();
    input_channel.read_to_string(&mut input)?;

    let json = serde_json::from_str(&input)?;
    Ok(json)
}

fn interpret_ast(ast: &JSON) -> Option<&JSON> {
    let ast = ast.as_object().unwrap();
    assert!(ast.get("type") == Some(&json!("Program")));

    // TODO: actually interpret the body
    ast.get("body")
}

fn main() {
    // Read the AST JSON from the standard input
    let ast = read_json_ast(&mut io::stdin()).unwrap();
 
    // TODO : Interpret it!
    let result = interpret_ast(&ast).unwrap();
    println!("{}", serde_json::to_string(&result).unwrap());
}
