use std::fmt::Debug;
use std::io;
use std::io::prelude::*;

use atty::Stream;

use slothjs::ast;
use slothjs::interpret::Interpretable;
use slothjs::{
    CallContext,
    Exception,
    Heap,
    HeapNode,
    Interpreted,
    JSRef,
    Program,
};

// TODO: add build.rs to install esprima and create its json tree.
const ESPRIMA: &'static str = include_str!("../../tmp/esprima.json");

fn die<E: Debug>(msg: &str, err: E, errcode: i32) -> ! {
    eprintln!("{}: {:?}", msg, err);
    std::process::exit(errcode);
}

fn load_esprima(heap: &mut Heap) -> Result<(), Exception> {
    let esprima_json = serde_json::from_str::<serde_json::Value>(ESPRIMA)
        .unwrap_or_else(|e| die("Esprima JSON: bundle is not valid", e, 127));

    let esprima = Program::parse_from(&esprima_json)
        .unwrap_or_else(|e| die("Esprima JSON: parsing failed", e, 127));

    esprima.interpret(heap)?;
    Ok(())
}

fn evaluate_input(input: &str, heap: &mut Heap) -> Result<Interpreted, Exception> {
    let esprima_ref: JSRef = heap.lookup_path(&["esprima"])?.to_ref(heap)?;
    let esparse = (heap.get(esprima_ref))
        .get_value("parse")
        .ok_or_else(|| Exception::ReferenceNotFound(ast::Identifier::from("esprima.parse")))?
        .to_ref()?;

    let arguments = vec![
        Interpreted::from(input),
        /*{ loc: true },*/
    ];
    let estree: Interpreted = heap.execute(
        esparse,
        CallContext {
            this_ref: esprima_ref,
            method_name: "parse".to_string(),
            arguments,
            loc: None,
        },
    )?;
    let estree = HeapNode {
        heap,
        node: estree.to_ref(heap)?,
    };

    let program = Program::parse_from(&estree).unwrap();

    program.interpret(heap)
}

fn batch_main(heap: &mut Heap) -> io::Result<()> {
    let mut input = String::new();
    io::stdin().lock().read_to_string(&mut input)?;

    let result = evaluate_input(&input, heap).unwrap();
    let output = result.to_value(heap).unwrap().to_string(heap).unwrap();
    println!("{}", output);
    Ok(())
}

fn repl_main(heap: &mut Heap) -> io::Result<()> {
    let stdin = io::stdin();
    let mut input_iter = stdin.lock().lines();

    loop {
        // prompt
        print!("sljs> ");
        io::stdout().flush().unwrap();

        // get input
        let input = input_iter.next();
        if input.is_none() {
            break;
        }
        let input = input.unwrap().unwrap();
        if input.len() == 0 {
            continue;
        }

        match evaluate_input(&input, heap) {
            Ok(result) => {
                let result = result.to_value(heap).expect("result value");
                let result = result.to_json(heap).expect("result json");
                println!("{}", result);
            }
            Err(e) => eprintln!("Exception: {:?}", e),
        }
    }

    Ok(())
}

fn main() -> io::Result<()> {
    let interactive = atty::is(Stream::Stdin);

    let mut heap = Heap::new();

    if interactive {
        eprint!("Loading...");
    }
    load_esprima(&mut heap).unwrap_or_else(|e| die("Esprima loading failed", e, 127));

    if interactive {
        eprintln!("\rWelcome to sljs!");
        repl_main(&mut heap)?;
    } else {
        batch_main(&mut heap).unwrap_or_else(|e| die("Error: ", e, 1));
    }

    Ok(())
}
