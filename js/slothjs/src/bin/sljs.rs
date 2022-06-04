use std::fmt::Debug;
use std::io;
use std::io::prelude::*;
use std::str::FromStr;

use atty::Stream;

use slothjs::{
    CallContext,
    Exception,
    Heap,
    HeapNode,
    Interpreted,
    Interpretable,
    JSRef,
    JSValue,
    Program,
    ast::Identifier,
};

fn die<E: Debug>(msg: &str, err: E, errcode: i32) -> ! {
    eprintln!("{}: {:?}", msg, err);
    std::process::exit(errcode);
}

struct EsprimaParser {
    object: JSRef,
    esparse: JSRef,
}

impl EsprimaParser {
    const ESPRIMA: &'static str = include_str!("../../tmp/esprima.json");

    fn load(heap: &mut Heap) -> Result<Self, Exception> {
        let esprima_json = serde_json::from_str::<serde_json::Value>(Self::ESPRIMA)
            .unwrap_or_else(|e| die("Esprima JSON: bundle is not valid", e, 127));

        let esprima = Program::parse_from(&esprima_json)
            .unwrap_or_else(|e| die("Esprima JSON: parsing failed", e, 127));

        esprima.interpret(heap)?;

        let object: JSRef = heap.lookup_path(&["esprima"])?.to_ref(heap)?;
        let esparse = (heap.get(object))
            .get_value("parse")
            .ok_or_else(|| Exception::ReferenceNotFound(Identifier::from("esprima.parse")))?
            .to_ref()?;
        Ok(EsprimaParser{ object, esparse })
    }

    fn parse<'heap>(&self, input: &str, heap: &'heap mut Heap) -> Result<HeapNode<'heap>, Exception> {
        let estree: Interpreted = heap.execute(
            self.esparse,
            CallContext {
                this_ref: self.object,
                method_name: "parse".to_string(),
                arguments: vec![ Interpreted::from(input), /*{ loc: true },*/ ],
                loc: None,
            },
        )?;
        let node = estree.to_ref(heap)?;
        Ok(HeapNode { heap, node })
    }
}


struct Runtime {
    heap: Heap,
    parser: EsprimaParser,
}

impl Runtime {
    fn load() -> Result<Self, Exception> {
        let mut heap = Heap::new();
        let parser = EsprimaParser::load(&mut heap)?;
        Ok(Runtime{ heap, parser })
    }

    fn evaluate(&mut self, input: &str) -> Result<JSValue, Exception> {
        let estree = self.parser.parse(input, &mut self.heap)?;

        let program = Program::parse_from(estree)
            .map_err(Exception::invalid_ast)?;

        let result = program.interpret(&mut self.heap)?;
        result.to_value(&self.heap)
    }

    fn string_from(&mut self, value: JSValue) -> String {
        value.to_string(&mut self.heap).expect("JSValue.to_string()")
    }

    fn dbg(&mut self, refstr: &str) {
        if let Ok(refnum) = usize::from_str(refstr) {
            let href = unsafe { JSRef::from_index(refnum) };
            let object = self.heap.get(href);
            dbg!(object);
        } else {
            // TODO: evaluate an expression to a reference
            eprintln!("Command error: expected a number");
        }
    }
}


fn batch_main(runtime: &mut Runtime) -> io::Result<()> {
    let mut input = String::new();
    io::stdin().lock().read_to_string(&mut input)?;

    match runtime.evaluate(&input) {
        Ok(result) => {
            println!("{}", runtime.string_from(result));
        }
        Err(e) => {
            eprintln!("Exception: {:?}", e);
        }
    };
    Ok(())
}

fn repl_main(runtime: &mut Runtime) -> io::Result<()> {
    let stdin = io::stdin();
    let mut input_iter = stdin.lock().lines();

    loop {
        // prompt
        print!("sljs> ");
        io::stdout().flush()?;

        // get input
        let input = match input_iter.next() {
            None => break,
            Some(input) => input?,
        };
        if input.is_empty() {
            continue;
        }

        if let Some(refstr) = input.strip_prefix(":dbg ") {
            runtime.dbg(refstr);
            continue;
        }

        match runtime.evaluate(&input) {
            Ok(result) => {
                println!("{}", runtime.string_from(result));
            }
            Err(e) => {
                eprintln!("Exception: {:?}", e);
            }
        }
    }

    Ok(())
}

fn main() -> io::Result<()> {
    let interactive = atty::is(Stream::Stdin);
    if interactive {
        eprint!("Loading...");
    }

    let mut runtime = Runtime::load()
        .unwrap_or_else(|e| die("Esprima loading failed", e, 127));

    if interactive {
        eprintln!("\rWelcome to sljs!");
        repl_main(&mut runtime)?;
    } else {
        batch_main(&mut runtime)
            .unwrap_or_else(|e| die("Error: ", e, 1));
    }

    Ok(())
}
