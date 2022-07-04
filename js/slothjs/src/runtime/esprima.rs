use crate::{
    ast::Identifier,
    runtime::{
        self,
        EvalResult,
    },
    CallContext,
    Exception,
    Heap,
    HeapNode,
    Interpretable,
    Interpreted,
    JSRef,
    JSResult,
    Program,
};
use serde_json::json;

pub struct EsprimaParser {
    object: JSRef,
    esparse: JSRef,
    locflag: JSRef,
}

impl EsprimaParser {
    const ESPRIMA: &'static str = include_str!("../../tmp/esprima.json");
}

impl runtime::Parser for EsprimaParser {
    fn load(heap: &mut Heap) -> EvalResult<Self> {
        let esprima_json = serde_json::from_str::<serde_json::Value>(Self::ESPRIMA)?;

        let esprima = Program::parse_from(&esprima_json).map_err(Exception::SyntaxTreeError)?;

        esprima.interpret(heap)?;

        let object: JSRef = heap.lookup_path(&["esprima"])?.to_ref(heap)?;
        let esparse = (heap.get(object))
            .get_own_value("parse")
            .ok_or_else(|| Exception::ReferenceNotFound(Identifier::from("esprima.parse")))?
            .to_ref()?;

        let locjson = json!({ "loc": true });
        let locflag = heap.object_from_json(&locjson).to_ref()?;
        Ok(EsprimaParser {
            object,
            esparse,
            locflag,
        })
    }

    fn parse(&self, input: &str, heap: &mut Heap) -> EvalResult<Program> {
        let mut arguments = vec![Interpreted::from(input)];
        if self.locflag != Heap::NULL {
            arguments.push(Interpreted::from(self.locflag));
        }
        let estree: Interpreted = heap.execute(
            self.esparse,
            CallContext::from(arguments).with_this(self.object).with_name("parse"),
        )?;
        let node = estree.to_ref(heap)?;

        let program =
            HeapNode::with(heap, node, Program::parse_from).map_err(Exception::SyntaxTreeError)?;
        Ok(program)
    }

    fn eval(call: CallContext, heap: &mut Heap) -> JSResult<Interpreted> {
        let code = call.arg_value(0, heap)?.stringify(heap)?;

        let esprima_ref = (heap .get(Heap::GLOBAL) .get_own_value("esprima"))
            .ok_or_else(|| Exception::ReferenceNotFound(Identifier::from("esprima")))?
            .to_ref()?;
        let parse_ref = (heap .get(esprima_ref) .get_own_value("parse"))
            .ok_or_else(|| {
                Exception::TypeErrorGetProperty(Interpreted::from(esprima_ref), "parse".to_string())
            })?
            .to_ref()?;

        let parser = EsprimaParser {
            object: esprima_ref,
            esparse: parse_ref,
            locflag: Heap::NULL,
        };
        let program = parser.parse(&code, heap)?;
        program.interpret(heap)
    }
}
