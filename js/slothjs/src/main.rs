use std::io;
use std::fmt::Debug;

use serde_json::Value as JSON;


#[derive(Debug)]
enum Error<'a> {
    ShouldBeString{ value: &'a JSON },
    ShouldBeObject{ value: &'a JSON },
    ShouldBeArray{ value: &'a JSON },
    ObjectWithout{ value: &'a JSON, attr: &'static str },
    UnexpectedValue{ value: &'a JSON, want: &'static str },
    UnknownType{ value: &'a JSON, ty: &'a str},
}

trait Interptetable {
    fn interpret(&self, state: &mut RuntimeState) -> Result<Box<JSON>, Error>;
}

trait ASTNode: Interptetable {
    const TYPE_ID: &'static str;

    fn from_json(value: &JSON) -> Result<Box<Self>, Error>;

}

trait Statement: ASTNode {}

struct Program {
    body: Vec<Box<dyn Interptetable>>,
}

impl Program {
    fn from_json(json: &JSON) -> Result<Box<Self>, Error> {
        let object = json.as_object()
            .ok_or(Error::ShouldBeObject{ value: json })?;

        // check type
        let ty = object.get("type")
            .ok_or(Error::ObjectWithout{ value: json, attr: "type" })?;
        if ty.as_str() != Some("Program") {
            return Err(Error::UnexpectedValue{
                value: &ty,
                want: "Program"
            });
        };

        // parse body
        let jbody = json.get("body")
            .ok_or(Error::ObjectWithout{ value: json, attr: "body"})?;
        let jbody = jbody.as_array()
            .ok_or(Error::ShouldBeArray{ value: jbody })?;

        let mut body = Vec::<Box<dyn Interptetable>>::new();
        for jstmt in jbody.iter() {
            let jty = jstmt.get("type")
                .ok_or(Error::ObjectWithout{ value: jstmt, attr: "type" })?;
            let ty = jty.as_str()
                .ok_or(Error::ShouldBeString{ value: jty })?;

            let stmt = match ty {
                "ExpressionStatement" =>
                    ExpressionStatement::from_json(jstmt)?,
                _ =>
                    return Err(Error::UnknownType{ value: jstmt, ty: ty})
            };
            body.push(stmt);
        }

        Ok(Box::new(Program { body }))
    }

    fn interpret(&self) -> Result<Box<JSON>, Error> {
        let mut runtime = RuntimeState{};
        let mut result = Box::new(JSON::Null);
        for stmt in self.body.iter() {
            result = stmt.interpret(&mut runtime)?;
        }
        Ok(result)
    }
}

// ==============================================
enum Expr {
    Literal(JSON),
}

struct ExpressionStatement {
    expression: Expr,
}

impl ASTNode for ExpressionStatement {
    const TYPE_ID: &'static str = "ExpressionStatement";

    fn from_json(value: &JSON) -> Result<Box<Self>, Error> {
        let ty = value.get("type")
            .ok_or(Error::ObjectWithout{ value, attr: "type" })?;
        if ty.as_str() != Some(Self::TYPE_ID) {
            return Err(Error::UnexpectedValue{
                value: &ty,
                want: Self::TYPE_ID
            });
        }

        let jexpr = value.get("expression")
            .ok_or(Error::ObjectWithout{ value, attr: "expression"})?;
        let jexpr_ty = jexpr.get("type")
            .ok_or(Error::ObjectWithout{ value, attr: "type"})?;
        let jexpr_ty = jexpr_ty.as_str()
            .ok_or(Error::ShouldBeString{ value })?;

        let expression = match jexpr_ty {
            "Literal" => {
                let jval = jexpr.get("value")
                    .ok_or(Error::ObjectWithout{ value: jexpr, attr: "value" })?;
                Expr::Literal(jval.clone().take())
            }
            _ =>
                return Err(Error::UnknownType{ value: jexpr, ty: jexpr_ty}),
        };

        Ok(Box::new(ExpressionStatement { expression }))
    }
}

impl Interptetable for ExpressionStatement {
    fn interpret(&self, _state: &mut RuntimeState) -> Result<Box<JSON>, Error> {
        match &self.expression {
            Expr::Literal(val) => Ok(Box::new(val.clone())),
        }
    }
}

// ==============================================

struct RuntimeState {}


// ==============================================

fn die<T, E: Debug>(msg: &str, exitcode: i32) -> impl FnOnce(E) -> T
{
    let msg = msg.to_string();
    move |e: E| {
        eprintln!("{}: {:?}", msg, e);
        std::process::exit(exitcode)
    }
}

fn main() {
    let stdin = io::stdin();
    let stdin = stdin.lock();

    // thanks to https://stackoverflow.com/a/55797976/621719
    let deserializer= serde_json::Deserializer::from_reader(stdin);
    for json in deserializer.into_iter::<JSON>() {
        let json = json.unwrap_or_else(die("JSON error", 1));

        let ast = Program::from_json(&json)
            .unwrap_or_else(die("Parse error", 2));

        let result = ast.interpret()
            .unwrap_or_else(die("Interpretation error", 3));

        // TODO: JSON output, optionally
        let output = serde_json::to_string(&result)
            .unwrap_or_else(die("Result serialization error", 4));

        println!("{}", output);
    }
}
