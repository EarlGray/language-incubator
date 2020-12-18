use std::io;
use std::fmt::Debug;
use std::collections::HashMap;

use serde_json::Value as JSON;
use serde_json::json;


#[derive(Debug)]
enum ParseError<'a> {
    ShouldBeString{ value: &'a JSON },
    ShouldBeArray{ value: &'a JSON },
    //ShouldBeObject{ value: &'a JSON },
    ObjectWithout{ attr: &'static str, value: &'a JSON},
    UnexpectedValue{ value: &'a JSON, want: &'static str },
    UnknownType{ ty: &'a str, value: &'a JSON },
}

#[derive(Debug)]
enum Exception {
    ReferenceError(String),
}

#[derive(Debug, Clone)]
struct JSValue(JSON);

/*
 *  JSON helpers
 */
fn json_get<'a>(json: &'a JSON, property: &'static str) -> Result<&'a JSON, ParseError<'a>> {
    json.get(property)
        .ok_or(ParseError::ObjectWithout{ attr: property, value: json })
}

fn json_get_str<'a>(
    json: &'a JSON,
    property: &'static str
) -> Result<&'a str, ParseError<'a>> {
    let jstr = json_get(json, property)?;
    jstr.as_str().ok_or(ParseError::ShouldBeString{ value: jstr })
}

fn json_get_array<'a>(
    json: &'a JSON,
    property: &'static str
) -> Result<&'a Vec<JSON>, ParseError<'a>> {
    let jarray = json_get(json, property)?;
    jarray.as_array().ok_or(ParseError::ShouldBeArray{ value: jarray })
}

fn json_expect_str<'a>(
    json: &'a JSON,
    property: &'static str,
    value: &'static str
) -> Result<(), ParseError<'a>> {
    let jstr = json_get(json, property)?;
    let got = jstr.as_str().ok_or(ParseError::ShouldBeString{ value: jstr })?;
    if got == value {
        Ok(())
    } else {
        Err(ParseError::UnexpectedValue{ value: jstr, want: value })
    }
}


trait Interpretable {
    fn interpret(&self, state: &mut RuntimeState) -> Result<JSValue, Exception>;
}

trait ASTNode: Interpretable {
    const TYPE_ID: &'static str;

    fn from_json(value: &JSON) -> Result<Box<Self>, ParseError>;
}

struct Program {
    body: Vec<Box<dyn Interpretable>>,
}

impl Program {
    fn from_json(json: &JSON) -> Result<Box<Self>, ParseError> {
        json_expect_str(json, "type", "Program")?;

        // parse body
        let jbody = json_get_array(json, "body")?;

        let mut body = Vec::new();
        for jstmt in jbody.iter() {
            let ty = json_get_str(jstmt, "type")?;
            let stmt: Box<dyn Interpretable> = match ty {
                ExpressionStatement::TYPE_ID =>
                    ExpressionStatement::from_json(jstmt)?,
                VariableDeclaration::TYPE_ID =>
                    VariableDeclaration::from_json(jstmt)?,
                _ =>
                    return Err(ParseError::UnknownType{ value: jstmt, ty})
            };
            body.push(stmt);
        }

        Ok(Box::new(Program { body }))
    }

    fn interpret(&self) -> Result<JSValue, Exception> {
        let mut runtime = RuntimeState{
            variables: HashMap::new(),
        };
        let mut result = JSValue(JSON::Null);
        for stmt in self.body.iter() {
            result = stmt.interpret(&mut runtime)?;
        }
        Ok(result)
    }
}

// ==============================================
enum Expr {
    Literal(JSValue),
    Identifier(String),
    BinaryOp(Box<Expr>, BinOp, Box<Expr>),
    Call(Box<Expr>, Vec<Box<Expr>>),
    Array(Vec<Box<Expr>>),
}

enum BinOp { Add }

impl Expr {
    fn from_json(jexpr: &JSON) -> Result<Expr, ParseError> {
        let jexpr_ty = json_get_str(jexpr, "type")?;

        let expr = match jexpr_ty {
            "ArrayExpression" => {
                let jelements = json_get_array(jexpr, "elements")?;
                let elements: Result<Vec<Box<Expr>>, ParseError> = jelements.iter()
                    .map(|j| Expr::from_json(j).map(|e| Box::new(e)))
                    .collect();
                Expr::Array(elements?)
            }
            "BinaryExpression" => {
                let jleft = json_get(jexpr, "left")?;
                let left = Expr::from_json(&jleft)?;

                let jright = json_get(jexpr, "right")?;
                let right = Expr::from_json(&jright)?;

                let opstr = json_get_str(jexpr, "operator")?;
                let op = match opstr {
                    "+" => BinOp::Add,
                    _ => return Err(ParseError::UnexpectedValue{
                        value: jexpr.get("operator").unwrap(),
                        want: "+|"
                    }),
                };
                Expr::BinaryOp(Box::new(left), op, Box::new(right))
            }
            "CallExpression" => {
                let jcallee = json_get(jexpr, "callee")?;
                let callee = Expr::from_json(jcallee)?;

                let jarguments = json_get_array(jexpr, "arguments")?;
                let arguments: Result<Vec<Box<Expr>>, ParseError> = jarguments.iter()
                    .map(|j| Expr::from_json(j).map(|e| Box::new(e)))
                    .collect();

                Expr::Call(Box::new(callee), arguments?)
            }
            "Identifier" => {
                let name = json_get_str(jexpr, "name")?;
                Expr::Identifier(name.to_string())
            }
            "Literal" => {
                let jval = json_get(jexpr, "value")?;
                Expr::Literal(JSValue(jval.clone().take()))
            }
            _ =>
                return Err(ParseError::UnknownType{ value: jexpr, ty: jexpr_ty}),
        };
        Ok(expr)
    }

    fn interpret(&self, state: &mut RuntimeState) -> Result<JSValue, Exception> {
        match self {
            Expr::Literal(val) =>
                Ok(val.clone()),
            Expr::Identifier(name) =>
                state.lookup_identifier(&name),
            Expr::BinaryOp(lexpr, op, rexpr) => {
                let lval = lexpr.interpret(state)?;
                let rval = rexpr.interpret(state)?;
                match op {
                    BinOp::Add => {
                        if let Some(lnum) = lval.0.as_f64() {
                            if let Some(rnum) = rval.0.as_f64() {
                                return Ok(JSValue(json!(lnum + rnum)));
                            }
                        }
                        panic!("TODO: adding non-numbers");
                    }
                }
            }
            Expr::Call(_callee, _arguments) => {
                panic!("TODO: Expr::Call");
            }
            Expr::Array(elements) => {
                let mut arr = vec![];
                for elem in elements.iter() {
                    let value = elem.interpret(state)?;
                    arr.push(value.0);
                }
                Ok(JSValue(JSON::Array(arr)))
            }
        }
    }
}


struct ExpressionStatement {
    expression: Expr,
}

impl ASTNode for ExpressionStatement {
    const TYPE_ID: &'static str = "ExpressionStatement";

    fn from_json(value: &JSON) -> Result<Box<Self>, ParseError> {
        json_expect_str(value, "type", Self::TYPE_ID)?;

        let jexpr = json_get(value, "expression")?;
        let expression = Expr::from_json(jexpr)?;
        Ok(Box::new(ExpressionStatement { expression }))
    }
}

impl Interpretable for ExpressionStatement {
    fn interpret(&self, state: &mut RuntimeState) -> Result<JSValue, Exception> {
        self.expression.interpret(state)
    }
}

struct VariableDeclarator {
    name: String,
    init: Option<Box<Expr>>,
}

enum DeclarationKind { Var, Let, Const }

struct VariableDeclaration {
    kind: DeclarationKind,
    declarations: Vec<VariableDeclarator>,
}

impl ASTNode for VariableDeclaration {
    const TYPE_ID: &'static str = "VariableDeclaration";

    fn from_json(value: &JSON) -> Result<Box<Self>, ParseError> {
        json_expect_str(value, "type", Self::TYPE_ID)?;

        let kind = match json_get_str(value, "kind")? {
            "const" => DeclarationKind::Const,
            "let" => DeclarationKind::Let,
            "var" => DeclarationKind::Var,
            _ => return Err(ParseError::UnexpectedValue{
                value: value.get("kind").unwrap(),
                want: "const|let|var",
            }),
        };
        let jdeclarations = json_get_array(value, "declarations")?;

        let mut declarations = vec![];
        for decl in jdeclarations.iter() {
            json_expect_str(decl, "type", "VariableDeclarator")?;

            let jid = json_get(decl, "id")?;
            json_expect_str(jid, "type", "Identifier")?;
            let name = json_get_str(jid, "name")?.to_string();

            let jinit = json_get(decl, "init")?;
            let init = match jinit {
                JSON::Null => None,
                _ => {
                    let expr = Expr::from_json(jinit)?;
                    Some(Box::new(expr))
                }
            };

            declarations.push(VariableDeclarator{ name, init });
        }
        Ok(Box::new(VariableDeclaration{ kind, declarations }))
    }
}

impl Interpretable for VariableDeclaration {
    fn interpret(&self, state: &mut RuntimeState) -> Result<JSValue, Exception> {
        for decl in self.declarations.iter() {
            state.declare_identifier(decl.name.clone())?;
            if let Some(init) = &decl.init {
                let value = init.interpret(state)?;
                state.set_identifier(&decl.name, value)?;
            }
        }
        Ok(JSValue(JSON::Null))
    }
}

// ==============================================

struct RuntimeState {
    variables: HashMap<String, JSValue>,
}

impl RuntimeState {
    fn declare_identifier(&mut self, name: String) -> Result<(), Exception> {
        let undefined = JSValue(JSON::Null);    // not quite, but works for now
        self.variables.entry(name).or_insert(undefined);
        Ok(())
    }

    fn set_identifier(&mut self, name: &str, value: JSValue) -> Result<(), Exception> {
        let entry = self.variables.get_mut(name)
            .ok_or(Exception::ReferenceError(name.to_string()))?;
        *entry = value;
        Ok(())
    }

    fn lookup_identifier(&self, name: &str) -> Result<JSValue, Exception> {
        match self.variables.get(name) {
            None => Err(Exception::ReferenceError(name.to_string())),
            Some(value) => Ok(value.clone()),
        }
    }
}

// ==============================================

fn die<E: Debug>(msg: &str, err: E, exitcode: i32) -> ! {
    let msg = msg.to_string();
    eprintln!("{}: {:?}", msg, err);
    std::process::exit(exitcode);
}

fn main() {
    let stdin = io::stdin();
    let stdin = stdin.lock();

    // thanks to https://stackoverflow.com/a/55797976/621719
    let deserializer= serde_json::Deserializer::from_reader(stdin);
    for json in deserializer.into_iter::<JSON>() {
        let json = json
            .unwrap_or_else(|e| die("JSON error", e, 1));

        let ast = Program::from_json(&json)
            .unwrap_or_else(|e| die("Parse error", e, 2));

        let result = ast.interpret()
            .unwrap_or_else(|e| die("Interpretation error", e, 3));

        // TODO: JSON output, optionally
        let output = serde_json::to_string(&result.0)
            .unwrap_or_else(|e| die("Result serialization error", e, 4));

        println!("{}", output);
    }
}
