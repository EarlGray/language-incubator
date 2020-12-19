use std::io;
use std::fmt::Debug;
use std::collections::HashMap;

use serde_json::Value as JSON;
use serde_json::json;

#[cfg(test)]
mod test;


#[derive(Debug)]
enum ParseError<'a> {
    ShouldBeBool{ value: &'a JSON },
    ShouldBeString{ value: &'a JSON },
    ShouldBeArray{ value: &'a JSON },
    //ShouldBeObject{ value: &'a JSON },
    ObjectWithout{ attr: &'static str, value: &'a JSON},
    UnexpectedValue{ want: &'static str, value: &'a JSON },
    UnknownType{ ty: &'a str, value: &'a JSON },
}

#[derive(Debug)]
enum Exception {
    SyntaxError,
    ReferenceError(String),
}


/*
 *  JSValue: "type" "system"
 */

#[derive(Debug, Clone, PartialEq)]
struct JSValue(JSON);

const UNDEFINED: JSValue = JSValue(JSON::Null);


type JSNumber = f64;

/*
#[derive(Debug, Clone)]
enum JSValue {
    Null,
    Undefined,
    Number(JSNumber),
    Str(String),
}
*/

impl JSValue {
    /// to_string() makes a string representation of the value
    /// ```
    /// JSValue::from("1").to_string()    // "\"1\""
    /// JSValue::from(1).to_string()      // "1"
    /// JSValue::from(json!([1, 2])).to_string()   // "[1,2]"
    /// ```
    fn to_string(&self) -> String {
        match &self.0 {
            JSON::Null => "null".to_string(),
            JSON::Number(n) => n.to_string(),
            JSON::Bool(b) => b.to_string(),
            JSON::String(_s) => {
                let mut s = String::new();
                s.push_str(&self.0.to_string());
                s
            }
            JSON::Array(a) => {
                let body = a.iter()
                    .map(|v| JSValue(v.clone()).to_string())
                    .collect::<Vec<String>>()
                    .join(",");

                let mut s = String::new();
                s.push_str("[");
                s.push_str(&body);
                s.push_str("]");
                s
            }
            JSON::Object(obj) => {
                let mut s = String::new();
                let mut empty = true;
                s.push('{');
                for (key, jval) in obj.iter() {
                    s.push(' ');
                    if is_valid_identifier(&key) {
                        s.push_str(key);
                    } else {
                        let skey = JSValue(json!(key)).to_string();
                        s.push_str(&skey);
                    }
                    s.push_str(": ");
                    let val = JSValue(jval.clone()).to_string();
                    s.push_str(&val);
                    s.push(',');
                    empty = false;
                }
                if !empty { s.pop(); s.push(' '); }
                s.push('}');
                s
            }
        }
    }

    /// stringify() corresponds to .toString() in JavaScript
    fn stringify(&self) -> String {
        if let Some(s) = self.0.as_str() {
            return s.to_string();
        }
        if let Some(a) = self.0.as_array() {
            return a.iter()
                    .map(|v| JSValue(v.clone()).to_string())
                    .collect::<Vec<String>>()
                    .join(",");
        }
        if self.0.is_object() {
            return "[object Object]".to_string();
        }
        return self.to_string();
    }

    fn numberify(&self) -> Option<JSNumber> {
        if self.0.is_null() {
            Some(0.0)
        } else if let Some(b) = self.0.as_bool() {
            Some(if b { 1.0 } else { 0.0 })
        } else if let Some(n) = self.0.as_f64() {
            Some(n)
        } else if let Some(s) = self.0.as_str() {
            s.parse::<JSNumber>().ok()
        } else {
            None
        }
    }

    /*
    fn from_json(json: &JSON) -> JSValue {
        match json {
        }
    }


    fn as_number(&self) -> Option<JSNumber> {
    }
    */
}

#[test]
fn test_numberify() {
}

#[test]
fn test_boolify() {
}

impl From<JSON> for JSValue {
    fn from(json: JSON) -> Self { JSValue(json) }
}

impl From<JSNumber> for JSValue {
    fn from(number: JSNumber) -> Self { JSValue(json!(number)) }
}

impl From<i64> for JSValue {
    fn from(number: i64) -> Self { JSValue(json!(number)) }
}

impl From<String> for JSValue {
    fn from(s: String) -> Self { JSValue(json!(s)) }
}

impl From<&str> for JSValue {
    fn from(s: &str) -> Self { JSValue(json!(s.to_string())) }
}


/*
 *  JSON helpers
 */

fn is_valid_identifier(s: &str) -> bool {
    let is_start = |c: char| (c.is_alphabetic() || c == '_' || c == '$');

    let mut it = s.chars();
    if let Some(c) = it.next() {
        is_start(c) && it.all(|c| is_start(c) || c.is_numeric())
    } else {
        false
    }
}

fn json_get<'a>(json: &'a JSON, property: &'static str) -> Result<&'a JSON, ParseError<'a>> {
    json.get(property)
        .ok_or(ParseError::ObjectWithout{ attr: property, value: json })
}

fn json_get_bool<'a>(
    json: &'a JSON,
    property: &'static str
) -> Result<bool, ParseError<'a>> {
    let jbool = json_get(json, property)?;
    jbool.as_bool().ok_or(ParseError::ShouldBeBool{ value: jbool })
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


// ==============================================
enum ObjectKey {
    Computed(Expr),
    Identifier(String),
}

enum Expr {
    Literal(JSValue),
    Identifier(String),
    BinaryOp(Box<Expr>, BinOp, Box<Expr>),
    Call(Box<Expr>, Vec<Box<Expr>>),
    Array(Vec<Box<Expr>>),
    Object(Vec<(ObjectKey, Box<Expr>)>),
    Member(Box<Expr>, Box<Expr>, bool),
}

enum BinOp { Add, KindaEqual }

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
                    "==" => BinOp::KindaEqual,
                    _ => return Err(ParseError::UnexpectedValue{
                        value: jexpr.get("operator").unwrap(),
                        want: "+|=="
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
                match name {
                    "undefined" => Expr::Literal(UNDEFINED),
                    _ => Expr::Identifier(name.to_string())
                }
            }
            "Literal" => {
                let jval = json_get(jexpr, "value")?;
                Expr::Literal(JSValue::from(jval.clone()))
            }
            "MemberExpression" => {
                let computed = json_get_bool(jexpr, "computed")?;

                let jobject = json_get(jexpr, "object")?;
                let object = Expr::from_json(jobject)?;

                let jproperty = json_get(jexpr, "property")?;
                let property = Expr::from_json(jproperty)?;
                Expr::Member(Box::new(object), Box::new(property), computed)
            }
            "ObjectExpression" => {
                let jproperties = json_get_array(jexpr, "properties")?;

                let mut properties = vec![];
                for jprop in jproperties.iter() {
                    json_expect_str(jprop, "type", "Property")?;

                    let jkey = json_get(jprop, "key")?;
                    let keyexpr = Expr::from_json(jkey)?;
                    let key = if json_get_bool(jprop, "computed")? {
                        ObjectKey::Computed(keyexpr)
                    } else {
                        match keyexpr {
                            Expr::Identifier(ident) =>
                                ObjectKey::Identifier(ident),
                            Expr::Literal(jval) =>
                                match jval.0.as_str() {
                                    Some(val) => ObjectKey::Identifier(val.to_string()),
                                    None => ObjectKey::Identifier(jval.to_string()),
                                }
                            _ =>
                                return Err(ParseError::UnexpectedValue{
                                    want: "Identifier|Literal",
                                    value: jprop,
                                })
                        }
                    };

                    let jvalue = json_get(jprop, "value")?;
                    let value = Expr::from_json(jvalue)?;

                    properties.push((key, Box::new(value)));
                }
                Expr::Object(properties)
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
                        if !(lval.0.is_string() || rval.0.is_string()) {
                            if let Some(lnum) = lval.numberify() {
                                if let Some(rnum) = rval.numberify() {
                                    return Ok(JSValue::from(lnum + rnum));
                                }
                            }
                        }
                        let lvalstr = lval.stringify();
                        let rvalstr = rval.stringify();
                        return Ok(JSValue::from(lvalstr + &rvalstr));
                    }
                    BinOp::KindaEqual => {
                        Ok(JSValue::from(JSON::Bool(lval == rval)))
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
                Ok(JSValue::from(JSON::Array(arr)))
            }
            Expr::Member(objexpr, propexpr, computed) => {
                let object = objexpr.interpret(state)?;
                let property = if *computed {
                    propexpr.interpret(state)?
                } else {
                    match &**propexpr {
                        Expr::Identifier(name) => JSValue::from(name.clone()),
                        _ => panic!("Member(computed=false) property is not an identifier")
                    }
                };
                if let Some(arr) = object.0.as_array() {
                    match property.0.as_i64() {
                        Some(index) => {
                            let j = arr.get(index as usize).unwrap_or(&UNDEFINED.0).clone();
                            Ok(JSValue::from(j))
                        }
                        None => Ok(UNDEFINED),
                    }
                } else if let Some(obj) = object.0.as_object() {
                    let prop = property.stringify();
                    let j = obj.get(&prop).unwrap_or(&UNDEFINED.0).clone();
                    Ok(JSValue::from(j))
                } else {
                    Ok(UNDEFINED)
                }
            }
            Expr::Object(properties) => {
                let mut object = json!({});
                for (key, valexpr) in properties.iter() {
                    let keyname = match key {
                        ObjectKey::Identifier(ident) =>
                            ident.clone(),
                        ObjectKey::Computed(expr) => {
                            let result = expr.interpret(state)?;
                            result.stringify()
                        }
                    };
                    let value = valexpr.interpret(state)?;
                    object[keyname] = value.0;
                }
                Ok(JSValue::from(object))
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
    _kind: DeclarationKind,
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
        Ok(Box::new(VariableDeclaration{ _kind: kind, declarations }))
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
        Ok(UNDEFINED)
    }
}

// ==============================================

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

        let mut result = UNDEFINED;
        for stmt in self.body.iter() {
            result = stmt.interpret(&mut runtime)?;
        }
        Ok(result)
    }
}

// ==============================================

struct RuntimeState {
    variables: HashMap<String, JSValue>,
}

impl RuntimeState {
    fn declare_identifier(&mut self, name: String) -> Result<(), Exception> {
        self.variables.entry(name).or_insert(UNDEFINED);
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
        println!("{}", result.to_string());
    }
}
