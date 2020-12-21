use std::convert::TryFrom;

use crate::ast::*;  // yes, EVERYTHING.
use crate::error::ParseError;
use crate::value::{JSON, JSValue, UNDEFINED};


/*
 *  JSON helpers
 */

fn json_get<'a>(
    json: &'a JSON,
    property: &'static str
) -> Result<&'a JSON, ParseError<JSON>> {
    json.get(property)
        .ok_or(ParseError::ObjectWithout{ attr: property, value: json.clone() })
}

fn json_get_bool<'a>(
    json: &'a JSON,
    property: &'static str
) -> Result<bool, ParseError<JSON>> {
    let jbool = json_get(json, property)?;
    jbool.as_bool().ok_or(ParseError::ShouldBeBool{ value: jbool.clone() })
}

fn json_get_str<'a>(
    json: &'a JSON,
    property: &'static str
) -> Result<&'a str, ParseError<JSON>> {
    let jstr = json_get(json, property)?;
    jstr.as_str().ok_or(ParseError::ShouldBeString{ value: jstr.clone() })
}

fn json_get_array<'a>(
    json: &'a JSON,
    property: &'static str
) -> Result<&'a Vec<JSON>, ParseError<JSON>> {
    let jarray = json_get(json, property)?;
    jarray.as_array().ok_or(ParseError::ShouldBeArray{ value: jarray.clone() })
}

fn json_expect_str<'a>(
    json: &'a JSON,
    property: &'static str,
    value: &'static str
) -> Result<(), ParseError<JSON>> {
    let jstr = json_get(json, property)?;
    let got = jstr.as_str().ok_or(ParseError::ShouldBeString{ value: jstr.clone() })?;
    if got == value {
        Ok(())
    } else {
        Err(ParseError::UnexpectedValue{ want: value, value: jstr.clone(), })
    }
}

impl TryFrom<&JSON> for Program {
    type Error = ParseError<JSON>;

    fn try_from(json: &JSON) -> Result<Self, Self::Error> {
        json_expect_str(json, "type", "Program")?;

        let jbody = json_get_array(json, "body")?;

        let mut body = vec![];
        for jstmt in jbody.iter() {
            let stmt = Statement::try_from(jstmt)?;
            body.push(stmt);
        }
        Ok(Program{ body })
    }
}

impl TryFrom<&JSON> for Statement {
    type Error = ParseError<JSON>;

    fn try_from(json: &JSON) -> Result<Self, Self::Error> {
        let typ = json_get_str(json, "type")?;
        match typ {
            "EmptyStatement" =>
                Ok(Statement::Empty),
            "ExpressionStatement" => Ok(Statement::Expression(
                ExpressionStatement::try_from(json)?
            )),
            "BlockStatement" => Ok(Statement::Block(
                BlockStatement::try_from(json)?
            )),
            "VariableDeclaration" => Ok(Statement::VariableDeclaration(
                VariableDeclaration::try_from(json)?
            )),
            _ => Err(ParseError::UnknownType{ value: json.clone() }),
        }
    }
}

impl TryFrom<&JSON> for BlockStatement {
    type Error = ParseError<JSON>;

    fn try_from(json: &JSON) -> Result<Self, Self::Error> {
        let jbody = json_get_array(json, "body")?;
        let mut body = vec![];
        for jstmt in jbody.iter() {
            let stmt = Statement::try_from(jstmt)?;
            body.push(stmt);
        }
        Ok(BlockStatement{ body })
    }
}

impl TryFrom<&JSON> for Expr {
    type Error = ParseError<JSON>;

    fn try_from(jexpr: &JSON) -> Result<Self, Self::Error> {
        let jexpr_ty = json_get_str(jexpr, "type")?;

        let expr = match jexpr_ty {
            "ArrayExpression" => {
                let jelements = json_get_array(jexpr, "elements")?;
                let elements = jelements.iter()
                    .map(|j| Expr::try_from(j).map(|e| Box::new(e)))
                    .collect::<Result<Vec<Box<Expr>>, ParseError<JSON>>>();
                Expr::Array(elements?)
            }
            "AssignmentExpression" => {
                let jright = json_get(jexpr, "right")?;
                let right = Expr::try_from(jright)?;

                let jleft = json_get(jexpr, "left")?;
                let left = Expr::try_from(jleft)?;

                let jop = json_get_str(jexpr, "operator")?;
                let op = match jop {
                    "=" => AssignOp::Equal,
                    _ => return Err(ParseError::UnexpectedValue{
                        want: "=",
                        value: jexpr.get("operator").unwrap().clone()
                    }),
                };
                Expr::Assign(Box::new(left), op, Box::new(right))
            }
            "BinaryExpression" => {
                let jleft = json_get(jexpr, "left")?;
                let left = Expr::try_from(jleft)?;

                let jright = json_get(jexpr, "right")?;
                let right = Expr::try_from(jright)?;

                let opstr = json_get_str(jexpr, "operator")?;
                let op = match opstr {
                    "+" => BinOp::Plus,
                    "==" => BinOp::EqEq,
                    _ => return Err(ParseError::UnexpectedValue{
                        want: "+|==",
                        value: jexpr.get("operator").unwrap().clone(),
                    }),
                };
                Expr::BinaryOp(Box::new(left), op, Box::new(right))
            }
            "CallExpression" => {
                let jcallee = json_get(jexpr, "callee")?;
                let callee = Expr::try_from(jcallee)?;

                let jarguments = json_get_array(jexpr, "arguments")?;
                let arguments = jarguments.iter()
                    .map(|j| Expr::try_from(j).map(|e| Box::new(e)))
                    .collect::<Result<Vec<Box<Expr>>, ParseError<JSON>>>();

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
                let object = Expr::try_from(jobject)?;

                let jproperty = json_get(jexpr, "property")?;
                let property = Expr::try_from(jproperty)?;
                Expr::Member(Box::new(object), Box::new(property), computed)
            }
            "ObjectExpression" => {
                let jproperties = json_get_array(jexpr, "properties")?;

                let mut properties = vec![];
                for jprop in jproperties.iter() {
                    json_expect_str(jprop, "type", "Property")?;

                    let jkey = json_get(jprop, "key")?;
                    let keyexpr = Expr::try_from(jkey)?;
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
                                    value: jprop.clone(),
                                })
                        }
                    };

                    let jvalue = json_get(jprop, "value")?;
                    let value = Expr::try_from(jvalue)?;

                    properties.push((key, Box::new(value)));
                }
                Expr::Object(properties)
            }
            _ =>
                return Err(ParseError::UnknownType{ value: jexpr.clone() }),
        };
        Ok(expr)
    }
}

impl TryFrom<&JSON> for ExpressionStatement {
    type Error = ParseError<JSON>;

    fn try_from(value: &JSON) -> Result<Self, Self::Error> {
        json_expect_str(value, "type", "ExpressionStatement")?;

        let jexpr = json_get(value, "expression")?;
        let expression = Expr::try_from(jexpr)?;
        Ok(ExpressionStatement { expression })
    }
}


impl TryFrom<&JSON> for VariableDeclaration {
    type Error = ParseError<JSON>;

    fn try_from(value: &JSON) -> Result<Self, Self::Error> {
        json_expect_str(value, "type", "VariableDeclaration")?;

        let kind = match json_get_str(value, "kind")? {
            "const" => DeclarationKind::Const,
            "let" => DeclarationKind::Let,
            "var" => DeclarationKind::Var,
            _ => return Err(ParseError::UnexpectedValue{
                want: "const|let|var",
                value: value.get("kind").unwrap().clone(),
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
                    let expr = Expr::try_from(jinit)?;
                    Some(Box::new(expr))
                }
            };

            declarations.push(VariableDeclarator{ name, init });
        }
        Ok(VariableDeclaration{ _kind: kind, declarations })
    }
}
