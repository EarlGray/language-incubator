use std::convert::TryFrom;

use crate::ast::*; // yes, EVERYTHING.

use crate::error::ParseError;
use crate::object::JSON;

/*
 *  JSON helpers
 */

fn json_get<'a>(json: &'a JSON, property: &'static str) -> Result<&'a JSON, ParseError<JSON>> {
    json.get(property).ok_or(ParseError::ObjectWithout {
        attr: property,
        value: json.clone(),
    })
}

fn json_get_bool<'a>(json: &'a JSON, property: &'static str) -> Result<bool, ParseError<JSON>> {
    let jbool = json_get(json, property)?;
    jbool.as_bool().ok_or(ParseError::ShouldBeBool {
        value: jbool.clone(),
    })
}

fn json_get_str<'a>(json: &'a JSON, property: &'static str) -> Result<&'a str, ParseError<JSON>> {
    let jstr = json_get(json, property)?;
    jstr.as_str().ok_or(ParseError::ShouldBeString {
        value: jstr.clone(),
    })
}

fn json_get_array<'a>(
    json: &'a JSON,
    property: &'static str,
) -> Result<&'a Vec<JSON>, ParseError<JSON>> {
    let jarray = json_get(json, property)?;
    jarray.as_array().ok_or(ParseError::ShouldBeArray {
        value: jarray.clone(),
    })
}

/// maps `null` into None, an object into Some(T) using a closure.
fn json_map_object<'a, T, F>(json: &'a JSON, action: F) -> Result<Option<T>, ParseError<JSON>>
where
    F: Fn(&JSON) -> Result<T, ParseError<JSON>>,
{
    match json {
        JSON::Null => Ok(None),
        JSON::Object(_) => {
            let value = action(json)?;
            Ok(Some(value))
        }
        _ => {
            let want = "null | object";
            let value = json.clone();
            Err(ParseError::UnexpectedValue { want, value })
        }
    }
}

fn json_expect_str<'a>(
    json: &'a JSON,
    property: &'static str,
    value: &'static str,
) -> Result<(), ParseError<JSON>> {
    let jstr = json_get(json, property)?;
    let got = jstr.as_str().ok_or(ParseError::ShouldBeString {
        value: jstr.clone(),
    })?;
    if got == value {
        Ok(())
    } else {
        Err(ParseError::UnexpectedValue {
            want: value,
            value: jstr.clone(),
        })
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
        Ok(Program { body })
    }
}

impl TryFrom<&JSON> for Statement {
    type Error = ParseError<JSON>;

    fn try_from(json: &JSON) -> Result<Self, Self::Error> {
        let typ = json_get_str(json, "type")?;
        match typ {
            "BlockStatement" => {
                let stmt = BlockStatement::try_from(json)?;
                Ok(Statement::Block(stmt))
            }
            "BreakStatement" => {
                let stmt = BreakStatement::try_from(json)?;
                Ok(Statement::Break(stmt))
            }
            "ContinueStatement" => {
                let stmt = ContinueStatement::try_from(json)?;
                Ok(Statement::Continue(stmt))
            }
            "DoWhileStatement" => {
                let mut stmt = ForStatement::try_from(json)?;
                stmt.init = stmt.body.clone();
                Ok(Statement::For(Box::new(stmt)))
            }
            "EmptyStatement" => Ok(Statement::Empty),
            "ExpressionStatement" => {
                let stmt = ExpressionStatement::try_from(json)?;
                Ok(Statement::Expr(stmt))
            }
            "ForStatement" | "WhileStatement" => {
                let stmt = ForStatement::try_from(json)?;
                Ok(Statement::For(Box::new(stmt)))
            }
            "ForInStatement" => {
                let stmt = ForInStatement::try_from(json)?;
                Ok(Statement::ForIn(stmt))
            }
            "FunctionDeclaration" => {
                let decl = FunctionDeclaration::try_from(json)?;
                Ok(Statement::FunctionDeclaration(decl))
            }
            "IfStatement" => {
                let stmt = IfStatement::try_from(json)?;
                Ok(Statement::If(Box::new(stmt)))
            }
            "LabeledStatement" => {
                let stmt = LabelStatement::try_from(json)?;
                Ok(Statement::Label(stmt))
            }
            "ReturnStatement" => {
                let stmt = ReturnStatement::try_from(json)?;
                Ok(Statement::Return(stmt))
            }
            "ThrowStatement" => {
                let stmt = ThrowStatement::try_from(json)?;
                Ok(Statement::Throw(stmt))
            }
            "TryStatement" => {
                let stmt = TryStatement::try_from(json)?;
                Ok(Statement::Try(stmt))
            }
            "VariableDeclaration" => {
                let decl = VariableDeclaration::try_from(json)?;
                Ok(Statement::VariableDeclaration(decl))
            }
            _ => Err(ParseError::UnknownType {
                value: json.clone(),
            }),
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
        Ok(BlockStatement { body })
    }
}

impl TryFrom<&JSON> for IfStatement {
    type Error = ParseError<JSON>;

    fn try_from(value: &JSON) -> Result<Self, Self::Error> {
        json_expect_str(value, "type", "IfStatement")?;

        let jtest = json_get(value, "test")?;
        let test = Expr::try_from(jtest)?;

        let jthen = json_get(value, "consequent")?;
        let consequent = Statement::try_from(jthen)?;

        let alternate = value
            .get("alternate")
            .and_then(|jelse| Statement::try_from(jelse).ok());

        Ok(IfStatement {
            test,
            consequent,
            alternate,
        })
    }
}

impl TryFrom<&JSON> for ForStatement {
    type Error = ParseError<JSON>;

    fn try_from(json: &JSON) -> Result<Self, Self::Error> {
        let for_ok = json_expect_str(json, "type", "ForStatement");
        let while_ok = json_expect_str(json, "type", "WhileStatement");
        let dowhile_ok = json_expect_str(json, "type", "DoWhileStatement");
        while_ok.or(for_ok).or(dowhile_ok)?;

        let init = match json.get("init") {
            Some(jinit) if !jinit.is_null() => Statement::try_from(jinit)?,
            _ => Statement::Empty,
        };
        let test = match json.get("test") {
            Some(jtest) if !jtest.is_null() => Some(Expr::try_from(jtest)?),
            _ => None,
        };
        let update = match json.get("update") {
            Some(jupdate) if !jupdate.is_null() => Some(Expr::try_from(jupdate)?),
            _ => None,
        };
        let jbody = json_get(json, "body")?;
        let body = Statement::try_from(jbody)?;
        Ok(ForStatement {
            init,
            test,
            update,
            body,
        })
    }
}

impl TryFrom<&JSON> for ForInStatement {
    type Error = ParseError<JSON>;

    fn try_from(json: &JSON) -> Result<Self, Self::Error> {
        json_expect_str(json, "type", "ForInStatement")?;

        let jleft = json_get(json, "left")?;
        let left = if let Ok(vardecl) = VariableDeclaration::try_from(jleft) {
            ForInTarget::Var(vardecl)
        } else if let Ok(expr) = Expr::try_from(jleft) {
            ForInTarget::Expr(expr)
        } else {
            return Err(ParseError::UnexpectedValue {
                want: "VariableDeclaration | Pattern",
                value: jleft.clone(),
            });
        };

        let jright = json_get(json, "right")?;
        let right = Expr::try_from(jright)?;

        let jbody = json_get(json, "body")?;
        let body = Statement::try_from(jbody)?;

        Ok(ForInStatement {
            left,
            right,
            body: Box::new(body),
        })
    }
}

impl TryFrom<&JSON> for BreakStatement {
    type Error = ParseError<JSON>;

    fn try_from(value: &JSON) -> Result<Self, Self::Error> {
        json_expect_str(value, "type", "BreakStatement")?;

        let label = match value.get("label") {
            Some(JSON::Null) => None,
            Some(jlabel) => Some(Identifier::try_from(jlabel)?),
            _ => None,
        };

        Ok(BreakStatement(label))
    }
}

impl TryFrom<&JSON> for ContinueStatement {
    type Error = ParseError<JSON>;

    fn try_from(value: &JSON) -> Result<Self, Self::Error> {
        json_expect_str(value, "type", "ContinueStatement")?;

        let label = match value.get("label") {
            Some(JSON::Null) => None,
            Some(jlabel) => Some(Identifier::try_from(jlabel)?),
            _ => None,
        };

        Ok(ContinueStatement(label))
    }
}

impl TryFrom<&JSON> for LabelStatement {
    type Error = ParseError<JSON>;

    fn try_from(value: &JSON) -> Result<Self, Self::Error> {
        json_expect_str(value, "type", "LabeledStatement")?;

        let jlabel = json_get(value, "label")?;
        let label = Identifier::try_from(jlabel)?;

        let jbody = json_get(value, "body")?;
        let body = Statement::try_from(jbody)?;

        Ok(LabelStatement(label, Box::new(body)))
    }
}

impl TryFrom<&JSON> for ReturnStatement {
    type Error = ParseError<JSON>;

    fn try_from(value: &JSON) -> Result<Self, Self::Error> {
        json_expect_str(value, "type", "ReturnStatement")?;

        let jargument = json_get(value, "argument")?;
        let argument = json_map_object(jargument, |jobject| Expr::try_from(jobject))?;

        Ok(ReturnStatement(argument))
    }
}

impl TryFrom<&JSON> for ThrowStatement {
    type Error = ParseError<JSON>;

    fn try_from(value: &JSON) -> Result<Self, Self::Error> {
        json_expect_str(value, "type", "ThrowStatement")?;

        let jargument = json_get(value, "argument")?;
        let argument = Expr::try_from(jargument)?;
        Ok(ThrowStatement(argument))
    }
}

impl TryFrom<&JSON> for TryStatement {
    type Error = ParseError<JSON>;

    fn try_from(jstmt: &JSON) -> Result<Self, Self::Error> {
        json_expect_str(jstmt, "type", "TryStatement")?;

        let jblock = json_get(jstmt, "block")?;
        let block = BlockStatement::try_from(jblock)?;

        let jhandler = json_get(jstmt, "handler")?;
        let handler = json_map_object(jhandler, |jobject| {
            let jparam = json_get(jobject, "param")?;
            let param = Identifier::try_from(jparam)?;

            let jbody = json_get(jobject, "body")?;
            let body = BlockStatement::try_from(jbody)?;

            Ok(CatchClause { param, body })
        })?;

        let jfinalizer = json_get(jstmt, "finalizer")?;
        let finalizer = json_map_object(jfinalizer, |jobject| BlockStatement::try_from(jobject))?;

        Ok(TryStatement {
            block,
            handler,
            finalizer,
        })
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
            _ => {
                return Err(ParseError::UnexpectedValue {
                    want: "const|let|var",
                    value: value.get("kind").unwrap().clone(),
                })
            }
        };
        let jdeclarations = json_get_array(value, "declarations")?;

        let mut declarations = vec![];
        for decl in jdeclarations.iter() {
            json_expect_str(decl, "type", "VariableDeclarator")?;

            let jid = json_get(decl, "id")?;
            let name = Identifier::try_from(jid)?;

            let jinit = json_get(decl, "init")?;
            let init = match jinit {
                JSON::Null => None,
                _ => {
                    let expr = Expr::try_from(jinit)?;
                    Some(Box::new(expr))
                }
            };

            declarations.push(VariableDeclarator { name, init });
        }
        Ok(VariableDeclaration { kind, declarations })
    }
}

impl TryFrom<&JSON> for FunctionDeclaration {
    type Error = ParseError<JSON>;

    fn try_from(value: &JSON) -> Result<Self, Self::Error> {
        json_expect_str(value, "type", "FunctionDeclaration")?;

        let function = FunctionExpression::try_from(value)?;
        let id = function.id.clone().ok_or(ParseError::ObjectWithout {
            attr: "id",
            value: value.clone(),
        })?;
        Ok(FunctionDeclaration { id, function })
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

impl TryFrom<&JSON> for Expr {
    type Error = ParseError<JSON>;

    fn try_from(jexpr: &JSON) -> Result<Self, Self::Error> {
        let jexpr_ty = json_get_str(jexpr, "type")?;

        let expr = match jexpr_ty {
            "ArrayExpression" => {
                let jelements = json_get_array(jexpr, "elements")?;
                let elements = jelements
                    .iter()
                    .map(|j| Expr::try_from(j).map(|e| Box::new(e)))
                    .collect::<Result<Vec<Box<Expr>>, ParseError<JSON>>>();
                let expr = ArrayExpression(elements?);
                Expr::Array(expr)
            }
            "AssignmentExpression" => {
                let expr = AssignmentExpression::try_from(jexpr)?;
                Expr::Assign(expr)
            }
            "BinaryExpression" => {
                let expr = BinaryExpression::try_from(jexpr)?;
                Expr::BinaryOp(expr)
            }
            "CallExpression" => {
                let jcallee = json_get(jexpr, "callee")?;
                let callee = Expr::try_from(jcallee)?;

                let jarguments = json_get_array(jexpr, "arguments")?;
                let arguments = jarguments
                    .iter()
                    .map(|j| Expr::try_from(j).map(|e| Box::new(e)))
                    .collect::<Result<Vec<Box<Expr>>, ParseError<JSON>>>();

                Expr::Call(CallExpression(Box::new(callee), arguments?))
            }
            "ConditionalExpression" => {
                let jtest = json_get(jexpr, "test")?;
                let condexpr = Expr::try_from(jtest)?;

                let jthen = json_get(jexpr, "consequent")?;
                let thenexpr = Expr::try_from(jthen)?;

                let jelse = json_get(jexpr, "alternate")?;
                let elseexpr = Expr::try_from(jelse)?;

                let expr = ConditionalExpression(
                    Box::new(condexpr),
                    Box::new(thenexpr),
                    Box::new(elseexpr),
                );
                Expr::Conditional(expr)
            }
            "FunctionExpression" => {
                let expr = FunctionExpression::try_from(jexpr)?;
                Expr::Function(expr)
            }
            "Identifier" => {
                let expr = Identifier::try_from(jexpr)?;
                Expr::Identifier(expr)
            }
            "Literal" => {
                let jval = json_get(jexpr, "value")?;
                let expr = Literal(jval.clone());
                Expr::Literal(expr)
            }
            "LogicalExpression" => {
                let expr = LogicalExpression::try_from(jexpr)?;
                Expr::LogicalOp(expr)
            }
            "MemberExpression" => {
                let computed = json_get_bool(jexpr, "computed")?;

                let jobject = json_get(jexpr, "object")?;
                let object = Expr::try_from(jobject)?;

                let jproperty = json_get(jexpr, "property")?;
                let property = Expr::try_from(jproperty)?;
                let expr = MemberExpression(Box::new(object), Box::new(property), computed);
                Expr::Member(expr)
            }
            "NewExpression" => {
                let jcallee = json_get(jexpr, "callee")?;
                let callee = Expr::try_from(jcallee)?;

                let jarguments = json_get_array(jexpr, "arguments")?;
                let arguments = jarguments
                    .iter()
                    .map(|j| Expr::try_from(j).map(|e| Box::new(e)))
                    .collect::<Result<Vec<Box<Expr>>, ParseError<JSON>>>()?;

                let expr = NewExpression(Box::new(callee), arguments);
                Expr::New(expr)
            }
            "ObjectExpression" => {
                let expr = ObjectExpression::try_from(jexpr)?;
                Expr::Object(expr)
            }
            "ThisExpression" => Expr::This,
            "UnaryExpression" => {
                let expr = UnaryExpression::try_from(jexpr)?;
                Expr::Unary(expr)
            }
            "UpdateExpression" => {
                let expr = UpdateExpression::try_from(jexpr)?;
                Expr::Update(expr)
            }
            _ => {
                return Err(ParseError::UnknownType {
                    value: jexpr.clone(),
                })
            }
        };
        Ok(expr)
    }
}

impl TryFrom<&JSON> for Identifier {
    type Error = ParseError<JSON>;

    fn try_from(jexpr: &JSON) -> Result<Self, Self::Error> {
        let name = json_get_str(jexpr, "name")?;
        Ok(Identifier(name.to_string()))
    }
}

impl TryFrom<&JSON> for UnaryExpression {
    type Error = ParseError<JSON>;

    fn try_from(jexpr: &JSON) -> Result<Self, Self::Error> {
        let jop = json_get_str(jexpr, "operator")?;
        let op = match jop {
            "+" => UnOp::Plus,
            "-" => UnOp::Minus,
            "!" => UnOp::Exclamation,
            "~" => UnOp::Tilde,
            "delete" => UnOp::Delete,
            "typeof" => UnOp::Typeof,
            "void" => UnOp::Void,
            _ => {
                return Err(ParseError::UnexpectedValue {
                    want: "+ | - | ! | ~ | typeof | void",
                    value: jexpr.clone(),
                })
            }
        };

        let jargument = json_get(jexpr, "argument")?;
        let argument = Expr::try_from(jargument)?;

        Ok(UnaryExpression(op, Box::new(argument)))
    }
}

impl TryFrom<&JSON> for UpdateExpression {
    type Error = ParseError<JSON>;

    fn try_from(jexpr: &JSON) -> Result<Self, Self::Error> {
        let jargument = json_get(jexpr, "argument")?;
        let argument = Expr::try_from(jargument)?;

        let prefix = json_get_bool(jexpr, "prefix")?;
        let operator = json_get_str(jexpr, "operator")?;

        let op = match operator {
            "++" => UpdOp::Increment,
            "--" => UpdOp::Decrement,
            _ => {
                return Err(ParseError::UnexpectedValue {
                    want: "++ or --",
                    value: jexpr.clone(),
                })
            }
        };
        Ok(UpdateExpression(op, prefix, Box::new(argument)))
    }
}

impl TryFrom<&JSON> for BinaryExpression {
    type Error = ParseError<JSON>;

    fn try_from(jexpr: &JSON) -> Result<Self, Self::Error> {
        let jleft = json_get(jexpr, "left")?;
        let left = Expr::try_from(jleft)?;

        let jright = json_get(jexpr, "right")?;
        let right = Expr::try_from(jright)?;

        let opstr = json_get_str(jexpr, "operator")?;
        let op = match opstr {
            "+" => BinOp::Plus,
            "-" => BinOp::Minus,
            "*" => BinOp::Star,
            "==" => BinOp::EqEq,
            "===" => BinOp::EqEqEq,
            "!=" => BinOp::NotEq,
            "!==" => BinOp::NotEqEq,
            "<" => BinOp::Less,
            ">" => BinOp::Greater,
            "<=" => BinOp::LtEq,
            ">=" => BinOp::GtEq,
            "instanceof" => BinOp::InstanceOf,
            _ => {
                return Err(ParseError::UnexpectedValue {
                    want: "+|-|*|==|===|!=|<|>|<=|>=|instanceof",
                    value: jexpr.get("operator").unwrap().clone(),
                })
            }
        };
        Ok(BinaryExpression(Box::new(left), op, Box::new(right)))
    }
}

impl TryFrom<&JSON> for LogicalExpression {
    type Error = ParseError<JSON>;

    fn try_from(jexpr: &JSON) -> Result<Self, Self::Error> {
        let jleft = json_get(jexpr, "left")?;
        let left = Expr::try_from(jleft)?;

        let jright = json_get(jexpr, "right")?;
        let right = Expr::try_from(jright)?;

        let opstr = json_get_str(jexpr, "operator")?;
        let op = match opstr {
            "&&" => BoolOp::And,
            "||" => BoolOp::Or,
            _ => {
                return Err(ParseError::UnexpectedValue {
                    want: "&& or ||",
                    value: jexpr.get("operator").unwrap().clone(),
                })
            }
        };
        Ok(LogicalExpression(Box::new(left), op, Box::new(right)))
    }
}

impl TryFrom<&JSON> for AssignmentExpression {
    type Error = ParseError<JSON>;

    fn try_from(jexpr: &JSON) -> Result<Self, Self::Error> {
        let jright = json_get(jexpr, "right")?;
        let right = Expr::try_from(jright)?;

        let jleft = json_get(jexpr, "left")?;
        let left = Expr::try_from(jleft)?;

        let jop = json_get_str(jexpr, "operator")?;
        let modop = match jop {
            "=" => None,
            "+=" => Some(BinOp::Plus),
            "-=" => Some(BinOp::Minus),
            "*=" => Some(BinOp::Star),
            _ => {
                return Err(ParseError::UnexpectedValue {
                    want: "= | +=",
                    value: jexpr.get("operator").unwrap().clone(),
                })
            }
        };
        Ok(AssignmentExpression(
            Box::new(left),
            AssignOp(modop),
            Box::new(right),
        ))
    }
}

impl TryFrom<&JSON> for ObjectExpression {
    type Error = ParseError<JSON>;

    fn try_from(jexpr: &JSON) -> Result<Self, Self::Error> {
        json_expect_str(jexpr, "type", "ObjectExpression")?;

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
                    Expr::Identifier(ident) => ObjectKey::Identifier(ident.0),
                    Expr::Literal(jval) => match jval.0.as_str() {
                        Some(val) => ObjectKey::Identifier(val.to_string()),
                        None => ObjectKey::Identifier(jval.0.to_string()),
                    },
                    _ => {
                        return Err(ParseError::UnexpectedValue {
                            want: "Identifier|Literal",
                            value: jprop.clone(),
                        })
                    }
                }
            };

            let jvalue = json_get(jprop, "value")?;
            let value = Expr::try_from(jvalue)?;

            properties.push((key, Box::new(value)));
        }
        Ok(ObjectExpression(properties))
    }
}

impl TryFrom<&JSON> for FunctionExpression {
    type Error = ParseError<JSON>;

    fn try_from(jexpr: &JSON) -> Result<Self, Self::Error> {
        let id: Option<Identifier> = json_get(jexpr, "id")
            .and_then(|jid| Identifier::try_from(jid))
            .ok();

        let jparams = json_get_array(jexpr, "params")?;
        let params = jparams
            .into_iter()
            .map(|jparam| Identifier::try_from(jparam))
            .collect::<Result<Vec<_>, ParseError<JSON>>>()?;

        let jbody = json_get(jexpr, "body")?;
        let body = BlockStatement::try_from(jbody)?;

        Ok(FunctionExpression {
            id,
            params,
            body: Box::new(body),
            is_generator: json_get_bool(jexpr, "generator").unwrap_or(false),
            is_expression: json_get_bool(jexpr, "expression").unwrap_or(false),
            is_async: json_get_bool(jexpr, "async").unwrap_or(false),
        })
    }
}
