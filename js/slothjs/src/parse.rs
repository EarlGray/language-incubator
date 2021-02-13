use std::collections::HashSet;

use crate::ast::*; // yes, EVERYTHING.

use crate::error::ParseError;
use crate::object::JSON;

#[derive(Debug)]
pub struct ParserContext {
    pub used_variables: HashSet<Identifier>,
    pub declared_variables: HashSet<Identifier>,
}

impl ParserContext {
    pub fn new() -> ParserContext {
        ParserContext {
            used_variables: HashSet::new(),
            declared_variables: HashSet::new(),
        }
    }
}

trait ParseFrom<T> {
    type Error;

    fn parse_from(source: &T, ctx: &mut ParserContext) -> Result<Self, Self::Error>
    where
        Self: Sized;
}

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
fn json_map_object<'a, T, F>(json: &'a JSON, mut action: F) -> Result<Option<T>, ParseError<JSON>>
where
    F: FnMut(&JSON) -> Result<T, ParseError<JSON>>,
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

impl Program {
    pub fn parse_from(json: &JSON) -> Result<Program, ParseError<JSON>> {
        json_expect_str(json, "type", "Program")?;

        let mut ctx = ParserContext::new();
        let jbody = json_get_array(json, "body")?;
        let body = (jbody.into_iter())
            .map(|jstmt| Statement::parse_from(jstmt, &mut ctx))
            .collect::<Result<Vec<Statement>, ParseError<JSON>>>()?;

        let variables = ctx.declared_variables;
        Ok(Program { body, variables })
    }
}

impl ParseFrom<JSON> for Statement {
    type Error = ParseError<JSON>;

    fn parse_from(json: &JSON, ctx: &mut ParserContext) -> Result<Self, Self::Error> {
        let typ = json_get_str(json, "type")?;
        match typ {
            "BlockStatement" => {
                let stmt = BlockStatement::parse_from(json, ctx)?;
                Ok(Statement::Block(stmt))
            }
            "BreakStatement" => {
                let stmt = BreakStatement::parse_from(json, ctx)?;
                Ok(Statement::Break(stmt))
            }
            "ContinueStatement" => {
                let stmt = ContinueStatement::parse_from(json, ctx)?;
                Ok(Statement::Continue(stmt))
            }
            "DoWhileStatement" => {
                let mut stmt = ForStatement::parse_from(json, ctx)?;
                stmt.init = stmt.body.clone();
                Ok(Statement::For(Box::new(stmt)))
            }
            "EmptyStatement" => Ok(Statement::Empty),
            "ExpressionStatement" => {
                let stmt = ExpressionStatement::parse_from(json, ctx)?;
                Ok(Statement::Expr(stmt))
            }
            "ForStatement" | "WhileStatement" => {
                let stmt = ForStatement::parse_from(json, ctx)?;
                Ok(Statement::For(Box::new(stmt)))
            }
            "ForInStatement" => {
                let stmt = ForInStatement::parse_from(json, ctx)?;
                Ok(Statement::ForIn(stmt))
            }
            "FunctionDeclaration" => {
                let decl = FunctionDeclaration::parse_from(json, ctx)?;
                Ok(Statement::FunctionDeclaration(decl))
            }
            "IfStatement" => {
                let stmt = IfStatement::parse_from(json, ctx)?;
                Ok(Statement::If(Box::new(stmt)))
            }
            "LabeledStatement" => {
                let stmt = LabelStatement::parse_from(json, ctx)?;
                Ok(Statement::Label(stmt))
            }
            "ReturnStatement" => {
                let stmt = ReturnStatement::parse_from(json, ctx)?;
                Ok(Statement::Return(stmt))
            }
            "SwitchStatement" => {
                let stmt = SwitchStatement::parse_from(json, ctx)?;
                Ok(Statement::Switch(stmt))
            }
            "ThrowStatement" => {
                let stmt = ThrowStatement::parse_from(json, ctx)?;
                Ok(Statement::Throw(stmt))
            }
            "TryStatement" => {
                let stmt = TryStatement::parse_from(json, ctx)?;
                Ok(Statement::Try(stmt))
            }
            "VariableDeclaration" => {
                let decl = VariableDeclaration::parse_from(json, ctx)?;
                Ok(Statement::VariableDeclaration(decl))
            }
            _ => Err(ParseError::UnknownType {
                value: json.clone(),
            }),
        }
    }
}

impl ParseFrom<JSON> for BlockStatement {
    type Error = ParseError<JSON>;

    fn parse_from(json: &JSON, ctx: &mut ParserContext) -> Result<Self, Self::Error> {
        let jbody = json_get_array(json, "body")?;
        let mut body = vec![];
        for jstmt in jbody.into_iter() {
            let stmt = Statement::parse_from(jstmt, ctx)?;
            body.push(stmt);
        }
        Ok(BlockStatement { body })
    }
}

impl ParseFrom<JSON> for IfStatement {
    type Error = ParseError<JSON>;

    fn parse_from(value: &JSON, ctx: &mut ParserContext) -> Result<Self, Self::Error> {
        json_expect_str(value, "type", "IfStatement")?;

        let jtest = json_get(value, "test")?;
        let test = Expr::parse_from(jtest, ctx)?;

        let jthen = json_get(value, "consequent")?;
        let consequent = Statement::parse_from(jthen, ctx)?;

        let alternate =
            (value.get("alternate")).and_then(|jelse| Statement::parse_from(jelse, ctx).ok());

        Ok(IfStatement {
            test,
            consequent,
            alternate,
        })
    }
}

impl ParseFrom<JSON> for SwitchStatement {
    type Error = ParseError<JSON>;

    fn parse_from(json: &JSON, ctx: &mut ParserContext) -> Result<Self, Self::Error> {
        json_expect_str(json, "type", "SwitchStatement")?;

        let jdiscriminant = json_get(json, "discriminant")?;
        let discriminant = Expr::parse_from(jdiscriminant, ctx)?;

        let jcases = json_get_array(json, "cases")?;
        let cases = (jcases.into_iter())
            .map(|jcase| {
                let jtest = json_get(jcase, "test")?;
                let test = json_map_object(jtest, |jtest| Expr::parse_from(jtest, ctx))?;

                let jconsequent = json_get_array(jcase, "consequent")?;
                let consequent = (jconsequent.into_iter())
                    .map(|jstmt| Statement::parse_from(jstmt, ctx))
                    .collect::<Result<Vec<Statement>, Self::Error>>()?;

                Ok(SwitchCase { test, consequent })
            })
            .collect::<Result<Vec<SwitchCase>, Self::Error>>()?;

        Ok(SwitchStatement {
            discriminant,
            cases,
        })
    }
}

impl ParseFrom<JSON> for ForStatement {
    type Error = ParseError<JSON>;

    fn parse_from(json: &JSON, ctx: &mut ParserContext) -> Result<Self, Self::Error> {
        let for_ok = json_expect_str(json, "type", "ForStatement");
        let while_ok = json_expect_str(json, "type", "WhileStatement");
        let dowhile_ok = json_expect_str(json, "type", "DoWhileStatement");
        for_ok.or(while_ok).or(dowhile_ok)?;

        let init = match json.get("init") {
            Some(jinit) => json_map_object(jinit, |jinit| {
                if let Ok(var) = VariableDeclaration::parse_from(jinit, ctx) {
                    Ok(Statement::VariableDeclaration(var))
                } else if let Ok(expr) = Expr::parse_from(jinit, ctx) {
                    Ok(Statement::Expr(ExpressionStatement { expression: expr }))
                } else {
                    panic!("jinit is not Expression | VariableDeclaration")
                }
            })?
            .unwrap_or(Statement::Empty),
            _ => Statement::Empty,
        };
        let test = match json.get("test") {
            Some(jtest) if !jtest.is_null() => Some(Expr::parse_from(jtest, ctx)?),
            _ => None,
        };
        let update = match json.get("update") {
            Some(jupdate) if !jupdate.is_null() => Some(Expr::parse_from(jupdate, ctx)?),
            _ => None,
        };
        let jbody = json_get(json, "body")?;
        let body = Statement::parse_from(jbody, ctx)?;
        Ok(ForStatement {
            init,
            test,
            update,
            body,
        })
    }
}

impl ParseFrom<JSON> for ForInStatement {
    type Error = ParseError<JSON>;

    fn parse_from(json: &JSON, ctx: &mut ParserContext) -> Result<Self, Self::Error> {
        json_expect_str(json, "type", "ForInStatement")?;

        let jleft = json_get(json, "left")?;
        let left = if let Ok(vardecl) = VariableDeclaration::parse_from(jleft, ctx) {
            ForInTarget::Var(vardecl)
        } else if let Ok(expr) = Expr::parse_from(jleft, ctx) {
            ForInTarget::Expr(expr)
        } else {
            return Err(ParseError::UnexpectedValue {
                want: "VariableDeclaration | Pattern",
                value: jleft.clone(),
            });
        };

        let jright = json_get(json, "right")?;
        let right = Expr::parse_from(jright, ctx)?;

        let jbody = json_get(json, "body")?;
        let body = Statement::parse_from(jbody, ctx)?;

        Ok(ForInStatement {
            left,
            right,
            body: Box::new(body),
        })
    }
}

impl ParseFrom<JSON> for BreakStatement {
    type Error = ParseError<JSON>;

    fn parse_from(value: &JSON, ctx: &mut ParserContext) -> Result<Self, Self::Error> {
        json_expect_str(value, "type", "BreakStatement")?;

        let label = match value.get("label") {
            Some(JSON::Null) => None,
            Some(jlabel) => Some(Identifier::parse_from(jlabel, ctx)?),
            _ => None,
        };

        Ok(BreakStatement(label))
    }
}

impl ParseFrom<JSON> for ContinueStatement {
    type Error = ParseError<JSON>;

    fn parse_from(value: &JSON, ctx: &mut ParserContext) -> Result<Self, Self::Error> {
        json_expect_str(value, "type", "ContinueStatement")?;

        let label = match value.get("label") {
            Some(JSON::Null) => None,
            Some(jlabel) => Some(Identifier::parse_from(jlabel, ctx)?),
            _ => None,
        };

        Ok(ContinueStatement(label))
    }
}

impl ParseFrom<JSON> for LabelStatement {
    type Error = ParseError<JSON>;

    fn parse_from(value: &JSON, ctx: &mut ParserContext) -> Result<Self, Self::Error> {
        json_expect_str(value, "type", "LabeledStatement")?;

        let jlabel = json_get(value, "label")?;
        let label = Identifier::parse_from(jlabel, ctx)?;

        let jbody = json_get(value, "body")?;
        let body = Statement::parse_from(jbody, ctx)?;

        Ok(LabelStatement(label, Box::new(body)))
    }
}

impl ParseFrom<JSON> for ReturnStatement {
    type Error = ParseError<JSON>;

    fn parse_from(value: &JSON, ctx: &mut ParserContext) -> Result<Self, Self::Error> {
        json_expect_str(value, "type", "ReturnStatement")?;

        let jargument = json_get(value, "argument")?;
        let argument = json_map_object(jargument, |jobject| Expr::parse_from(jobject, ctx))?;

        Ok(ReturnStatement(argument))
    }
}

impl ParseFrom<JSON> for ThrowStatement {
    type Error = ParseError<JSON>;

    fn parse_from(value: &JSON, ctx: &mut ParserContext) -> Result<Self, Self::Error> {
        json_expect_str(value, "type", "ThrowStatement")?;

        let jargument = json_get(value, "argument")?;
        let argument = Expr::parse_from(jargument, ctx)?;
        Ok(ThrowStatement(argument))
    }
}

impl ParseFrom<JSON> for TryStatement {
    type Error = ParseError<JSON>;

    fn parse_from(jstmt: &JSON, ctx: &mut ParserContext) -> Result<Self, Self::Error> {
        json_expect_str(jstmt, "type", "TryStatement")?;

        let jblock = json_get(jstmt, "block")?;
        let block = BlockStatement::parse_from(jblock, ctx)?;

        let jhandler = json_get(jstmt, "handler")?;
        let handler = json_map_object(jhandler, |jobject| {
            let jparam = json_get(jobject, "param")?;
            let param = Identifier::parse_from(jparam, ctx)?;

            let jbody = json_get(jobject, "body")?;
            let body = BlockStatement::parse_from(jbody, ctx)?;

            Ok(CatchClause { param, body })
        })?;

        let jfinalizer = json_get(jstmt, "finalizer")?;
        let finalizer = json_map_object(jfinalizer, |jobject| {
            BlockStatement::parse_from(jobject, ctx)
        })?;

        Ok(TryStatement {
            block,
            handler,
            finalizer,
        })
    }
}

impl ParseFrom<JSON> for VariableDeclaration {
    type Error = ParseError<JSON>;

    fn parse_from(value: &JSON, ctx: &mut ParserContext) -> Result<Self, Self::Error> {
        json_expect_str(value, "type", "VariableDeclaration")?;

        let kind = match json_get_str(value, "kind")? {
            "const" => todo!(), // DeclarationKind::Const,
            "let" => todo!(),   // DeclarationKind::Let,
            "var" => DeclarationKind::Var,
            _ => {
                return Err(ParseError::UnexpectedValue {
                    want: "var | let | const",
                    value: value.get("kind").unwrap().clone(),
                })
            }
        };
        let jdeclarations = json_get_array(value, "declarations")?;

        let mut declarations = vec![];
        for decl in jdeclarations.into_iter() {
            json_expect_str(decl, "type", "VariableDeclarator")?;

            let jid = json_get(decl, "id")?;
            let name = Identifier::parse_from(jid, ctx)?;

            let jinit = json_get(decl, "init")?;
            let init = match jinit {
                JSON::Null => None,
                _ => {
                    let expr = Expr::parse_from(jinit, ctx)?;
                    Some(Box::new(expr))
                }
            };

            ctx.declared_variables.insert(name.clone());
            declarations.push(VariableDeclarator { name, init });
        }
        Ok(VariableDeclaration { kind, declarations })
    }
}

impl ParseFrom<JSON> for FunctionDeclaration {
    type Error = ParseError<JSON>;

    fn parse_from(value: &JSON, ctx: &mut ParserContext) -> Result<Self, Self::Error> {
        json_expect_str(value, "type", "FunctionDeclaration")?;

        let function = FunctionExpression::parse_from(value, ctx)?;
        let id = function.id.clone().ok_or(ParseError::ObjectWithout {
            attr: "id",
            value: value.clone(),
        })?;
        ctx.declared_variables.insert(id.clone());
        Ok(FunctionDeclaration { id, function })
    }
}

impl ParseFrom<JSON> for ExpressionStatement {
    type Error = ParseError<JSON>;

    fn parse_from(value: &JSON, ctx: &mut ParserContext) -> Result<Self, Self::Error> {
        json_expect_str(value, "type", "ExpressionStatement")?;

        let jexpr = json_get(value, "expression")?;
        let expression = Expr::parse_from(jexpr, ctx)?;
        Ok(ExpressionStatement { expression })
    }
}

impl ParseFrom<JSON> for Expr {
    type Error = ParseError<JSON>;

    fn parse_from(jexpr: &JSON, ctx: &mut ParserContext) -> Result<Self, Self::Error> {
        let jexpr_ty = json_get_str(jexpr, "type")?;

        let expr = match jexpr_ty {
            "ArrayExpression" => {
                let jelements = json_get_array(jexpr, "elements")?;
                let elements = (jelements.into_iter())
                    .map(|jelem| Expr::parse_from(jelem, ctx))
                    .collect::<Result<Vec<Expr>, ParseError<JSON>>>();
                let expr = ArrayExpression(elements?);
                Expr::Array(expr)
            }
            "AssignmentExpression" => {
                let expr = AssignmentExpression::parse_from(jexpr, ctx)?;
                Expr::Assign(expr)
            }
            "BinaryExpression" => {
                let expr = BinaryExpression::parse_from(jexpr, ctx)?;
                Expr::BinaryOp(expr)
            }
            "CallExpression" => {
                let jcallee = json_get(jexpr, "callee")?;
                let callee = Expr::parse_from(jcallee, ctx)?;

                let jarguments = json_get_array(jexpr, "arguments")?;
                let arguments = (jarguments.into_iter())
                    .map(|jarg| Expr::parse_from(jarg, ctx))
                    .collect::<Result<Vec<Expr>, ParseError<JSON>>>();

                Expr::Call(CallExpression(Box::new(callee), arguments?))
            }
            "ConditionalExpression" => {
                let jtest = json_get(jexpr, "test")?;
                let condexpr = Expr::parse_from(jtest, ctx)?;

                let jthen = json_get(jexpr, "consequent")?;
                let thenexpr = Expr::parse_from(jthen, ctx)?;

                let jelse = json_get(jexpr, "alternate")?;
                let elseexpr = Expr::parse_from(jelse, ctx)?;

                let expr = ConditionalExpression(
                    Box::new(condexpr),
                    Box::new(thenexpr),
                    Box::new(elseexpr),
                );
                Expr::Conditional(expr)
            }
            "FunctionExpression" => {
                let expr = FunctionExpression::parse_from(jexpr, ctx)?;
                Expr::Function(expr)
            }
            "Identifier" => {
                let expr = Identifier::parse_from(jexpr, ctx)?;
                Expr::Identifier(expr)
            }
            "Literal" => {
                let jval = json_get(jexpr, "value")?;
                let expr = Literal(jval.clone());
                Expr::Literal(expr)
            }
            "LogicalExpression" => {
                let expr = LogicalExpression::parse_from(jexpr, ctx)?;
                Expr::LogicalOp(expr)
            }
            "MemberExpression" => {
                let computed = json_get_bool(jexpr, "computed")?;

                let jobject = json_get(jexpr, "object")?;
                let object = Expr::parse_from(jobject, ctx)?;

                let jproperty = json_get(jexpr, "property")?;
                let property = Expr::parse_from(jproperty, ctx)?;
                let expr = MemberExpression(Box::new(object), Box::new(property), computed);
                Expr::Member(expr)
            }
            "NewExpression" => {
                let jcallee = json_get(jexpr, "callee")?;
                let callee = Expr::parse_from(jcallee, ctx)?;

                let jarguments = json_get_array(jexpr, "arguments")?;
                let arguments = (jarguments.into_iter())
                    .map(|jarg| Expr::parse_from(jarg, ctx))
                    .collect::<Result<Vec<Expr>, ParseError<JSON>>>()?;

                let expr = NewExpression(Box::new(callee), arguments);
                Expr::New(expr)
            }
            "ObjectExpression" => {
                let expr = ObjectExpression::parse_from(jexpr, ctx)?;
                Expr::Object(expr)
            }
            "SequenceExpression" => {
                let expr = SequenceExpression::parse_from(jexpr, ctx)?;
                Expr::Sequence(expr)
            }
            "ThisExpression" => Expr::This,
            "UnaryExpression" => {
                let expr = UnaryExpression::parse_from(jexpr, ctx)?;
                Expr::Unary(expr)
            }
            "UpdateExpression" => {
                let expr = UpdateExpression::parse_from(jexpr, ctx)?;
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

impl ParseFrom<JSON> for Identifier {
    type Error = ParseError<JSON>;

    fn parse_from(jexpr: &JSON, ctx: &mut ParserContext) -> Result<Self, Self::Error> {
        let name = json_get_str(jexpr, "name")?;
        let identifier = Identifier::from(name);
        ctx.used_variables.insert(identifier.clone());
        Ok(identifier)
    }
}

impl ParseFrom<JSON> for UnaryExpression {
    type Error = ParseError<JSON>;

    fn parse_from(jexpr: &JSON, ctx: &mut ParserContext) -> Result<Self, Self::Error> {
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
        let argument = Expr::parse_from(jargument, ctx)?;

        Ok(UnaryExpression(op, Box::new(argument)))
    }
}

impl ParseFrom<JSON> for UpdateExpression {
    type Error = ParseError<JSON>;

    fn parse_from(jexpr: &JSON, ctx: &mut ParserContext) -> Result<Self, Self::Error> {
        let jargument = json_get(jexpr, "argument")?;
        let argument = Expr::parse_from(jargument, ctx)?;

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

impl ParseFrom<JSON> for SequenceExpression {
    type Error = ParseError<JSON>;

    fn parse_from(jexpr: &JSON, ctx: &mut ParserContext) -> Result<Self, Self::Error> {
        let jexprs = json_get_array(jexpr, "expressions")?;

        let exprs = (jexprs.into_iter())
            .map(|jexpr| Expr::parse_from(jexpr, ctx))
            .collect::<Result<Vec<Expr>, ParseError<JSON>>>()?;
        Ok(SequenceExpression(Box::new(exprs)))
    }
}

impl ParseFrom<JSON> for BinaryExpression {
    type Error = ParseError<JSON>;

    fn parse_from(jexpr: &JSON, ctx: &mut ParserContext) -> Result<Self, Self::Error> {
        let jleft = json_get(jexpr, "left")?;
        let left = Expr::parse_from(jleft, ctx)?;

        let jright = json_get(jexpr, "right")?;
        let right = Expr::parse_from(jright, ctx)?;

        let opstr = json_get_str(jexpr, "operator")?;
        let op = match opstr {
            "+" => BinOp::Plus,
            "-" => BinOp::Minus,
            "*" => BinOp::Star,
            "/" => BinOp::Slash,
            "%" => BinOp::Percent,
            "==" => BinOp::EqEq,
            "===" => BinOp::EqEqEq,
            "!=" => BinOp::NotEq,
            "!==" => BinOp::NotEqEq,
            "<" => BinOp::Less,
            ">" => BinOp::Greater,
            "<=" => BinOp::LtEq,
            ">=" => BinOp::GtEq,
            "|" => BinOp::Pipe,
            "^" => BinOp::Hat,
            "&" => BinOp::Ampersand,
            "<<" => BinOp::LtLt,
            ">>" => BinOp::GtGt,
            ">>>" => BinOp::GtGtGt,
            "in" => BinOp::In,
            "instanceof" => BinOp::InstanceOf,
            _ => {
                return Err(ParseError::UnexpectedValue {
                    want: "one of: + - * / % == === != < > <= >= instanceof | ^ & << >> >>>",
                    value: jexpr.get("operator").unwrap().clone(),
                })
            }
        };
        Ok(BinaryExpression(Box::new(left), op, Box::new(right)))
    }
}

impl ParseFrom<JSON> for LogicalExpression {
    type Error = ParseError<JSON>;

    fn parse_from(jexpr: &JSON, ctx: &mut ParserContext) -> Result<Self, Self::Error> {
        let jleft = json_get(jexpr, "left")?;
        let left = Expr::parse_from(jleft, ctx)?;

        let jright = json_get(jexpr, "right")?;
        let right = Expr::parse_from(jright, ctx)?;

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

impl ParseFrom<JSON> for AssignmentExpression {
    type Error = ParseError<JSON>;

    fn parse_from(jexpr: &JSON, ctx: &mut ParserContext) -> Result<Self, Self::Error> {
        let jright = json_get(jexpr, "right")?;
        let right = Expr::parse_from(jright, ctx)?;

        let jleft = json_get(jexpr, "left")?;
        let left = Expr::parse_from(jleft, ctx)?;

        let jop = json_get_str(jexpr, "operator")?;
        let modop = match jop {
            "=" => None,
            "+=" => Some(BinOp::Plus),
            "-=" => Some(BinOp::Minus),
            "*=" => Some(BinOp::Star),
            "/=" => Some(BinOp::Slash),
            "%=" => Some(BinOp::Percent),
            "<<=" => Some(BinOp::LtLt),
            ">>=" => Some(BinOp::GtGt),
            ">>>=" => Some(BinOp::GtGtGt),
            "|=" => Some(BinOp::Pipe),
            "^=" => Some(BinOp::Hat),
            "&=" => Some(BinOp::Ampersand),
            _ => {
                return Err(ParseError::UnexpectedValue {
                    want: "one of: = += -= *= /= %= <<= >>= >>>= |= ^= &=",
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

impl ParseFrom<JSON> for ObjectExpression {
    type Error = ParseError<JSON>;

    fn parse_from(jexpr: &JSON, ctx: &mut ParserContext) -> Result<Self, Self::Error> {
        json_expect_str(jexpr, "type", "ObjectExpression")?;

        let jproperties = json_get_array(jexpr, "properties")?;

        let mut properties = vec![];
        for jprop in jproperties.into_iter() {
            json_expect_str(jprop, "type", "Property")?;

            let jkey = json_get(jprop, "key")?;
            let keyexpr = Expr::parse_from(jkey, ctx)?;
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
            let value = Expr::parse_from(jvalue, ctx)?;

            properties.push((key, Box::new(value)));
        }
        Ok(ObjectExpression(properties))
    }
}

impl ParseFrom<JSON> for FunctionExpression {
    type Error = ParseError<JSON>;

    fn parse_from(jexpr: &JSON, ctx: &mut ParserContext) -> Result<Self, Self::Error> {
        let id: Option<Identifier> = json_get(jexpr, "id")
            .and_then(|jid| Identifier::parse_from(jid, ctx))
            .ok();

        let mut inner_ctx = ParserContext::new();
        let jparams = json_get_array(jexpr, "params")?;
        let params = (jparams.into_iter())
            .map(|jparam| Identifier::parse_from(jparam, &mut inner_ctx))
            .collect::<Result<Vec<_>, ParseError<JSON>>>()?;

        let jbody = json_get(jexpr, "body")?;

        let body = BlockStatement::parse_from(jbody, &mut inner_ctx)?;
        let body = Box::new(body);

        let ParserContext {
            used_variables: mut free_variables,
            declared_variables: variables,
        } = inner_ctx;

        free_variables.remove(&Identifier::from("arguments"));
        for var in params.iter().chain(variables.iter()) {
            free_variables.remove(var);
        }

        ctx.used_variables
            .extend(free_variables.clone().into_iter());

        Ok(FunctionExpression {
            id,
            params,
            variables,
            free_variables,
            body,
            is_generator: json_get_bool(jexpr, "generator").unwrap_or(false),
            is_expression: json_get_bool(jexpr, "expression").unwrap_or(false),
            is_async: json_get_bool(jexpr, "async").unwrap_or(false),
        })
    }
}
