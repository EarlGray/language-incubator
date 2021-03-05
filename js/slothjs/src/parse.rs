use std::collections::HashSet;

use serde_json;

use crate::ast::*; // yes, EVERYTHING.

use crate::error::ParseError;
use crate::object::JSON;
use crate::source;

type ParseResult<T, S> = Result<T, ParseError<S>>;

/// `ParserContext` collects lexical scope information to be used later.
#[derive(Debug)]
pub struct ParserContext {
    pub used_variables: HashSet<Identifier>,
    pub declared_variables: HashSet<Identifier>,
    pub declared_functions: Vec<FunctionDeclaration>,
}

impl ParserContext {
    pub fn new() -> ParserContext {
        ParserContext {
            used_variables: HashSet::new(),
            declared_variables: HashSet::new(),
            declared_functions: Vec::new(),
        }
    }
}

/// `ParseFrom` is a unifying interface for constructing all `ast::*` types.
///
/// It's not strictly necessary, `impl ast::X { fn parse_from(...) -> ParseResult<X> {...} }`
/// would do just fine. On the other hand, it feels like a good idea to have all of these
/// to conform to one interface.
trait ParseFrom: Sized {
    fn parse_from<S: SourceNode>(source: &S, ctx: &mut ParserContext) -> ParseResult<Self, S>;
}

/// `SourceNode` is how `ParseFrom::parse_from` sees AST nodes.
pub trait SourceNode: Sized + Clone {
    /// Make the node into a literal.
    fn get_literal(&self) -> Literal;

    /// Try to get source mapping for `self`.
    fn get_location(&self) -> Option<source::Location>;

    /// Get a child node with this name; it's a ParseError if it does not exist.
    fn get_node(&self, property: &'static str) -> ParseResult<&Self, Self>;

    /// Get a child node with this name; if it does not exist, return None.
    /// Then transform it through `action`, propagating its Result out.
    fn map_node<T, F>(&self, property: &'static str, action: F) -> ParseResult<Option<T>, Self>
    where
        F: FnMut(&Self) -> ParseResult<T, Self>;

    /// Get the boolean value of a child node with name `property`.
    /// It's a ParseError if it does not exist or does not have a boolean meaning.
    fn get_bool(&self, property: &'static str) -> ParseResult<bool, Self>;

    /// Get the string value of a child node with name `property`.
    /// It's a ParseError if it does not exist or does not have a string meaning.
    fn get_str(&self, property: &'static str) -> ParseResult<&str, Self>;

    /// Get the array of children of a child node with name `property`.
    /// It's a ParseError if it does not exist or does not have an array meaning.
    fn get_array(&self, property: &'static str) -> ParseResult<&[Self], Self>;

    /// Check that the value of `property` is a string equal to `value`.
    fn expect_str(&self, property: &'static str, value: &'static str) -> ParseResult<(), Self>;
}

impl SourceNode for JSON {
    fn get_literal(&self) -> Literal {
        Literal(self.clone())
    }

    fn get_location(&self) -> Option<source::Location> {
        // TODO: clone() can be costly
        serde_json::from_value::<source::Location>(self.clone()).ok()
    }

    fn get_node(&self, property: &'static str) -> ParseResult<&Self, Self> {
        self.get(property).ok_or_else(|| ParseError::ObjectWithout {
            attr: property,
            value: self.clone(),
        })
    }

    fn map_node<T, F>(&self, property: &'static str, mut action: F) -> ParseResult<Option<T>, Self>
    where
        F: FnMut(&Self) -> ParseResult<T, Self>,
    {
        let result = match self.get(property) {
            Some(child) if !child.is_null() => Some(action(child)?),
            _ => None,
        };
        Ok(result)
    }

    fn get_bool(&self, property: &'static str) -> ParseResult<bool, Self> {
        let jbool = self.get_node(property)?;
        jbool.as_bool().ok_or_else(|| ParseError::ShouldBeBool {
            value: jbool.clone(),
        })
    }

    fn get_str(&self, property: &'static str) -> ParseResult<&str, Self> {
        let jstr = self.get_node(property)?;
        jstr.as_str().ok_or_else(|| ParseError::ShouldBeString {
            value: jstr.clone(),
        })
    }

    fn get_array(&self, property: &'static str) -> ParseResult<&[Self], Self> {
        let jarray = self.get_node(property)?;
        let array = jarray.as_array().ok_or_else(|| ParseError::ShouldBeArray {
            value: jarray.clone(),
        })?;
        Ok(&array[..])
    }

    fn expect_str(&self, property: &'static str, value: &'static str) -> ParseResult<(), Self> {
        let jstr = self.get_node(property)?;
        let got = jstr.as_str().ok_or_else(|| ParseError::ShouldBeString {
            value: jstr.clone(),
        })?;
        if got != value {
            return Err(ParseError::UnexpectedValue {
                want: value,
                value: jstr.clone(),
            });
        }
        Ok(())
    }
}

impl Program {
    pub fn parse_from<S: SourceNode>(source: &S) -> ParseResult<Program, S> {
        source.expect_str("type", "Program")?;

        let mut ctx = ParserContext::new();
        let jbody = source.get_array("body")?;
        let body = (jbody.into_iter())
            .map(|jstmt| Statement::parse_from(jstmt, &mut ctx))
            .collect::<Result<Vec<Statement>, _>>()?;

        let ParserContext {
            declared_variables: variables,
            declared_functions: functions,
            ..
        } = ctx;
        Ok(Program {
            body,
            variables,
            functions,
        })
    }
}

impl ParseFrom for Statement {
    fn parse_from<S: SourceNode>(source: &S, ctx: &mut ParserContext) -> ParseResult<Self, S> {
        let loc = source.get_location().map(|loc| Box::new(loc));

        let typ = source.get_str("type")?;
        let stmt = match typ {
            "BlockStatement" => Stmt::Block(BlockStatement::parse_from(source, ctx)?),
            "BreakStatement" => Stmt::Break(BreakStatement::parse_from(source, ctx)?),
            "ContinueStatement" => Stmt::Continue(ContinueStatement::parse_from(source, ctx)?),
            "DoWhileStatement" => {
                let mut stmt = ForStatement::parse_from(source, ctx)?;
                stmt.init = stmt.body.clone();
                Stmt::For(Box::new(stmt))
            }
            "EmptyStatement" => Stmt::Empty,
            "ExpressionStatement" => Stmt::Expr(ExpressionStatement::parse_from(source, ctx)?),
            "ForStatement" | "WhileStatement" => {
                let stmt = ForStatement::parse_from(source, ctx)?;
                Stmt::For(Box::new(stmt))
            }
            "ForInStatement" => {
                let stmt = ForInStatement::parse_from(source, ctx)?;
                Stmt::ForIn(Box::new(stmt))
            }
            "FunctionDeclaration" => Stmt::Function(FunctionDeclaration::parse_from(source, ctx)?),
            "IfStatement" => {
                let stmt = IfStatement::parse_from(source, ctx)?;
                Stmt::If(Box::new(stmt))
            }
            "LabeledStatement" => {
                let stmt = LabelStatement::parse_from(source, ctx)?;
                Stmt::Label(Box::new(stmt))
            }
            "ReturnStatement" => Stmt::Return(ReturnStatement::parse_from(source, ctx)?),
            "SwitchStatement" => Stmt::Switch(SwitchStatement::parse_from(source, ctx)?),
            "ThrowStatement" => Stmt::Throw(ThrowStatement::parse_from(source, ctx)?),
            "TryStatement" => Stmt::Try(TryStatement::parse_from(source, ctx)?),
            "VariableDeclaration" => Stmt::Variable(VariableDeclaration::parse_from(source, ctx)?),
            _ => {
                return Err(ParseError::UnknownType {
                    value: source.clone(),
                })
            }
        };
        Ok(Statement { stmt, loc })
    }
}

impl ParseFrom for BlockStatement {
    fn parse_from<S: SourceNode>(source: &S, ctx: &mut ParserContext) -> ParseResult<Self, S> {
        let jbody = source.get_array("body")?;
        let body = (jbody.iter())
            .map(|jstmt| Statement::parse_from(jstmt, ctx))
            .collect::<ParseResult<Vec<Statement>, _>>()?;
        Ok(BlockStatement { body })
    }
}

impl ParseFrom for IfStatement {
    fn parse_from<S: SourceNode>(source: &S, ctx: &mut ParserContext) -> ParseResult<Self, S> {
        source.expect_str("type", "IfStatement")?;

        let jtest = source.get_node("test")?;
        let test = Expression::parse_from(jtest, ctx)?;

        let jthen = source.get_node("consequent")?;
        let consequent = Statement::parse_from(jthen, ctx)?;

        let alternate = source.map_node("alternate", |jelse| Statement::parse_from(jelse, ctx))?;

        Ok(IfStatement {
            test,
            consequent,
            alternate,
        })
    }
}

impl ParseFrom for SwitchStatement {
    fn parse_from<S: SourceNode>(source: &S, ctx: &mut ParserContext) -> ParseResult<Self, S> {
        source.expect_str("type", "SwitchStatement")?;

        let jdiscriminant = source.get_node("discriminant")?;
        let discriminant = Expression::parse_from(jdiscriminant, ctx)?;

        let jcases = source.get_array("cases")?;
        let cases = (jcases.into_iter())
            .map(|jcase| {
                let test = jcase.map_node("test", |jtest| Expression::parse_from(jtest, ctx))?;

                let jconsequent = jcase.get_array("consequent")?;
                let consequent = (jconsequent.into_iter())
                    .map(|jstmt| Statement::parse_from(jstmt, ctx))
                    .collect::<Result<Vec<Statement>, _>>()?;

                Ok(SwitchCase { test, consequent })
            })
            .collect::<Result<Vec<SwitchCase>, _>>()?;

        Ok(SwitchStatement {
            discriminant,
            cases,
        })
    }
}

impl ParseFrom for ForStatement {
    fn parse_from<S: SourceNode>(source: &S, ctx: &mut ParserContext) -> ParseResult<Self, S> {
        let for_ok = source.expect_str("type", "ForStatement");
        let while_ok = source.expect_str("type", "WhileStatement");
        let dowhile_ok = source.expect_str("type", "DoWhileStatement");
        for_ok.or(while_ok).or(dowhile_ok)?;

        let init = source
            .map_node("init", |jinit| {
                if let Ok(var) = VariableDeclaration::parse_from(jinit, ctx) {
                    let stmt = Stmt::Variable(var);
                    Ok(Statement { stmt, loc: None })
                } else if let Ok(expr) = Expression::parse_from(jinit, ctx) {
                    let stmt = Stmt::Expr(ExpressionStatement { expression: expr });
                    Ok(Statement { stmt, loc: None })
                } else {
                    panic!("jinit is not Expression | VariableDeclaration")
                }
            })?
            .unwrap_or(Statement {
                stmt: Stmt::Empty,
                loc: None,
            });

        let test = source.map_node("test", |jtest| Expression::parse_from(jtest, ctx))?;
        let update = source.map_node("update", |jupdate| Expression::parse_from(jupdate, ctx))?;

        let jbody = source.get_node("body")?;
        let body = Statement::parse_from(jbody, ctx)?;
        Ok(ForStatement {
            init,
            test,
            update,
            body,
        })
    }
}

impl ParseFrom for ForInStatement {
    fn parse_from<S: SourceNode>(source: &S, ctx: &mut ParserContext) -> ParseResult<Self, S> {
        source.expect_str("type", "ForInStatement")?;

        let jleft = source.get_node("left")?;
        let left = if let Ok(vardecl) = VariableDeclaration::parse_from(jleft, ctx) {
            ForInTarget::Var(vardecl)
        } else if let Ok(expr) = Expression::parse_from(jleft, ctx) {
            ForInTarget::Expr(expr)
        } else {
            return Err(ParseError::UnexpectedValue {
                want: "VariableDeclaration | Pattern",
                value: jleft.clone(),
            });
        };

        let jright = source.get_node("right")?;
        let right = Expression::parse_from(jright, ctx)?;

        let jbody = source.get_node("body")?;
        let body = Statement::parse_from(jbody, ctx)?;

        Ok(ForInStatement { left, right, body })
    }
}

impl ParseFrom for BreakStatement {
    fn parse_from<S: SourceNode>(source: &S, ctx: &mut ParserContext) -> ParseResult<Self, S> {
        source.expect_str("type", "BreakStatement")?;

        let label = source.map_node("label", |jlabel| Identifier::parse_from(jlabel, ctx))?;
        Ok(BreakStatement(label))
    }
}

impl ParseFrom for ContinueStatement {
    fn parse_from<S: SourceNode>(source: &S, ctx: &mut ParserContext) -> ParseResult<Self, S> {
        source.expect_str("type", "ContinueStatement")?;

        let label = source.map_node("label", |jlabel| Identifier::parse_from(jlabel, ctx))?;
        Ok(ContinueStatement(label))
    }
}

impl ParseFrom for LabelStatement {
    fn parse_from<S: SourceNode>(source: &S, ctx: &mut ParserContext) -> ParseResult<Self, S> {
        source.expect_str("type", "LabeledStatement")?;

        let jlabel = source.get_node("label")?;
        let label = Identifier::parse_from(jlabel, ctx)?;

        let jbody = source.get_node("body")?;
        let body = Statement::parse_from(jbody, ctx)?;

        Ok(LabelStatement(label, body))
    }
}

impl ParseFrom for ReturnStatement {
    fn parse_from<S: SourceNode>(source: &S, ctx: &mut ParserContext) -> ParseResult<Self, S> {
        source.expect_str("type", "ReturnStatement")?;

        let argument =
            source.map_node("argument", |jobject| Expression::parse_from(jobject, ctx))?;
        Ok(ReturnStatement(argument))
    }
}

impl ParseFrom for ThrowStatement {
    fn parse_from<S: SourceNode>(source: &S, ctx: &mut ParserContext) -> ParseResult<Self, S> {
        source.expect_str("type", "ThrowStatement")?;

        let jargument = source.get_node("argument")?;
        let argument = Expression::parse_from(jargument, ctx)?;
        Ok(ThrowStatement(argument))
    }
}

impl ParseFrom for TryStatement {
    fn parse_from<S: SourceNode>(source: &S, ctx: &mut ParserContext) -> ParseResult<Self, S> {
        source.expect_str("type", "TryStatement")?;

        let jblock = source.get_node("block")?;
        let block = BlockStatement::parse_from(jblock, ctx)?;

        let handler = source.map_node("handler", |jhandler| {
            let jparam = jhandler.get_node("param")?;
            let param = Identifier::parse_from(jparam, ctx)?;

            let jbody = jhandler.get_node("body")?;
            let body = BlockStatement::parse_from(jbody, ctx)?;

            Ok(CatchClause { param, body })
        })?;

        let finalizer = source.map_node("finalizer", |jobject| {
            BlockStatement::parse_from(jobject, ctx)
        })?;

        Ok(TryStatement {
            block,
            handler,
            finalizer,
        })
    }
}

impl ParseFrom for VariableDeclaration {
    fn parse_from<S: SourceNode>(source: &S, ctx: &mut ParserContext) -> ParseResult<Self, S> {
        source.expect_str("type", "VariableDeclaration")?;

        let kind = match source.get_str("kind")? {
            "const" => todo!(), // DeclarationKind::Const,
            "let" => todo!(),   // DeclarationKind::Let,
            "var" => DeclarationKind::Var,
            _ => {
                return Err(ParseError::UnexpectedValue {
                    want: "var | let | const",
                    value: source.get_node("kind")?.clone(),
                })
            }
        };
        let jdeclarations = source.get_array("declarations")?;

        let mut declarations = vec![];
        for decl in jdeclarations.into_iter() {
            decl.expect_str("type", "VariableDeclarator")?;

            let jid = decl.get_node("id")?;
            let name = Identifier::parse_from(jid, ctx)?;

            let init = decl.map_node("init", |jinit| {
                let expr = Expression::parse_from(jinit, ctx)?;
                Ok(Box::new(expr))
            })?;

            ctx.declared_variables.insert(name.clone());
            declarations.push(VariableDeclarator { name, init });
        }
        Ok(VariableDeclaration { kind, declarations })
    }
}

impl ParseFrom for FunctionDeclaration {
    fn parse_from<S: SourceNode>(source: &S, ctx: &mut ParserContext) -> ParseResult<Self, S> {
        source.expect_str("type", "FunctionDeclaration")?;

        let function = FunctionExpression::parse_from(source, ctx)?;
        let id = (function.id.clone()).ok_or_else(|| ParseError::ObjectWithout {
            attr: "id",
            value: source.clone(),
        })?;
        let funcdecl = FunctionDeclaration { id, function };
        ctx.declared_functions.push(funcdecl.clone());
        Ok(funcdecl)
    }
}

impl ParseFrom for ExpressionStatement {
    fn parse_from<S: SourceNode>(source: &S, ctx: &mut ParserContext) -> ParseResult<Self, S> {
        source.expect_str("type", "ExpressionStatement")?;

        let jexpr = source.get_node("expression")?;
        let expression = Expression::parse_from(jexpr, ctx)?;
        Ok(ExpressionStatement { expression })
    }
}

impl ParseFrom for Expression {
    fn parse_from<S: SourceNode>(source: &S, ctx: &mut ParserContext) -> ParseResult<Self, S> {
        let expr_type = source.get_str("type")?;

        let expr = match expr_type {
            "ArrayExpression" => {
                let jelements = source.get_array("elements")?;
                let elements = (jelements.into_iter())
                    .map(|jelem| Expression::parse_from(jelem, ctx))
                    .collect::<Result<Vec<Expression>, _>>()?;
                let expr = ArrayExpression(elements);
                Expr::Array(expr)
            }
            "AssignmentExpression" => {
                let expr = AssignmentExpression::parse_from(source, ctx)?;
                Expr::Assign(Box::new(expr))
            }
            "BinaryExpression" => {
                let expr = BinaryExpression::parse_from(source, ctx)?;
                Expr::BinaryOp(Box::new(expr))
            }
            "CallExpression" => {
                let jcallee = source.get_node("callee")?;
                let callee = Expression::parse_from(jcallee, ctx)?;

                let jarguments = source.get_array("arguments")?;
                let arguments = (jarguments.into_iter())
                    .map(|jarg| Expression::parse_from(jarg, ctx))
                    .collect::<Result<Vec<Expression>, _>>()?;

                Expr::Call(Box::new(CallExpression(callee, arguments)))
            }
            "ConditionalExpression" => {
                let jtest = source.get_node("test")?;
                let condexpr = Expression::parse_from(jtest, ctx)?;

                let jthen = source.get_node("consequent")?;
                let thenexpr = Expression::parse_from(jthen, ctx)?;

                let jelse = source.get_node("alternate")?;
                let elseexpr = Expression::parse_from(jelse, ctx)?;

                let expr = ConditionalExpression {
                    condexpr,
                    thenexpr,
                    elseexpr,
                };
                Expr::Conditional(Box::new(expr))
            }
            "FunctionExpression" => {
                let expr = FunctionExpression::parse_from(source, ctx)?;
                Expr::Function(expr)
            }
            "Identifier" => {
                let expr = Identifier::parse_from(source, ctx)?;
                Expr::Identifier(expr)
            }
            "Literal" => {
                let jval = source.get_node("value")?;
                Expr::Literal(jval.get_literal())
            }
            "LogicalExpression" => {
                let expr = LogicalExpression::parse_from(source, ctx)?;
                Expr::LogicalOp(Box::new(expr))
            }
            "MemberExpression" => {
                let computed = source.get_bool("computed")?;

                let jobject = source.get_node("object")?;
                let object = Expression::parse_from(jobject, ctx)?;

                let jproperty = source.get_node("property")?;
                let property = Expression::parse_from(jproperty, ctx)?;
                let expr = MemberExpression(object, property, computed);
                Expr::Member(Box::new(expr))
            }
            "NewExpression" => {
                let jcallee = source.get_node("callee")?;
                let callee = Expression::parse_from(jcallee, ctx)?;

                let jarguments = source.get_array("arguments")?;
                let arguments = (jarguments.into_iter())
                    .map(|jarg| Expression::parse_from(jarg, ctx))
                    .collect::<Result<Vec<Expression>, _>>()?;

                let expr = NewExpression(callee, arguments);
                Expr::New(Box::new(expr))
            }
            "ObjectExpression" => {
                let expr = ObjectExpression::parse_from(source, ctx)?;
                Expr::Object(expr)
            }
            "SequenceExpression" => {
                let expr = SequenceExpression::parse_from(source, ctx)?;
                Expr::Sequence(expr)
            }
            "ThisExpression" => Expr::This,
            "UnaryExpression" => {
                let expr = UnaryExpression::parse_from(source, ctx)?;
                Expr::Unary(expr)
            }
            "UpdateExpression" => {
                let expr = UpdateExpression::parse_from(source, ctx)?;
                Expr::Update(Box::new(expr))
            }
            _ => {
                return Err(ParseError::UnknownType {
                    value: source.clone(),
                })
            }
        };
        Ok(Expression { expr, loc: None })
    }
}

impl ParseFrom for Identifier {
    fn parse_from<S: SourceNode>(source: &S, ctx: &mut ParserContext) -> ParseResult<Self, S> {
        let name = source.get_str("name")?;
        let identifier = Identifier::from(name);
        ctx.used_variables.insert(identifier.clone());
        Ok(identifier)
    }
}

impl ParseFrom for UnaryExpression {
    fn parse_from<S: SourceNode>(source: &S, ctx: &mut ParserContext) -> ParseResult<Self, S> {
        let jop = source.get_str("operator")?;
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
                    value: source.clone(),
                })
            }
        };

        let jargument = source.get_node("argument")?;
        let argument = Expression::parse_from(jargument, ctx)?;

        Ok(UnaryExpression(op, Box::new(argument)))
    }
}

impl ParseFrom for UpdateExpression {
    fn parse_from<S: SourceNode>(source: &S, ctx: &mut ParserContext) -> ParseResult<Self, S> {
        let jargument = source.get_node("argument")?;
        let argument = Expression::parse_from(jargument, ctx)?;

        let prefix = source.get_bool("prefix")?;
        let operator = source.get_str("operator")?;

        let op = match operator {
            "++" => UpdOp::Increment,
            "--" => UpdOp::Decrement,
            _ => {
                return Err(ParseError::UnexpectedValue {
                    want: "++ or --",
                    value: source.clone(),
                })
            }
        };
        Ok(UpdateExpression(op, prefix, argument))
    }
}

impl ParseFrom for SequenceExpression {
    fn parse_from<S: SourceNode>(source: &S, ctx: &mut ParserContext) -> ParseResult<Self, S> {
        let jexprs = source.get_array("expressions")?;

        let exprs = (jexprs.into_iter())
            .map(|jexpr| Expression::parse_from(jexpr, ctx))
            .collect::<Result<Vec<Expression>, _>>()?;
        Ok(SequenceExpression(exprs))
    }
}

impl ParseFrom for BinaryExpression {
    fn parse_from<S: SourceNode>(source: &S, ctx: &mut ParserContext) -> ParseResult<Self, S> {
        let jleft = source.get_node("left")?;
        let left = Expression::parse_from(jleft, ctx)?;

        let jright = source.get_node("right")?;
        let right = Expression::parse_from(jright, ctx)?;

        let opstr = source.get_str("operator")?;
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
                    value: source.get_node("operator")?.clone(),
                })
            }
        };
        Ok(BinaryExpression(left, op, right))
    }
}

impl ParseFrom for LogicalExpression {
    fn parse_from<S: SourceNode>(source: &S, ctx: &mut ParserContext) -> ParseResult<Self, S> {
        let jleft = source.get_node("left")?;
        let left = Expression::parse_from(jleft, ctx)?;

        let jright = source.get_node("right")?;
        let right = Expression::parse_from(jright, ctx)?;

        let opstr = source.get_str("operator")?;
        let op = match opstr {
            "&&" => BoolOp::And,
            "||" => BoolOp::Or,
            _ => {
                return Err(ParseError::UnexpectedValue {
                    want: "&& or ||",
                    value: source.get_node("operator")?.clone(),
                })
            }
        };
        Ok(LogicalExpression(left, op, right))
    }
}

impl ParseFrom for AssignmentExpression {
    fn parse_from<S: SourceNode>(source: &S, ctx: &mut ParserContext) -> ParseResult<Self, S> {
        let jright = source.get_node("right")?;
        let right = Expression::parse_from(jright, ctx)?;

        let jleft = source.get_node("left")?;
        let left = Expression::parse_from(jleft, ctx)?;

        let jop = source.get_str("operator")?;
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
                    value: source.get_node("operator")?.clone(),
                })
            }
        };
        Ok(AssignmentExpression(left, AssignOp(modop), right))
    }
}

impl ParseFrom for ObjectExpression {
    fn parse_from<S: SourceNode>(source: &S, ctx: &mut ParserContext) -> ParseResult<Self, S> {
        source.expect_str("type", "ObjectExpression")?;

        let jproperties = source.get_array("properties")?;

        let mut properties = vec![];
        for jprop in jproperties.into_iter() {
            jprop.expect_str("type", "Property")?;

            let jkey = jprop.get_node("key")?;
            let keyexpr = Expression::parse_from(jkey, ctx)?;
            let key = if jprop.get_bool("computed")? {
                ObjectKey::Computed(keyexpr)
            } else {
                match keyexpr.expr {
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

            let jvalue = jprop.get_node("value")?;
            let value = Expression::parse_from(jvalue, ctx)?;

            properties.push((key, value));
        }
        Ok(ObjectExpression(properties))
    }
}

impl ParseFrom for FunctionExpression {
    fn parse_from<S: SourceNode>(source: &S, ctx: &mut ParserContext) -> ParseResult<Self, S> {
        let id: Option<Identifier> = source
            .get_node("id")
            .and_then(|jid| Identifier::parse_from(jid, ctx))
            .ok();

        let mut inner_ctx = ParserContext::new();
        let jparams = source.get_array("params")?;
        let params = (jparams.into_iter())
            .map(|jparam| Identifier::parse_from(jparam, &mut inner_ctx))
            .collect::<Result<Vec<Identifier>, _>>()?;

        let jbody = source.get_node("body")?;
        let body = BlockStatement::parse_from(jbody, &mut inner_ctx)?;

        let ParserContext {
            used_variables: mut free_variables,
            declared_variables: variables,
            declared_functions: functions,
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
            functions,
            free_variables,
            body,
            is_generator: source.get_bool("generator").unwrap_or(false),
            is_expression: source.get_bool("expression").unwrap_or(false),
            is_async: source.get_bool("async").unwrap_or(false),
        })
    }
}
