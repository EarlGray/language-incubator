mod heapnode;
mod jsonnode;
#[cfg(test)]
mod test;

use crate::prelude::*;

use crate::ast::*; // yes, EVERYTHING.

use crate::error::ParseError;
use crate::source;
use crate::{
    JSRef,
    JSValue,
    JSON,
};

pub use self::heapnode::HeapNode;

type ParseResult<T> = Result<T, ParseError>;

/// `ParserContext` collects lexical scope information to be used later.
#[derive(Debug)]
pub struct ParserContext {
    pub declared_bindings: HashSet<Identifier>, // let|const ...
    pub declared_functions: Vec<FunctionDeclaration>, // function ...
    pub declared_variables: HashSet<Identifier>, // var ...

    pub used_identifiers: HashSet<Identifier>, // note: they are not free before they leave the scope
}

impl ParserContext {
    pub fn new() -> ParserContext {
        ParserContext {
            used_identifiers: HashSet::new(),
            declared_bindings: HashSet::new(),
            declared_variables: HashSet::new(),
            declared_functions: Vec::new(),
        }
    }

    fn remember_declaration(
        &mut self,
        kind: DeclarationKind,
        name: &Identifier,
    ) -> Result<(), ParseError> {
        let in_bindings = self.declared_bindings.contains(name);
        let in_variables = self.declared_variables.contains(name);
        match (kind, in_bindings, in_variables) {
            (DeclarationKind::Var, false, _) => self.declared_variables.insert(name.clone()),
            (DeclarationKind::Let, false, _) => self.declared_bindings.insert(name.clone()),
            _ => return Err(ParseError::BindingRedeclared {}),
        };
        Ok(())
    }

    fn enter_block_scope<T, F>(&mut self, mut action: F) -> ParseResult<(T, HashSet<Identifier>)>
    where
        F: FnMut(&mut ParserContext) -> ParseResult<T>,
    {
        // inner_ctx accumulates used identifiers and declared bindings.
        let mut inner_ctx = ParserContext::new();
        core::mem::swap(
            &mut self.declared_variables,
            &mut inner_ctx.declared_variables,
        );
        core::mem::swap(
            &mut self.declared_functions,
            &mut inner_ctx.declared_functions,
        );

        let result = action(&mut inner_ctx);

        core::mem::swap(
            &mut self.declared_variables,
            &mut inner_ctx.declared_variables,
        );
        core::mem::swap(
            &mut self.declared_functions,
            &mut inner_ctx.declared_functions,
        );

        let ParserContext {
            declared_bindings: bindings,
            used_identifiers: mut used_variables,
            ..
        } = inner_ctx;

        for binding in bindings.iter() {
            used_variables.remove(binding);
        }

        // add all remaining usages to the outer used_variables
        self.used_identifiers.extend(used_variables);

        // put declared bindings into BlockStatement; discard them from ParserContext
        let result = result?;
        Ok((result, bindings))
    }
}

/// `ParseFrom` is a unifying interface for constructing all `ast::*` types.
///
/// It's not strictly necessary, `impl ast::X { fn parse_from(...) -> ParseResult<X> {...} }`
/// would do just fine. On the other hand, it feels like a good idea to have all of these
/// to conform to one interface.
trait ParseFrom: Sized {
    fn parse_from<S>(source: &S, ctx: &mut ParserContext) -> ParseResult<Self>
    where
        S: SourceNode;
}

/// `SourceNode` is how `ParseFrom::parse_from` sees AST nodes.
pub trait SourceNode: Sized {
    type Error: fmt::Debug;

    /// Location of the node where an error happened.
    fn to_error(&self) -> JSON;

    /// Try to get source mapping for `self`.
    fn get_location(&self) -> Option<source::Location>;

    /// Make the node into a literal.
    fn get_literal(&self, property: &str) -> ParseResult<Literal>;

    /// Get the boolean value of a child node with name `property`.
    /// It's a ParseError if it does not exist or does not have a boolean meaning.
    fn get_bool(&self, property: &str) -> ParseResult<bool>;

    /// Get the string value of a child node with name `property`.
    /// It's a ParseError if it does not exist or does not have a string meaning.
    fn get_str(&self, property: &str) -> ParseResult<String>;

    /// Check that the value of `property` is a string equal to `value`.
    fn expect_str(&self, property: &str, value: &'static str) -> ParseResult<()> {
        let got = self.get_str(property)?;
        match got == value {
            true => Ok(()),
            false => Err(ParseError::UnexpectedValue {
                want: value,
                value: self.to_error(),
            }),
        }
    }

    /// Get a child node with this name; if it does not exist, return None.
    /// Then transform it through `action`, propagating its Result out.
    /// A child node exis
    fn map_node<T, F>(&self, property: &str, action: F) -> ParseResult<T>
    where
        F: FnMut(&Self) -> ParseResult<T>;

    fn map_opt_node<T, F>(&self, property: &str, action: F) -> ParseResult<Option<T>>
    where
        F: FnMut(&Self) -> ParseResult<T>,
    {
        match self.map_node(property, action) {
            Ok(stmt) => Ok(Some(stmt)),
            Err(ParseError::ObjectWithout { .. }) => Ok(None),
            Err(e) => Err(e),
        }
    }

    /// Map the array of children of a child node with name `property`.
    /// It's a ParseError if it does not exist or does not have an array meaning.
    fn map_array<T, F>(&self, property: &str, func: F) -> ParseResult<Vec<T>>
    where
        F: FnMut(&Self) -> ParseResult<T>;
}

impl Program {
    pub fn parse_from<S: SourceNode>(source: &S) -> ParseResult<Program> {
        source.expect_str("type", "Program")?;

        let mut ctx = ParserContext::new();
        let body = BlockStatement::parse_from(source, &mut ctx)?;

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
    fn parse_from<S: SourceNode>(source: &S, ctx: &mut ParserContext) -> ParseResult<Self> {
        let loc = source.get_location().map(Box::new);

        let typ = source.get_str("type")?;
        let stmt = match typ.as_str() {
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
                    value: source.to_error(),
                })
            }
        };
        Ok(Statement { stmt, loc })
    }
}

impl ParseFrom for BlockStatement {
    fn parse_from<S: SourceNode>(source: &S, ctx: &mut ParserContext) -> ParseResult<Self> {
        let (body, bindings) = ctx.enter_block_scope(|ctx| {
            source.map_array("body", |jstmt| Statement::parse_from(jstmt, ctx))
        })?;

        Ok(BlockStatement { body, bindings })
    }
}

impl ParseFrom for IfStatement {
    fn parse_from<S: SourceNode>(source: &S, ctx: &mut ParserContext) -> ParseResult<Self> {
        source.expect_str("type", "IfStatement")?;

        let test = source.map_node("test", |jtest| Expression::parse_from(jtest, ctx))?;
        let consequent =
            source.map_node("consequent", |jthen| Statement::parse_from(jthen, ctx))?;
        let alternate =
            match source.map_node("alternate", |jelse| Statement::parse_from(jelse, ctx)) {
                Ok(stmt) => Some(stmt),
                Err(ParseError::ObjectWithout { .. }) => None,
                Err(e) => return Err(e),
            };

        Ok(IfStatement {
            test,
            consequent,
            alternate,
        })
    }
}

impl ParseFrom for SwitchStatement {
    fn parse_from<S: SourceNode>(source: &S, ctx: &mut ParserContext) -> ParseResult<Self> {
        source.expect_str("type", "SwitchStatement")?;

        let discriminant = source.map_node("discriminant", |jdiscriminant| {
            Expression::parse_from(jdiscriminant, ctx)
        })?;

        let cases = source.map_array("cases", |jcase| {
            let test = jcase.map_opt_node("test", |jtest| Expression::parse_from(jtest, ctx))?;

            let consequent =
                jcase.map_array("consequent", |jstmt| Statement::parse_from(jstmt, ctx))?;
            Ok(SwitchCase { test, consequent })
        })?;

        Ok(SwitchStatement {
            discriminant,
            cases,
        })
    }
}

impl ParseFrom for ForStatement {
    fn parse_from<S: SourceNode>(source: &S, ctx: &mut ParserContext) -> ParseResult<Self> {
        let for_ok = source.expect_str("type", "ForStatement");
        let while_ok = source.expect_str("type", "WhileStatement");
        let dowhile_ok = source.expect_str("type", "DoWhileStatement");
        for_ok.or(while_ok).or(dowhile_ok)?;

        let init = source
            .map_opt_node("init", |jinit| {
                if let Ok(var) = VariableDeclaration::parse_from(jinit, ctx) {
                    let stmt = Stmt::Variable(var);
                    Ok(Statement { stmt, loc: None })
                } else if let Ok(expr) = Expression::parse_from(jinit, ctx) {
                    let stmt = Stmt::Expr(ExpressionStatement { expression: expr });
                    Ok(Statement { stmt, loc: None })
                } else {
                    Err(ParseError::UnexpectedValue {
                        want: "variable or expression",
                        value: jinit.to_error(),
                    })
                }
            })?
            .unwrap_or(Statement {
                stmt: Stmt::Empty,
                loc: None,
            });

        let test = source.map_opt_node("test", |jtest| Expression::parse_from(jtest, ctx))?;
        let update =
            source.map_opt_node("update", |jupdate| Expression::parse_from(jupdate, ctx))?;

        let body = source.map_node("body", |jbody| Statement::parse_from(jbody, ctx))?;
        Ok(ForStatement {
            init,
            test,
            update,
            body,
        })
    }
}

impl ParseFrom for ForInStatement {
    fn parse_from<S: SourceNode>(source: &S, ctx: &mut ParserContext) -> ParseResult<Self> {
        source.expect_str("type", "ForInStatement")?;

        let left = source.map_node("left", |jleft| {
            if let Ok(vardecl) = VariableDeclaration::parse_from(jleft, ctx) {
                Ok(ForInTarget::Var(vardecl))
            } else if let Ok(expr) = Expression::parse_from(jleft, ctx) {
                Ok(ForInTarget::Expr(expr))
            } else {
                Err(ParseError::UnexpectedValue {
                    want: "VariableDeclaration | Pattern",
                    value: jleft.to_error(),
                })
            }
        })?;
        let right = source.map_node("right", |jright| Expression::parse_from(jright, ctx))?;
        let body = source.map_node("body", |jbody| Statement::parse_from(jbody, ctx))?;
        Ok(ForInStatement { left, right, body })
    }
}

impl ParseFrom for BreakStatement {
    fn parse_from<S: SourceNode>(source: &S, ctx: &mut ParserContext) -> ParseResult<Self> {
        source.expect_str("type", "BreakStatement")?;

        let label = source.map_opt_node("label", |jlabel| Identifier::parse_from(jlabel, ctx))?;
        Ok(BreakStatement(label))
    }
}

impl ParseFrom for ContinueStatement {
    fn parse_from<S: SourceNode>(source: &S, ctx: &mut ParserContext) -> ParseResult<Self> {
        source.expect_str("type", "ContinueStatement")?;

        let label = source.map_opt_node("label", |jlabel| Identifier::parse_from(jlabel, ctx))?;
        Ok(ContinueStatement(label))
    }
}

impl ParseFrom for LabelStatement {
    fn parse_from<S: SourceNode>(source: &S, ctx: &mut ParserContext) -> ParseResult<Self> {
        source.expect_str("type", "LabeledStatement")?;

        let label = source.map_node("label", |jlabel| Identifier::parse_from(jlabel, ctx))?;
        let body = source.map_node("body", |jbody| Statement::parse_from(jbody, ctx))?;
        Ok(LabelStatement(label, body))
    }
}

impl ParseFrom for ReturnStatement {
    fn parse_from<S: SourceNode>(source: &S, ctx: &mut ParserContext) -> ParseResult<Self> {
        source.expect_str("type", "ReturnStatement")?;

        let argument =
            source.map_opt_node("argument", |jobject| Expression::parse_from(jobject, ctx))?;
        Ok(ReturnStatement(argument))
    }
}

impl ParseFrom for ThrowStatement {
    fn parse_from<S: SourceNode>(source: &S, ctx: &mut ParserContext) -> ParseResult<Self> {
        source.expect_str("type", "ThrowStatement")?;

        let argument = source.map_node("argument", |jarg| Expression::parse_from(jarg, ctx))?;
        Ok(ThrowStatement(argument))
    }
}

impl ParseFrom for TryStatement {
    fn parse_from<S: SourceNode>(source: &S, ctx: &mut ParserContext) -> ParseResult<Self> {
        source.expect_str("type", "TryStatement")?;

        let block = source.map_node("block", |jblock| BlockStatement::parse_from(jblock, ctx))?;

        let handler = source.map_opt_node("handler", |jhandler| {
            let param = jhandler.map_node("param", |jparam| Identifier::parse_from(jparam, ctx))?;
            let body = jhandler.map_node("body", |jbody| BlockStatement::parse_from(jbody, ctx))?;
            Ok(CatchClause { param, body })
        })?;

        let finalizer = source.map_opt_node("finalizer", |jobject| {
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
    fn parse_from<S: SourceNode>(source: &S, ctx: &mut ParserContext) -> ParseResult<Self> {
        source.expect_str("type", "VariableDeclaration")?;

        let kind = match source.get_str("kind")?.as_str() {
            "const" => todo!("DeclarationKind::Const"),
            "let" => DeclarationKind::Let,
            "var" => DeclarationKind::Var,
            _ => {
                return Err(ParseError::UnexpectedValue {
                    want: "var | let | const",
                    value: source.map_node("kind", |node| Ok(node.to_error()))?,
                })?;
            }
        };
        let declarations = source.map_array("declarations", |decl| {
            decl.expect_str("type", "VariableDeclarator")?;

            let name = decl.map_node("id", |jid| Identifier::parse_from(jid, ctx))?;

            let init = decl.map_opt_node("init", |jinit| {
                let expr = Expression::parse_from(jinit, ctx)?;
                Ok(Box::new(expr))
            })?;

            ctx.remember_declaration(kind, &name)?;
            Ok(VariableDeclarator { name, init })
        })?;

        Ok(VariableDeclaration { kind, declarations })
    }
}

impl ParseFrom for FunctionDeclaration {
    fn parse_from<S: SourceNode>(source: &S, ctx: &mut ParserContext) -> ParseResult<Self> {
        source.expect_str("type", "FunctionDeclaration")?;

        // Reuse similar structure of FunctionExpression and FunctionDeclaration trees:
        let function = FunctionExpression::parse_from(source, ctx)?;

        // id is mandatory in FunctionDeclaration:
        let id = (function.func.id.clone())
            .ok_or_else(|| ParseError::no_attr("id", source.to_error()))?;

        let funcdecl = FunctionDeclaration { id, function };
        ctx.declared_functions.push(funcdecl.clone());
        Ok(funcdecl)
    }
}

impl ParseFrom for ExpressionStatement {
    fn parse_from<S: SourceNode>(source: &S, ctx: &mut ParserContext) -> ParseResult<Self> {
        source.expect_str("type", "ExpressionStatement")?;

        let expression =
            source.map_node("expression", |jexpr| Expression::parse_from(jexpr, ctx))?;
        Ok(ExpressionStatement { expression })
    }
}

impl ParseFrom for Expression {
    fn parse_from<S: SourceNode>(source: &S, ctx: &mut ParserContext) -> ParseResult<Self> {
        let loc = source.get_location().map(Box::new);

        let expr_type = source.get_str("type")?;
        let expr = match expr_type.as_str() {
            "ArrayExpression" => {
                let elements =
                    source.map_array("elements", |jelem| Expression::parse_from(jelem, ctx))?;
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
                let callee =
                    source.map_node("callee", |jcallee| Expression::parse_from(jcallee, ctx))?;
                let arguments =
                    source.map_array("arguments", |jarg| Expression::parse_from(jarg, ctx))?;
                Expr::Call(Box::new(CallExpression(callee, arguments)))
            }
            "ConditionalExpression" => {
                let condexpr =
                    source.map_node("test", |jtest| Expression::parse_from(jtest, ctx))?;
                let thenexpr =
                    source.map_node("consequent", |jthen| Expression::parse_from(jthen, ctx))?;
                let elseexpr =
                    source.map_node("alternate", |jelse| Expression::parse_from(jelse, ctx))?;
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
                let lit = source.get_literal("value")?;
                Expr::Literal(lit)
            }
            "LogicalExpression" => {
                let expr = LogicalExpression::parse_from(source, ctx)?;
                Expr::LogicalOp(Box::new(expr))
            }
            "MemberExpression" => {
                let computed = source.get_bool("computed")?;

                let object = source.map_node("object", |jobj| Expression::parse_from(jobj, ctx))?;
                let property =
                    source.map_node("property", |jprop| Expression::parse_from(jprop, ctx))?;
                let expr = MemberExpression(object, property, computed);
                Expr::Member(Box::new(expr))
            }
            "NewExpression" => {
                let callee =
                    source.map_node("callee", |jcallee| Expression::parse_from(jcallee, ctx))?;
                let arguments =
                    source.map_array("arguments", |jarg| Expression::parse_from(jarg, ctx))?;
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
                    value: source.to_error(),
                })
            }
        };
        Ok(Expression { expr, loc })
    }
}

impl ParseFrom for Identifier {
    fn parse_from<S: SourceNode>(source: &S, ctx: &mut ParserContext) -> ParseResult<Self> {
        let name = source.get_str("name")?;
        let identifier = Identifier(name);
        ctx.used_identifiers.insert(identifier.clone());
        Ok(identifier)
    }
}

impl ParseFrom for UnaryExpression {
    fn parse_from<S: SourceNode>(source: &S, ctx: &mut ParserContext) -> ParseResult<Self> {
        let jop = source.get_str("operator")?;
        let op = match jop.as_str() {
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
                    value: source.to_error(),
                })
            }
        };

        let argument = source.map_node("argument", |jarg| Expression::parse_from(jarg, ctx))?;
        Ok(UnaryExpression(op, Box::new(argument)))
    }
}

impl ParseFrom for UpdateExpression {
    fn parse_from<S: SourceNode>(source: &S, ctx: &mut ParserContext) -> ParseResult<Self> {
        let argument = source.map_node("argument", |jarg| Expression::parse_from(jarg, ctx))?;
        let prefix = source.get_bool("prefix")?;
        let operator = source.get_str("operator")?;

        let op = match operator.as_str() {
            "++" => UpdOp::Increment,
            "--" => UpdOp::Decrement,
            _ => {
                return Err(ParseError::UnexpectedValue {
                    want: "++ or --",
                    value: source.to_error(),
                })
            }
        };
        Ok(UpdateExpression(op, prefix, argument))
    }
}

impl ParseFrom for SequenceExpression {
    fn parse_from<S: SourceNode>(source: &S, ctx: &mut ParserContext) -> ParseResult<Self> {
        let exprs = source.map_array("expressions", |jexpr| Expression::parse_from(jexpr, ctx))?;
        Ok(SequenceExpression(exprs))
    }
}

impl ParseFrom for BinaryExpression {
    fn parse_from<S: SourceNode>(source: &S, ctx: &mut ParserContext) -> ParseResult<Self> {
        let left = source.map_node("left", |jleft| Expression::parse_from(jleft, ctx))?;
        let right = source.map_node("right", |jright| Expression::parse_from(jright, ctx))?;

        let opstr = source.get_str("operator")?;
        let op = match opstr.as_str() {
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
                    value: source.map_node("operator", |jop| Ok(jop.to_error()))?,
                })
            }
        };
        Ok(BinaryExpression(left, op, right))
    }
}

impl ParseFrom for LogicalExpression {
    fn parse_from<S: SourceNode>(source: &S, ctx: &mut ParserContext) -> ParseResult<Self> {
        let opstr = source.get_str("operator")?;
        let op = match opstr.as_str() {
            "&&" => BoolOp::And,
            "||" => BoolOp::Or,
            _ => {
                return Err(ParseError::UnexpectedValue {
                    want: "&& or ||",
                    value: source.map_node("operator", |jop| Ok(jop.to_error()))?,
                })
            }
        };
        let left = source.map_node("left", |jleft| Expression::parse_from(jleft, ctx))?;
        let right = source.map_node("right", |jright| Expression::parse_from(jright, ctx))?;

        Ok(LogicalExpression(left, op, right))
    }
}

impl ParseFrom for AssignmentExpression {
    fn parse_from<S: SourceNode>(source: &S, ctx: &mut ParserContext) -> ParseResult<Self> {
        let jop = source.get_str("operator")?;
        let modop = match jop.as_str() {
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
                    value: source.map_node("operator", |jop| Ok(jop.to_error()))?,
                })
            }
        };

        let right = source.map_node("right", |jright| Expression::parse_from(jright, ctx))?;
        let left = source.map_node("left", |jleft| Expression::parse_from(jleft, ctx))?;

        Ok(AssignmentExpression(left, AssignOp(modop), right))
    }
}

impl ParseFrom for ObjectExpression {
    fn parse_from<S: SourceNode>(source: &S, ctx: &mut ParserContext) -> ParseResult<Self> {
        source.expect_str("type", "ObjectExpression")?;

        let properties = source.map_array("properties", |jprop| {
            jprop.expect_str("type", "Property")?;

            let keyexpr = jprop.map_node("key", |jkey| Expression::parse_from(jkey, ctx))?;
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
                            value: jprop.to_error(),
                        })
                    }
                }
            };

            let value = jprop.map_node("value", |jval| Expression::parse_from(jval, ctx))?;

            Ok((key, value))
        })?;

        Ok(ObjectExpression(properties))
    }
}

impl ParseFrom for FunctionExpression {
    fn parse_from<S: SourceNode>(source: &S, ctx: &mut ParserContext) -> ParseResult<Self> {
        let id: Option<Identifier> =
            source.map_opt_node("id", |jid| Identifier::parse_from(jid, ctx))?;

        let mut inner_ctx = ParserContext::new();
        let params = source.map_array("params", |jparam| {
            Identifier::parse_from(jparam, &mut inner_ctx)
        })?;

        let body = source.map_node("body", |jbody| {
            BlockStatement::parse_from(jbody, &mut inner_ctx)
        })?;

        let ParserContext {
            used_identifiers: mut free_variables,
            declared_variables: variables,
            declared_functions: functions,
            declared_bindings,
        } = inner_ctx;
        assert!(declared_bindings.is_empty());

        free_variables.remove(&Identifier::from("arguments"));
        for var in params.iter().chain(variables.iter()) {
            free_variables.remove(var);
        }

        ctx.used_identifiers.extend(free_variables.iter().cloned());

        let func = Function {
            id,
            params,
            variables,
            functions,
            free_variables,
            body,
            is_generator: source.get_bool("generator").unwrap_or(false),
            is_expression: source.get_bool("expression").unwrap_or(false),
            is_async: source.get_bool("async").unwrap_or(false),
        };
        Ok(FunctionExpression {
            func: Rc::new(func),
        })
    }
}
