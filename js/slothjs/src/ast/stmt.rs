use crate::prelude::*;

use crate::source;

use super::expr::{
    Expression,
    FunctionExpression,
    Identifier,
    Pattern,
};

#[derive(Clone, Debug)]
pub struct Statement {
    pub stmt: Stmt,
    pub loc: Option<Box<source::Location>>,
}

impl Statement {
    pub fn with_loc(self, loc: source::Location) -> Self {
        Statement {
            stmt: self.stmt,
            loc: Some(Box::new(loc)),
        }
    }
}

impl PartialEq for Statement {
    fn eq(&self, other: &Self) -> bool {
        self.stmt == other.stmt
    }
}

impl Eq for Statement {}

impl<T> From<T> for Statement
where
    Stmt: From<T>,
{
    fn from(stmt: T) -> Self {
        Statement {
            stmt: Stmt::from(stmt),
            loc: None,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Stmt {
    Empty,
    Block(BlockStatement),
    Expr(ExpressionStatement),
    If(Box<IfStatement>),
    Switch(SwitchStatement),
    For(Box<ForStatement>),
    ForIn(Box<ForInStatement>),
    Return(ReturnStatement),
    Break(BreakStatement),
    Continue(ContinueStatement),
    Label(Box<LabelStatement>),
    Throw(ThrowStatement),
    Try(TryStatement),

    // TODO: move declarations out?
    Variable(VariableDeclaration),
    Function(FunctionDeclaration),
}

impl<E> From<E> for Stmt
where
    Expression: From<E>,
{
    fn from(expr: E) -> Stmt {
        Stmt::Expr(ExpressionStatement {
            expression: Expression::from(expr),
        })
    }
}

impl From<VariableDeclaration> for Stmt {
    fn from(var: VariableDeclaration) -> Stmt {
        Stmt::Variable(var)
    }
}

impl From<BlockStatement> for Stmt {
    fn from(block: BlockStatement) -> Stmt {
        Stmt::Block(block)
    }
}

impl From<ReturnStatement> for Stmt {
    fn from(ret: ReturnStatement) -> Stmt {
        Stmt::Return(ret)
    }
}

// ==============================================
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ExpressionStatement {
    pub expression: Expression,
}

// ==============================================
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VariableDeclarator {
    pub name: Pattern,
    pub init: Option<Box<Expression>>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum DeclarationKind {
    Var,
    Let,
    Const,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VariableDeclaration {
    pub kind: DeclarationKind,
    pub declarations: Vec<VariableDeclarator>,
}

// ==============================================
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FunctionDeclaration {
    pub id: Identifier, // might be different from function.id
    pub function: FunctionExpression,
}

// ==============================================
#[derive(Clone, Debug)]
pub struct BlockStatement {
    pub body: Vec<Statement>,
    pub bindings: HashSet<Identifier>,
}

impl PartialEq for BlockStatement {
    fn eq(&self, other: &Self) -> bool {
        self.body == other.body
    }
}
impl Eq for BlockStatement {}

impl From<Vec<Statement>> for BlockStatement {
    fn from(body: Vec<Statement>) -> BlockStatement {
        BlockStatement {
            body,
            bindings: HashSet::new(),
        } // TODO: bindings analysis
    }
}

// ==============================================
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IfStatement {
    pub test: Expression,
    pub consequent: Statement,
    pub alternate: Option<Statement>,
}

// ==============================================
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SwitchStatement {
    pub discriminant: Expression,
    pub cases: Vec<SwitchCase>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SwitchCase {
    pub test: Option<Expression>,
    pub consequent: Vec<Statement>,
}

// ==============================================
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ForStatement {
    pub init: Statement, // Empty | VariableDeclaration | ExpressionStatement
    pub test: Option<Expression>,
    pub update: Option<Expression>,
    pub body: Statement,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ForInStatement {
    pub left: ForInTarget,
    pub right: Expression,
    pub body: Statement,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ForInTarget {
    Var(VariableDeclaration),
    Expr(Expression),
}

// ==============================================
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BreakStatement(pub Option<Identifier>);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ContinueStatement(pub Option<Identifier>);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LabelStatement(pub Identifier, pub Statement);

// ==============================================
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ReturnStatement(pub Option<Expression>);

// ==============================================
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ThrowStatement(pub Expression);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TryStatement {
    pub block: BlockStatement,
    pub handler: Option<CatchClause>,
    pub finalizer: Option<BlockStatement>,
}

// ==============================================
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CatchClause {
    pub param: Pattern,
    pub body: BlockStatement,
}

/// make a [`BlockStatement`]
pub fn block(body: Vec<Statement>) -> BlockStatement {
    BlockStatement::from(body)
}

/// make a [`VariableDeclaration`] from [(var1, value1), ...]
pub fn var<'a>(it: impl Iterator<Item = &'a (&'a str, Expression)>) -> VariableDeclaration {
    let declarations = it
        .map(|(name, init)| VariableDeclarator {
            name: Identifier::from(*name),
            init: Some(Box::new(init.clone())),
        })
        .collect();
    VariableDeclaration {
        kind: DeclarationKind::Var,
        declarations,
    }
}

/// make an [`ExpressionStatement`](`expr`)
pub fn expr(expr: Expression) -> Statement {
    Statement::from(expr)
}

/// make a [`ReturnStatement`](`expr`)
pub fn return_(expr: Expression) -> Statement {
    Statement::from(ReturnStatement(Some(expr)))
}
