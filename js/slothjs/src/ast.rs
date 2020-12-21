use crate::value::JSValue;

// ==============================================
pub struct Program {
    pub body: Vec<Statement>,   // TODO: Either<Statement, Directive>
}

// ==============================================
pub enum Statement {
    Empty,
    Block(BlockStatement),
    Expr(ExpressionStatement),
    If(Box<IfStatement>),

    // TODO: move declarations out?
    VariableDeclaration(VariableDeclaration),
}

// ==============================================
#[derive(Debug)]
pub enum Expr {
    Literal(JSValue),
    Identifier(String),
    BinaryOp(Box<Expr>, BinOp, Box<Expr>),
    Call(Box<Expr>, Vec<Box<Expr>>),
    Array(Vec<Box<Expr>>),
    Object(Vec<(ObjectKey, Box<Expr>)>),
    Member(Box<Expr>, Box<Expr>, bool),
    Assign(Box<Expr>, AssignOp, Box<Expr>),
    Conditional(Box<Expr>, Box<Expr>, Box<Expr>),
}

#[derive(Debug)]
pub enum BinOp {
    Plus,
    EqEq,
}

#[derive(Debug)]
pub enum AssignOp {
    Equal,
}

#[derive(Debug)]
pub enum ObjectKey {
    Computed(Expr),
    Identifier(String),
}

// ==============================================
pub struct ExpressionStatement {
    pub expression: Expr,
}

// ==============================================
pub struct VariableDeclarator {
    pub name: String,
    pub init: Option<Box<Expr>>,
}

pub enum DeclarationKind { Var, Let, Const }

pub struct VariableDeclaration {
    pub _kind: DeclarationKind,
    pub declarations: Vec<VariableDeclarator>,
}

// ==============================================
pub struct BlockStatement {
    pub body: Vec<Statement>,
}

// ==============================================
pub struct IfStatement {
    pub test: Expr,
    pub consequent: Statement,
    pub alternate: Option<Statement>,
}
