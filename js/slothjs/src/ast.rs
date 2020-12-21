use crate::value::JSValue;

// ==============================================
pub struct Program {
    pub body: Vec<Statement>,   // TODO: Either<Statement, Directive>
}

// ==============================================
pub enum Statement {
    Empty,
    Block(BlockStatement),
    Expression(ExpressionStatement),

    // TODO: move declarations out?
    VariableDeclaration(VariableDeclaration),
}

// ==============================================
pub enum Expr {
    Literal(JSValue),
    Identifier(String),
    BinaryOp(Box<Expr>, BinOp, Box<Expr>),
    Call(Box<Expr>, Vec<Box<Expr>>),
    Array(Vec<Box<Expr>>),
    Object(Vec<(ObjectKey, Box<Expr>)>),
    Member(Box<Expr>, Box<Expr>, bool),
}

pub enum BinOp {
    Add,
    KindaEqual
}

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
