use crate::object::JSON;

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
    For(Box<ForStatement>),

    // TODO: move declarations out?
    VariableDeclaration(VariableDeclaration),
}

#[derive(Debug, Clone)]
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

// ==============================================
pub struct ForStatement {
    // Empty | VariableDeclaration | ExpressionStatement
    pub init: Statement,
    pub test: Option<Expr>,
    pub update: Option<Expr>,
    pub body: Statement,
}


// ==============================================
#[derive(Debug, Clone)]
pub enum Expr {
    Literal(Literal),
    Identifier(Identifier),
    BinaryOp(BinaryExpression),
    Call(CallExpression),
    Array(ArrayExpression),
    Object(ObjectExpression),
    Member(MemberExpression),
    Assign(AssignmentExpression),
    Conditional(ConditionalExpression),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Literal(pub JSON);

#[derive(Clone, Debug, PartialEq)]
pub struct Identifier(pub String);

#[derive(Clone, Debug)]
pub struct BinaryExpression(pub Box<Expr>, pub BinOp, pub Box<Expr>);

#[derive(Clone, Debug)]
pub struct CallExpression(pub Box<Expr>, pub Vec<Box<Expr>>);

#[derive(Clone, Debug)]
pub struct ArrayExpression(pub Vec<Box<Expr>>);

#[derive(Clone, Debug)]
pub struct ObjectExpression(pub Vec<(ObjectKey, Box<Expr>)>);

#[derive(Clone, Debug)]
pub struct MemberExpression(pub Box<Expr>, pub Box<Expr>, pub bool);

#[derive(Clone, Debug)]
pub struct AssignmentExpression(pub Box<Expr>, pub AssignOp, pub Box<Expr>);

#[derive(Clone, Debug)]
pub struct ConditionalExpression(pub Box<Expr>, pub Box<Expr>, pub Box<Expr>);

#[derive(Clone, Debug)]
pub enum BinOp {
    Plus,
    EqEq,
    NotEq,
    Less,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AssignOp {
    Equal,
}

