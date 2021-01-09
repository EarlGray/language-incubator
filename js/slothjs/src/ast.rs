use crate::object::JSON;

// ==============================================
pub struct Program {
    pub body: Vec<Statement>,   // TODO: Either<Statement, Directive>
}

// ==============================================
#[derive(Clone, Debug)]
pub enum Statement {
    Empty,
    Block(BlockStatement),
    Expr(ExpressionStatement),
    If(Box<IfStatement>),
    For(Box<ForStatement>),
    Return(ReturnStatement),

    // TODO: move declarations out?
    VariableDeclaration(VariableDeclaration),
}

#[derive(Debug, Clone)]
pub enum ObjectKey {
    Computed(Expr),
    Identifier(String),
}

// ==============================================
#[derive(Clone, Debug)]
pub struct ExpressionStatement {
    pub expression: Expr,
}

// ==============================================
#[derive(Clone, Debug)]
pub struct VariableDeclarator {
    pub name: String,
    pub init: Option<Box<Expr>>,
}

#[derive(Copy, Clone, Debug)]
pub enum DeclarationKind { Var, Let, Const }

#[derive(Clone, Debug)]
pub struct VariableDeclaration {
    pub kind: DeclarationKind,
    pub declarations: Vec<VariableDeclarator>,
}

// ==============================================
#[derive(Clone, Debug)]
pub struct BlockStatement {
    pub body: Vec<Statement>,
}

// ==============================================
#[derive(Clone, Debug)]
pub struct IfStatement {
    pub test: Expr,
    pub consequent: Statement,
    pub alternate: Option<Statement>,
}

// ==============================================
#[derive(Clone, Debug)]
pub struct ForStatement {
    // Empty | VariableDeclaration | ExpressionStatement
    pub init: Statement,
    pub test: Option<Expr>,
    pub update: Option<Expr>,
    pub body: Statement,
}

// ==============================================
#[derive(Clone, Debug)]
pub struct ReturnStatement(pub Option<Expr>);

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
    Unary(UnaryExpression),
    Function(FunctionExpression),
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
pub struct UnaryExpression(pub UnOp, pub Box<Expr>);

#[derive(Clone, Debug)]
pub struct FunctionExpression {
    pub id: Option<Identifier>,
    pub params: Vec<FunctionParameter>,
    pub body: Box<BlockStatement>,
    pub generator: bool,
    pub expression: bool,
    pub is_async: bool,
}

// TODO: enum { AssignmentPattern, Identifier, BindingPattern }
pub type FunctionParameter = Identifier;

#[derive(Clone, Debug, PartialEq)]
pub enum BinOp {
    Plus,
    Minus,
    Star,
    EqEq,
    NotEq,
    Less,
    /*
    InstanceOf,
    In,
    Slash,
    Percent,
    StarStar,
    Pipe,
    Hat,
    Ampersand,
    EqEqEq,
    NotEqEq,
    Greater,
    LtEq,
    LtLt,
    GtGt,
    GtGtGt
     */
}

#[derive(Debug, Clone, PartialEq)]
pub struct AssignOp(pub Option<BinOp>);

#[derive(Debug, Clone, PartialEq)]
pub enum UnOp {
    Exclamation,
    Minus,
    Plus,
    Tilde,
    Typeof,
    Void,
    Delete,
}
