use crate::object::JSON;

// ==============================================
pub struct Program {
    pub body: Vec<Statement>, // TODO: Either<Statement, Directive>
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
    Break(BreakStatement),
    Continue(ContinueStatement),
    Label(LabelStatement),
    Throw(ThrowStatement),
    Try(TryStatement),

    // TODO: move declarations out?
    VariableDeclaration(VariableDeclaration),
    FunctionDeclaration(FunctionDeclaration),
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
    pub name: Pattern,
    pub init: Option<Box<Expr>>,
}

#[derive(Copy, Clone, Debug)]
pub enum DeclarationKind {
    Var,
    Let,
    Const,
}

#[derive(Clone, Debug)]
pub struct VariableDeclaration {
    pub kind: DeclarationKind,
    pub declarations: Vec<VariableDeclarator>,
}

// ==============================================
#[derive(Clone, Debug)]
pub struct FunctionDeclaration {
    pub id: Identifier, // might be different from function.id
    pub function: FunctionExpression,
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
pub struct BreakStatement(pub Option<Identifier>);

#[derive(Clone, Debug)]
pub struct ContinueStatement(pub Option<Identifier>);

#[derive(Clone, Debug)]
pub struct LabelStatement(pub Identifier, pub Box<Statement>);

// ==============================================
#[derive(Clone, Debug)]
pub struct ReturnStatement(pub Option<Expr>);

// ==============================================
#[derive(Clone, Debug)]
pub struct ThrowStatement(pub Expr);

#[derive(Clone, Debug)]
pub struct TryStatement {
    pub block: BlockStatement,
    pub handler: Option<CatchClause>,
    pub finalizer: Option<BlockStatement>,
}

#[derive(Clone, Debug)]
pub struct CatchClause {
    pub param: Pattern,
    pub body: BlockStatement,
}

// ==============================================
#[derive(Debug, Clone)]
pub enum Expr {
    Literal(Literal),
    Identifier(Identifier),
    BinaryOp(BinaryExpression),
    LogicalOp(LogicalExpression),
    Call(CallExpression),
    Array(ArrayExpression),
    Object(ObjectExpression),
    Member(MemberExpression),
    Assign(AssignmentExpression),
    Conditional(ConditionalExpression),
    Unary(UnaryExpression),
    Update(UpdateExpression),
    Function(FunctionExpression),
    This,
    New(NewExpression),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Literal(pub JSON);

#[derive(Clone, Debug, PartialEq)]
pub struct Identifier(pub String);

impl From<&str> for Identifier {
    fn from(s: &str) -> Identifier {
        Identifier(s.to_string())
    }
}

#[derive(Clone, Debug)]
pub struct BinaryExpression(pub Box<Expr>, pub BinOp, pub Box<Expr>);

#[derive(Clone, Debug)]
pub struct LogicalExpression(pub Box<Expr>, pub BoolOp, pub Box<Expr>);

#[derive(Clone, Debug)]
pub struct UnaryExpression(pub UnOp, pub Box<Expr>);

#[derive(Clone, Debug)]
pub struct UpdateExpression(pub UpdOp, pub bool, pub Box<Expr>);

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
pub struct FunctionExpression {
    pub id: Option<Identifier>,
    pub params: Vec<Pattern>,
    pub body: Box<BlockStatement>,
    pub is_generator: bool,
    pub is_expression: bool,
    pub is_async: bool,
}

// TODO: enum { AssignmentPattern, Identifier, BindingPattern }
pub type Pattern = Identifier;

#[derive(Clone, Debug)]
pub struct NewExpression(pub Box<Expr>, pub Vec<Box<Expr>>);

#[derive(Clone, Debug, PartialEq)]
pub enum BinOp {
    Plus,
    Minus,
    Star,
    EqEq,
    NotEq,
    EqEqEq,
    NotEqEq,
    Less,
    Greater,
    LtEq,
    GtEq,
    InstanceOf,
    /*
    In,
    Slash,
    Percent,
    StarStar,
    Pipe,
    Hat,
    Ampersand,
    LtLt,
    GtGt,
    GtGtGt
     */
}

#[derive(Clone, Debug, PartialEq)]
pub enum BoolOp {
    And,
    Or,
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

#[derive(Debug, Clone, PartialEq)]
pub enum UpdOp {
    Increment,
    Decrement,
}
