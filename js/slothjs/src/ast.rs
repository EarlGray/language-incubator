use std::collections::HashSet;
use std::rc::Rc;

use crate::object::JSON;
use crate::source;

// ==============================================

// ==============================================
pub struct Program {
    pub body: Vec<Statement>,           // TODO: Either<Statement, Directive>
    pub variables: HashSet<Identifier>, // The set of scope variables
    pub functions: Vec<FunctionDeclaration>,
}

// ==============================================
#[derive(Clone, Debug)]
pub struct Statement {
    pub stmt: Stmt,
    pub loc: Option<Box<source::Location>>,
}

#[derive(Clone, Debug)]
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

#[derive(Debug, Clone)]
pub enum ObjectKey {
    Computed(Expression),
    Identifier(String),
}

// ==============================================
#[derive(Clone, Debug)]
pub struct ExpressionStatement {
    pub expression: Expression,
}

// ==============================================
#[derive(Clone, Debug)]
pub struct VariableDeclarator {
    pub name: Pattern,
    pub init: Option<Box<Expression>>,
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
    pub test: Expression,
    pub consequent: Statement,
    pub alternate: Option<Statement>,
}

// ==============================================
#[derive(Clone, Debug)]
pub struct SwitchStatement {
    pub discriminant: Expression,
    pub cases: Vec<SwitchCase>,
}

#[derive(Clone, Debug)]
pub struct SwitchCase {
    pub test: Option<Expression>,
    pub consequent: Vec<Statement>,
}

// ==============================================
#[derive(Clone, Debug)]
pub struct ForStatement {
    pub init: Statement, // Empty | VariableDeclaration | ExpressionStatement
    pub test: Option<Expression>,
    pub update: Option<Expression>,
    pub body: Statement,
}

#[derive(Clone, Debug)]
pub struct ForInStatement {
    pub left: ForInTarget,
    pub right: Expression,
    pub body: Statement,
}

#[derive(Clone, Debug)]
pub enum ForInTarget {
    Var(VariableDeclaration),
    Expr(Expression),
}

// ==============================================
#[derive(Clone, Debug)]
pub struct BreakStatement(pub Option<Identifier>);

#[derive(Clone, Debug)]
pub struct ContinueStatement(pub Option<Identifier>);

#[derive(Clone, Debug)]
pub struct LabelStatement(pub Identifier, pub Statement);

// ==============================================
#[derive(Clone, Debug)]
pub struct ReturnStatement(pub Option<Expression>);

// ==============================================
#[derive(Clone, Debug)]
pub struct ThrowStatement(pub Expression);

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
pub struct Expression {
    pub expr: Expr,
    pub loc: Option<Box<source::Location>>,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(Literal),
    Identifier(Identifier),
    BinaryOp(Box<BinaryExpression>),
    LogicalOp(Box<LogicalExpression>),
    Call(Box<CallExpression>),
    Array(ArrayExpression),
    Object(ObjectExpression),
    Member(Box<MemberExpression>),
    Assign(Box<AssignmentExpression>),
    Conditional(Box<ConditionalExpression>),
    Unary(UnaryExpression),
    Update(Box<UpdateExpression>),
    Sequence(SequenceExpression),
    Function(FunctionExpression),
    This,
    New(Box<NewExpression>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Literal(pub JSON);

#[derive(Clone, Debug, PartialEq, Hash, Eq)]
pub struct Identifier(pub String);

impl Identifier {
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl From<&str> for Identifier {
    fn from(s: &str) -> Identifier {
        Identifier(s.to_string())
    }
}

#[derive(Clone, Debug)]
pub struct BinaryExpression(pub Expression, pub BinOp, pub Expression);

#[derive(Clone, Debug)]
pub struct LogicalExpression(pub Expression, pub BoolOp, pub Expression);

#[derive(Clone, Debug)]
pub struct UnaryExpression(pub UnOp, pub Box<Expression>);

#[derive(Clone, Debug)]
pub struct UpdateExpression(pub UpdOp, pub bool, pub Expression);

#[derive(Clone, Debug)]
pub struct CallExpression(pub Expression, pub Vec<Expression>);

#[derive(Clone, Debug)]
pub struct ArrayExpression(pub Vec<Expression>);

#[derive(Clone, Debug)]
pub struct ObjectExpression(pub Vec<(ObjectKey, Expression)>);

#[derive(Clone, Debug)]
pub struct MemberExpression(pub Expression, pub Expression, pub bool);

#[derive(Clone, Debug)]
pub struct SequenceExpression(pub Vec<Expression>);

#[derive(Clone, Debug)]
pub struct AssignmentExpression(pub Expression, pub AssignOp, pub Expression);

#[derive(Clone, Debug)]
pub struct ConditionalExpression {
    pub condexpr: Expression,
    pub thenexpr: Expression,
    pub elseexpr: Expression,
}

#[derive(Clone, Debug)]
pub struct Function {
    pub id: Option<Identifier>,
    pub params: Vec<Pattern>,           // cannot be a HashSet, needs order
    pub variables: HashSet<Identifier>, // the set of local variables
    pub functions: Vec<FunctionDeclaration>, // the set of declared functions
    pub free_variables: HashSet<Identifier>,
    pub body: BlockStatement,
    pub is_generator: bool,
    pub is_expression: bool,
    pub is_async: bool,
}

#[derive(Clone, Debug)]
pub struct FunctionExpression {
    pub func: Rc<Function>,
}

// TODO: enum { AssignmentPattern, Identifier, BindingPattern }
pub type Pattern = Identifier;

#[derive(Clone, Debug)]
pub struct NewExpression(pub Expression, pub Vec<Expression>);

#[derive(Clone, Debug, PartialEq)]
pub enum BinOp {
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    EqEq,
    NotEq,
    EqEqEq,
    NotEqEq,
    Less,
    Greater,
    LtEq,
    GtEq,
    Pipe,
    Hat,
    Ampersand,
    LtLt,
    GtGt,
    GtGtGt,
    In,
    InstanceOf,
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
