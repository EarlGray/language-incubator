use crate::prelude::*;

use crate::{
    source,
    JSON,
};

use super::stmt::{
    BlockStatement,
    FunctionDeclaration,
};

#[derive(Debug, Clone)]
pub struct Expression {
    pub expr: Expr,
    pub loc: Option<Box<source::Location>>,
}

impl Expression {
    pub fn with_loc(self, loc: source::Location) -> Self {
        Expression {
            expr: self.expr,
            loc: Some(Box::new(loc)),
        }
    }
}

impl PartialEq for Expression {
    fn eq(&self, other: &Self) -> bool {
        self.expr == other.expr
    }
}
impl Eq for Expression {}

impl<E> From<E> for Expression
where
    Expr: From<E>,
{
    fn from(expr: E) -> Expression {
        let expr = Expr::from(expr);
        Expression { expr, loc: None }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
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
    Unary(Box<UnaryExpression>),
    Update(Box<UpdateExpression>),
    Sequence(SequenceExpression),
    Function(FunctionExpression),
    This,
    New(Box<NewExpression>),
}

impl From<Literal> for Expr {
    fn from(lit: Literal) -> Expr {
        Expr::Literal(lit)
    }
}

impl From<Identifier> for Expr {
    fn from(id: Identifier) -> Expr {
        Expr::Identifier(id)
    }
}

impl From<BinaryExpression> for Expr {
    fn from(binary: BinaryExpression) -> Expr {
        Expr::BinaryOp(Box::new(binary))
    }
}

impl From<UnaryExpression> for Expr {
    fn from(unary: UnaryExpression) -> Expr {
        Expr::Unary(Box::new(unary))
    }
}

impl From<ArrayExpression> for Expr {
    fn from(arrexpr: ArrayExpression) -> Expr {
        Expr::Array(arrexpr)
    }
}

impl From<ObjectExpression> for Expr {
    fn from(objexpr: ObjectExpression) -> Expr {
        Expr::Object(objexpr)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Literal(pub JSON);

impl<V> From<V> for Expr
where
    JSON: From<V>,
{
    fn from(val: V) -> Expr {
        let json = JSON::from(val);
        Expr::Literal(Literal(json))
    }
}

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

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BinaryExpression(pub Expression, pub BinOp, pub Expression);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LogicalExpression(pub Expression, pub BoolOp, pub Expression);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct UnaryExpression(pub UnOp, pub Expression);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct UpdateExpression(pub UpdOp, pub bool, pub Expression);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CallExpression(pub Expression, pub Vec<Expression>);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ArrayExpression(pub Vec<Expression>);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ObjectExpression(pub Vec<(ObjectKey, Expression)>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ObjectKey {
    Computed(Expression),
    Identifier(String),
}

impl<E> From<E> for ObjectKey
where
    Expression: From<E>,
{
    fn from(expr: E) -> Self {
        let expr = Expression::from(expr);
        match expr.expr {
            Expr::Identifier(id) => ObjectKey::Identifier(id.0),
            _ => ObjectKey::Computed(expr),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MemberExpression(pub Expression, pub Expression, pub bool);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SequenceExpression(pub Vec<Expression>);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AssignmentExpression(pub Expression, pub AssignOp, pub Expression);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ConditionalExpression {
    pub condexpr: Expression,
    pub thenexpr: Expression,
    pub elseexpr: Expression,
}

#[derive(Clone, Debug, PartialEq, Eq)]
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FunctionExpression {
    pub func: Rc<Function>,
}

// TODO: enum { AssignmentPattern, Identifier, BindingPattern }
pub type Pattern = Identifier;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NewExpression(pub Expression, pub Vec<Expression>);

#[derive(Clone, Debug, PartialEq, Eq)]
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BoolOp {
    And,
    Or,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AssignOp(pub Option<BinOp>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnOp {
    Exclamation,
    Minus,
    Plus,
    Tilde,
    Typeof,
    Void,
    Delete,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UpdOp {
    Increment,
    Decrement,
}

pub fn null() -> Expression {
    lit(JSON::Null)
}
pub fn undefined() -> Expression {
    id("undefined")
}

pub fn lit<V>(value: V) -> Expression
where
    JSON: From<V>,
{
    Expression::from(Literal(JSON::from(value)))
}

pub fn id(name: &str) -> Expression {
    Expression::from(Identifier::from(name))
}

pub fn array<E>(exprs: Vec<E>) -> Expression
where
    Expression: From<E>,
{
    let exprs = exprs.into_iter().map(Expression::from).collect();
    let expr = Expr::Array(ArrayExpression(exprs));
    Expression { expr, loc: None }
}

pub fn empty_array() -> Expression {
    array::<Expression>(vec![])
}

pub fn object<K>(pairs: Vec<(K, Expression)>) -> Expression
where
    ObjectKey: From<K>,
{
    let pairs = pairs
        .into_iter()
        .map(|(k, v)| (ObjectKey::from(k), v))
        .collect();
    Expression::from(ObjectExpression(pairs))
}

pub fn empty_object() -> Expression {
    object::<Identifier>(vec![])
}

/// [`UnaryExpression`]([`UnOp::Plus`], expr)
pub fn plus<E>(expr: E) -> Expression
where
    Expression: From<E>,
{
    let expr = Expression::from(expr);
    let expr = Expr::Unary(Box::new(UnaryExpression(UnOp::Plus, expr)));
    Expression { expr, loc: None }
}

/// [`BinaryExpression`](left, [`BinOp::Plus`], right)
pub fn add<E1, E2>(left: E1, right: E2) -> Expression
where
    Expression: From<E1>,
    Expression: From<E2>,
{
    let left = Expression::from(left);
    let right = Expression::from(right);
    Expression::from(BinaryExpression(left, BinOp::Plus, right))
}
