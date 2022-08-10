//! AST definitions for JavaScript expressions.
//!
//! The main struct here is [`Expression`], which wraps [`Expr`] enum.

use crate::prelude::*;

use crate::{
    source,
    JSON,
};

use super::stmt::{
    BlockStatement,
    FunctionDeclaration,
};

/// `Expression` represents an [`Expr`] together with its source span, if any.
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

/// The enumeration of every possible kind of JS expressions.
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

impl From<MemberExpression> for Expr {
    fn from(membexpr: MemberExpression) -> Expr {
        Expr::Member(Box::new(membexpr))
    }
}

impl From<CallExpression> for Expr {
    fn from(callexpr: CallExpression) -> Expr {
        Expr::Call(Box::new(callexpr))
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Literal(pub JSON);  // TODO: change to JSValue

impl<V> From<V> for Literal
where
    JSON: From<V>,
{
    fn from(val: V) -> Literal {
        Literal(JSON::from(val))
    }
}

impl<V> From<V> for Expr
where
    JSON: From<V>,
{
    fn from(val: V) -> Expr {
        Expr::Literal(Literal::from(val))
    }
}

#[derive(Clone, Debug, PartialEq, Hash, Eq)]
pub struct Identifier(pub JSString);

impl Identifier {
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

impl From<&str> for Identifier {
    fn from(s: &str) -> Identifier {
        Identifier(s.into())
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

/// Describes an [`ObjectExpression`] key: `ObjectKey::Computed` or `ObjectKey::Identifier`
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ObjectKey {
    Computed(Expression),
    Identifier(JSString),
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
pub struct AssignmentExpression(pub Expression, pub Option<BinOp>, pub Expression);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ConditionalExpression {
    pub condexpr: Expression,
    pub thenexpr: Expression,
    pub elseexpr: Expression,
}

/// `Function` describes a JS function definition (`params`, `body`, etc).
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

/// Lists all possible binary operation for [`BinaryExpression`]
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

/// Lists all boolean operations (`&&`, `||`) for [`LogicalExpression`]
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BoolOp {
    And,
    Or,
}

/// Lists all unary operations for [`UnaryExpression`]
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

/// Lists all update operations (`++`, `--`) for [`UpdateExpression`]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UpdOp {
    Increment,
    Decrement,
}

/// make a [`Literal`] (`JSON::Null`) (JS: `null`).
pub fn null() -> Expression {
    lit(JSON::Null)
}

/// make an [`Identifier`] (`"undefined"`), (JS: `undefined`)
pub fn undefined() -> Expression {
    id("undefined")
}

/// make a [`Literal`] from `value`
///
///  - `lit(2)` is `2` in JavaScript
///  - `lit(true)` is `true` in JavaScript
///  - ...
///
/// DO NOT USE this for arrays and objects, use [`array()`] and [`object()`] instead!
pub fn lit<V>(value: V) -> Expression
where
    JSON: From<V>,
{
    Expression::from(Literal(JSON::from(value)))
}

/// make an [`Identifier`] from `name` (JS: `name`)
pub fn id(name: &str) -> Expression {
    Expression::from(Identifier::from(name))
}

/// make an [`ArrayExpression`] (`vec![v1, v2, ...]`) (JS: `[v1, v2, ...]`)
pub fn array<E>(exprs: Vec<E>) -> Expression
where
    Expression: From<E>,
{
    let exprs = exprs.into_iter().map(Expression::from).collect();
    let expr = Expr::Array(ArrayExpression(exprs));
    Expression { expr, loc: None }
}

/// make an empty [`ArrayExpression`], JS: `[]`
pub fn empty_array() -> Expression {
    array::<Expression>(vec![])
}

/// make a [`ObjectExpression`] (`vec![(k1, v1), ...]`) (JS: `{k1: v1, ...}`)
pub fn object<K>(pairs: Vec<(K, Expression)>) -> Expression
where
    ObjectKey: From<K>,
{
    let pairs = (pairs.into_iter())
        .map(|(k, v)| (ObjectKey::from(k), v))
        .collect();
    Expression::from(ObjectExpression(pairs))
}

/// make an empty [`ObjectExpression`] (JS: `{}`)
pub fn empty_object() -> Expression {
    object::<Identifier>(vec![])
}

/// make a [`UnaryExpression`]([`UnOp::Plus`], `expr`)
pub fn plus<E>(expr: E) -> Expression
where
    Expression: From<E>,
{
    let expr = Expression::from(expr);
    let expr = Expr::Unary(Box::new(UnaryExpression(UnOp::Plus, expr)));
    Expression { expr, loc: None }
}

/// make a [`BinaryExpression`](`left`, `op`, `right`)
pub fn binary<E1, E2>(op: BinOp, left: E1, right: E2) -> Expression
where
    Expression: From<E1> + From<E2>,
{
    let left = Expression::from(left);
    let right = Expression::from(right);
    Expression::from(BinaryExpression(left, op, right))
}

/// make a [`BinaryExpression`] (`left`, [`BinOp::Plus`], `right`)
pub fn add<E1, E2>(left: E1, right: E2) -> Expression
where
    Expression: From<E1> + From<E2>,
{
    binary(BinOp::Plus, Expression::from(left), Expression::from(right))
}

/// make a non-computed (i.e. `object.attr`) [`MemberExpression`](`object`, `attr`)
pub fn memb<E, I>(object: E, attr: I) -> Expression
where
    Expression: From<E>,
    Identifier: From<I>,
{
    let object = Expression::from(object);
    let attr = Identifier::from(attr);
    let attr = Expression {
        expr: Expr::Identifier(attr),
        loc: None,
    };
    MemberExpression(object, attr, false).into()
}

/// make a computed  [`MemberExpression`](`object`, `attr`) (JS: `object[attr]`)
pub fn index<E>(object: E, attr: Expression) -> Expression
where
    Expression: From<E>,
{
    let object = Expression::from(object);
    MemberExpression(object, attr, true).into()
}

/// make a [`CallExpression`] with `callee` and `arguments` (JS: `callee(arguments...)`)
pub fn call<E>(callee: E, arguments: Vec<Expression>) -> Expression
where
    Expression: From<E>,
{
    let callee = Expression::from(callee);
    CallExpression(callee, arguments).into()
}
