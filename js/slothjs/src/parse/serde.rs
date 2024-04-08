use serde::{Deserialize, Serialize};

use crate::ast::{BinOp, Literal};
use crate::ast::{BoolOp, Identifier};
use crate::JSString;

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(from = "BinExprSerde", into = "BinExprSerde")]
struct BinaryExpression(Expression, BinOp, Expression);

impl From<BinExprSerde> for BinaryExpression {
    fn from(value: BinExprSerde) -> Self {
        Self(value.left, value.operator, value.right)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct BinExprSerde {
    operator: BinOp,
    left: Expression,
    right: Expression,
}

impl From<BinaryExpression> for BinExprSerde {
    fn from(value: BinaryExpression) -> Self {
        let BinaryExpression(left, operator, right) = value;
        Self {
            operator,
            left,
            right,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(from = "LogicExprSerde")]
#[serde(into = "LogicExprSerde")]
pub struct LogicalExpression(Expression, BoolOp, Expression);

#[derive(Debug, Clone, Serialize, Deserialize)]
struct LogicExprSerde {
    operator: BoolOp,
    left: Expression,
    right: Expression,
}

impl From<LogicalExpression> for LogicExprSerde {
    fn from(value: LogicalExpression) -> Self {
        let LogicalExpression(left, operator, right) = value;
        Self {
            operator,
            left,
            right,
        }
    }
}

impl From<LogicExprSerde> for LogicalExpression {
    fn from(value: LogicExprSerde) -> Self {
        Self(value.left, value.operator, value.right)
    }
}

#[derive(Debug, Clone, PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(from = "CallExprSerde", into = "CallExprSerde")]
struct CallExpression(Expression, Vec<Expression>);

impl From<CallExprSerde> for CallExpression {
    fn from(value: CallExprSerde) -> Self {
        let CallExprSerde{callee, arguments} = value;
        Self(callee, arguments)
    }
}

#[derive(Debug, Clone)]
#[derive(Serialize, Deserialize)]
struct CallExprSerde {
    callee: Expression,
    arguments: Vec<Expression>,
}

impl From<CallExpression> for CallExprSerde {
    fn from(value: CallExpression) -> Self {
        let CallExpression(callee, arguments) = value;
        Self{callee, arguments}
    }
}

impl From<NewExpression> for CallExprSerde {
    fn from(value: NewExpression) -> Self {
        let NewExpression(callee, arguments) = value;
        Self{callee, arguments}
    }
}

#[derive(Clone, Debug, PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(from = "ArrayExprSerde", into = "ArrayExprSerde")]
struct ArrayExpression(Vec<Expression>);

impl From<ArrayExprSerde> for ArrayExpression {
    fn from(value: ArrayExprSerde) -> Self {
        let ArrayExprSerde { elements } = value;
        Self(elements)
    }
}

#[derive(Debug, Clone, PartialEq)]
#[derive(Serialize, Deserialize)]
struct ArrayExprSerde{
    elements: Vec<Expression>,
}

impl From<ArrayExpression> for ArrayExprSerde {
    fn from(value: ArrayExpression) -> Self {
        let ArrayExpression(elements) = value;
        Self{elements}
    }
}

#[derive(Debug, Clone, PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(from = "ObjectExprSerde", into = "ObjectExprSerde")]
struct ObjectExpression(Vec<(ObjectKey, Expression)>);

#[derive(Debug, Clone, PartialEq)]
enum ObjectKey {
    Computed(Expression),
    Identifier(JSString),
}

impl From<ObjectExprSerde> for ObjectExpression {
    fn from(value: ObjectExprSerde) -> Self {
        let properties = value.properties.into_iter().map(|prop| {
            let value = prop.value;
            let key = if prop.computed {
                ObjectKey::Computed(prop.key)
            } else {
                let Expr::Identifier(ident) = prop.key.expr else {
                    panic!()
                };
                ObjectKey::Identifier(ident.0)
            };
            (key, value)
        }).collect();
        Self(properties)
    }
}

#[derive(Debug, Clone, PartialEq)]
#[derive(Serialize, Deserialize)]
struct ObjectExprSerde {
    properties: Vec<PropSerde>,
}

impl From<ObjectExpression> for ObjectExprSerde {
    fn from(value: ObjectExpression) -> Self {
        let properties = value.0.into_iter().map(|(key, value)| {
            let (computed, key) = match key {
                ObjectKey::Computed(expr) =>
                    (true, expr.clone()),
                ObjectKey::Identifier(name) =>
                    (false, Expression::from(Expr::Identifier(Identifier::from(name.as_str())))),
            };
            PropSerde{
                key, value,
                kind: PropKind::Init,
                computed,
                method: false,
                shorthand: false, 
            }
        }).collect();
        Self{properties}
    }
}

#[derive(Debug, Clone, PartialEq)]
#[derive(Serialize, Deserialize)]
struct PropSerde {
    key: Expression,
    value: Expression,
    kind: PropKind,
    computed: bool,
    method: bool,
    shorthand: bool,
}

#[derive(Debug, Clone, PartialEq)]
#[derive(Serialize, Deserialize)]
enum PropKind {
    #[serde(rename = "init")] Init,
}

#[derive(Debug, Clone, PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(from = "MemberExprSerde", into = "MemberExprSerde")]
struct MemberExpression(Expression, Expression, bool);

impl From<MemberExprSerde> for MemberExpression {
    fn from(value: MemberExprSerde) -> Self {
        let MemberExprSerde{object, property, computed} = value;
        Self(object, property, computed)
    }
}

#[derive(Debug, Clone, PartialEq)]
#[derive(Serialize, Deserialize)]
struct MemberExprSerde {
    object: Expression,
    property: Expression,
    computed: bool,
}

impl From<MemberExpression> for MemberExprSerde {
    fn from(value: MemberExpression) -> Self {
        let MemberExpression(object, property, computed ) = value;
        Self{object, property, computed}
    }
}

#[derive(Debug, Clone, PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(from = "AssignExprSerde", into = "AssignExprSerde")]
struct AssignmentExpression(Expression, Option<BinOp>, Expression);

impl From<AssignExprSerde> for AssignmentExpression {
    fn from(value: AssignExprSerde) -> Self {
        let AssignExprSerde{operator, left, right} = value;
        let op = match operator.strip_suffix("=").unwrap() {
            "" => None,
            pre => Some(BinOp::try_from(pre).unwrap()),
        };
        Self(left, op, right)
    }
}

#[derive(Debug, Clone, PartialEq)]
#[derive(Serialize, Deserialize)]
struct AssignExprSerde {
    operator: String,
    left: Expression,
    right: Expression,
}

impl From<AssignmentExpression> for AssignExprSerde {
    fn from(value: AssignmentExpression) -> Self {
        let AssignmentExpression(left, op, right) = value;
        let operator = match op {
            None => format!("="),
            Some(op) => format!("{}", op),
        };
        Self{operator, left, right}
    }
}

#[derive(Debug, Clone, PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(from = "CondExprSerde", into = "CondExprSerde")]
struct ConditionalExpression(Expression, Expression, Expression);

impl From<CondExprSerde> for ConditionalExpression {
    fn from(value: CondExprSerde) -> Self {
        let CondExprSerde{test, consequent, alternate} = value;
        Self(test, consequent, alternate)
    }
}

#[derive(Debug, Clone, PartialEq)]
#[derive(Serialize, Deserialize)]
struct CondExprSerde {
    test: Expression,
    consequent: Expression,
    alternate: Expression,
}

impl From<ConditionalExpression> for CondExprSerde {
    fn from(value: ConditionalExpression) -> Self {
        let ConditionalExpression(test, consequent, alternate) = value;
        Self{test, consequent, alternate}
    }
}

#[derive(Debug, Clone, PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(from = "SeqExprSerde", into = "SeqExprSerde")]
struct SequenceExpression(Vec<Expression>);

impl From<SeqExprSerde> for SequenceExpression {
    fn from(value: SeqExprSerde) -> Self {
        let SeqExprSerde{expressions} = value;
        Self(expressions)
    }
}

#[derive(Debug, Clone, PartialEq)]
#[derive(Serialize, Deserialize)]
struct SeqExprSerde {
    expressions: Vec<Expression>,
}

impl From<SequenceExpression> for SeqExprSerde {
    fn from(value: SequenceExpression) -> Self {
        let SequenceExpression(expressions) = value;
        Self{expressions}
    }
}

#[derive(Debug, Clone, PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(from = "CallExprSerde", into = "CallExprSerde")]
struct NewExpression(Expression, Vec<Expression>);

impl From<CallExprSerde> for NewExpression {
    fn from(value: CallExprSerde) -> Self {
        let CallExprSerde { callee, arguments } = value;
        Self(callee, arguments)
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(tag = "type")]
enum Expr {
    Identifier(Identifier),

    Literal(Literal),

    #[serde(rename = "ThisExpression")]
    This,

    #[serde(rename = "BinaryExpression")]
    BinaryOp(Box<BinaryExpression>),

    #[serde(rename = "LogicalExpression")]
    LogicalOp(Box<LogicalExpression>),

    #[serde(rename = "CallExpression")]
    Call(Box<CallExpression>),

    #[serde(rename = "NewExpression")]
    New(Box<NewExpression>),

    #[serde(rename = "ArrayExpression")]
    Array(ArrayExpression),

    #[serde(rename = "ObjectExpression")]
    Object(ObjectExpression),

    #[serde(rename = "MemberExpression")]
    Member(Box<MemberExpression>),

    #[serde(rename = "AssignmentExpression")]
    Assign(Box<AssignmentExpression>),

    #[serde(rename = "ConditionalExpression")]
    Conditional(Box<ConditionalExpression>),

    #[serde(rename = "SequenceExpression")]
    Sequence(SequenceExpression),
}

impl From<Expression> for Expr {
    fn from(value: Expression) -> Self {
        value.expr
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
struct Expression {
    #[serde(flatten)]
    expr: Expr,

    #[serde(skip_serializing_if = "Option::is_none")]
    loc: Option<Box<crate::source::Location>>,
}

impl Expression {}

impl From<Expr> for Expression {
    fn from(expr: Expr) -> Self {
        Self { expr, loc: None }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{
        source::{Location, Position}, JSString
    };

    use serde_json::{json, Value as JSON};

    #[test]
    fn source_position() {
        let input = r#"{"line": 12, "column": 15 }"#;
        let got: Position = serde_json::from_str(input).unwrap();
        assert_eq!(got, Position::new(12, 15));
    }

    #[test]
    fn source_location() {
        let input = r#"{
                "start": {"line": 1, "column": 10},
                "end": {"line": 9, "column": 19}
            }"#;
        let got: Location = serde_json::from_str(input).unwrap();
        let want = Location::new(Position::new(1, 10), Position::new(9, 19));
        assert_eq!(got, want)
    }

    #[test]
    fn string_hello() {
        let input = r#" "hello" "#;
        let got: JSString = serde_json::from_str(input).unwrap();
        assert_eq!(got, JSString::from("hello"));
    }

    #[test]
    fn string_quoted() {
        let input = r#" "\"quoted" "#;
        let got: JSString = serde_json::from_str(input).unwrap();
        assert_eq!(got, JSString::from("\"quoted"));
    }

    #[test]
    fn string_unicode() {
        let input = r#" "\u0416" "#;
        let got: JSString = serde_json::from_str(input).unwrap();
        assert_eq!(got, JSString::from("Ж"));
    }

    #[test]
    fn identifier_ser() {
        let input = Identifier::from("x");
        let got = serde_json::to_value(&input).unwrap();
        assert_eq!(got, json!({"name": "x"}));
    }

    #[test]
    fn ident_de() {
        let got: Expr = serde_json::from_str(
            r#"
            { "type": "Identifier", "name": "x" }
        "#,
        )
        .unwrap();
        assert_eq!(got, Expr::Identifier(Identifier::from("x")));
    }

    #[test]
    fn literal() {
        let inp = Literal::from(JSON::Null);
        let got = serde_json::to_value(&inp).unwrap();
        assert_eq!(got, json!({"value": null}));

        let inp = Literal::from(42);
        let got = serde_json::to_value(&inp).unwrap();
        assert_eq!(got, json!({"value": 42}));

        let inp = Literal::from(true);
        let got = serde_json::to_value(&inp).unwrap();
        assert_eq!(got, json!({"value": true}));
    }

    #[test]
    fn expr_ident_ser() {
        let inp = Expr::Identifier(Identifier::from("x"));
        let got = serde_json::to_value(&inp).unwrap();
        assert_eq!(got, json!({ "type": "Identifier", "name": "x" }));

        let loc = Location::new(Position::new(5, 6), Position::new(5, 7));
        let inp = Expression {
            expr: Expr::Identifier(Identifier::from("x")),
            loc: Some(Box::new(loc)),
        };
        let got = serde_json::to_value(&inp).unwrap();
        assert_eq!(
            got,
            json!({
                "type": "Identifier",
                "name": "x",
                "loc": {
                    "start": {"line": 5, "column": 6},
                    "end": {"line": 5, "column": 7},
                }
            })
        );

        let inp = Expression::from(Expr::Identifier(Identifier::from("x")));
        let got = serde_json::to_value(&inp).unwrap();
        assert_eq!(
            got,
            json!({
                "type": "Identifier",
                "name": "x",
            })
        );
    }

    #[test]
    fn expr_lit_ser() {
        let inp = Expr::Literal(Literal::from(2));
        let got = serde_json::to_value(&inp).unwrap();
        assert_eq!(got, json!({ "type": "Literal", "value": 2 }));
    }

    #[test]
    fn expr_binop_ser() {
        let inp = Expr::BinaryOp(Box::new(BinaryExpression(
            Expression::from(Expr::Literal(Literal::from(2))),
            BinOp::Plus,
            Expression::from(Expr::Identifier(Identifier::from("x"))),
        )));
        let got = serde_json::to_value(&inp).unwrap();
        assert_eq!(
            got,
            json!({
                "type": "BinaryExpression",
                "operator": "+",
                "left": { "type": "Literal", "value": 2 },
                "right": { "type": "Identifier", "name": "x" },
            })
        );
    }

    #[test]
    fn expr_call_ser() {
        let inp = Expr::Call(Box::new(CallExpression(
            Expression::from(Expr::Identifier(Identifier::from("f"))),
            vec![],
        )));
        let got = serde_json::to_value(&inp).unwrap();
        assert_eq!(got, json!({
            "type": "CallExpression",
            "callee": {"type": "Identifier", "name": "f"},
            "arguments": [],
        }))
    }

    #[test]
    fn expr_object_de() {
        let inp = json!({
            "type": "ObjectExpression",
            "properties": [
                {
                    "type": "Property",
                    "key": {
                        "type": "Identifier",
                        "name": "a"
                    },
                    "computed": true,
                    "value": {
                        "type": "Literal",
                        "value": 1,
                        "raw": "1"
                    },
                    "kind": "init",
                    "method": false,
                    "shorthand": false
                }
            ]
        });
        let got: Expr = serde_json::from_value(inp).unwrap();
        let id_x = Expression::from(Expr::Identifier(Identifier::from("a")));
        let lit_1 = Expression::from(Expr::Literal(Literal::from(1)));
        assert_eq!(got, Expr::Object(ObjectExpression(vec![
            (ObjectKey::Computed(id_x), lit_1),
        ])))
    }

    #[test]
    fn expr_member_de() {
        let inp = json!({
            "type": "MemberExpression",
            "computed": false,
            "object": {
                "type": "Identifier",
                "name": "a"
            },
            "property": {
                "type": "Identifier",
                "name": "b"
            }
        });
        let got: Expr = serde_json::from_value(inp).unwrap();
        assert_eq!(got, Expr::Member(Box::new(MemberExpression(
            Expression::from(Expr::Identifier(Identifier::from("a"))),
            Expression::from(Expr::Identifier(Identifier::from("b"))),
            false,
        ))));
    }

    #[test]
    fn expr_assign_de() {
        let inp = json!({
            "type": "AssignmentExpression",
            "operator": "=",
            "left": {
                "type": "Identifier",
                "name": "a"
            },
            "right": {
                "type": "Identifier",
                "name": "b"
            }
        });
        let got: Expr = serde_json::from_value(inp).unwrap();
        assert_eq!(got, Expr::Assign(Box::new(AssignmentExpression(
            Expression::from(Expr::Identifier(Identifier::from("a"))),
            None,
            Expression::from(Expr::Identifier(Identifier::from("b"))),
        ))));

        let inp = json!({
            "type": "AssignmentExpression",
            "operator": ">>=",
            "left": {
                "type": "Identifier",
                "name": "a"
            },
            "right": {
                "type": "Identifier",
                "name": "b"
            }
        });
        let got: Expr = serde_json::from_value(inp).unwrap();
        assert_eq!(got, Expr::Assign(Box::new(AssignmentExpression(
            Expression::from(Expr::Identifier(Identifier::from("a"))),
            Some(BinOp::GtGt),
            Expression::from(Expr::Identifier(Identifier::from("b"))),
        ))));
    }

    #[test]
    fn expr_cond_de() {
        let inp = json!({
            "type": "ConditionalExpression",
            "test": { "type": "Identifier", "name": "p" },
            "consequent": { "type": "Identifier", "name": "a" },
            "alternate": { "type": "Identifier", "name": "b" }
        });
        let got: Expr = serde_json::from_value(inp).unwrap();
        assert_eq!(got, Expr::Conditional(Box::new(ConditionalExpression(
            Expression::from(Expr::Identifier(Identifier::from("p"))),
            Expression::from(Expr::Identifier(Identifier::from("a"))),
            Expression::from(Expr::Identifier(Identifier::from("b"))),
        ))));
    }

    #[test]
    fn expr_seq_de() {
        let got: Expr = serde_json::from_value(json!({
            "type": "SequenceExpression",
            "expressions": [
                { "type": "Identifier", "name": "a" },
                { "type": "Identifier", "name": "b" },
            ]
        })).unwrap();
        assert_eq!(got, Expr::Sequence(SequenceExpression(vec![
            Expression::from(Expr::Identifier(Identifier::from("a"))),
            Expression::from(Expr::Identifier(Identifier::from("b"))),
        ])));
    }

    #[test]
    fn expr_this_de() {
        let got: Expr = serde_json::from_value(json!({
            "type": "ThisExpression",
        })).unwrap();
        assert_eq!(got, Expr::This);
    }

    #[test]
    fn expr_new_de() {
        let got: Expr = serde_json::from_value(json!({
            "type": "NewExpression",
            "callee": {
                "type": "Identifier",
                "name": "F"
            },
            "arguments": []
        })).unwrap();
        assert_eq!(got, Expr::New(Box::new(NewExpression(
            Expression::from(Expr::Identifier(Identifier::from("F"))),
            vec![],
        ))));
    }

    #[test]
    fn expr_de() {
        let got: Expr = serde_json::from_str(
            r#"
            { "type": "Literal", "value": 2 }
        "#,
        )
        .unwrap();
        assert_eq!(got, Expr::Literal(Literal::from(2)));

        let inp = json!({
            "type": "BinaryExpression",
            "operator": "+",
            "left": {"type": "Literal", "value": 2},
            "right": {"type": "Identifier", "name": "x"},
        });
        let got: Expr = serde_json::from_value(inp).unwrap();
        assert_eq!(
            got,
            Expr::BinaryOp(Box::new(BinaryExpression(
                Expression::from(Expr::Literal(Literal::from(2))),
                BinOp::Plus,
                Expression::from(Expr::Identifier(Identifier::from("x"))),
            )))
        );

        let inp = json!({
            "type": "LogicalExpression",
            "operator": "&&",
            "left": {"type": "Identifier", "name": "a"},
            "right": {"type": "Identifier", "name": "b"},
        });
        let got: Expr = serde_json::from_value(inp).unwrap();
        assert_eq!(
            got,
            Expr::LogicalOp(Box::new(LogicalExpression(
                Expression::from(Expr::Identifier(Identifier::from("a"))),
                BoolOp::And,
                Expression::from(Expr::Identifier(Identifier::from("b"))),
            )))
        );

        let inp = json!({
            "type": "CallExpression",
            "callee": {"type": "Identifier", "name": "f"},
            "arguments": [],
        });
        let got: Expr = serde_json::from_value(inp).unwrap();
        assert_eq!(got, Expr::Call(Box::new(CallExpression(
            Expression::from(Expr::Identifier(Identifier::from("f"))),
            vec![],
        ))))
    }

    #[test]
    fn scratch() {}
}
