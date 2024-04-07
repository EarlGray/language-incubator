use serde::{Deserialize, Serialize};

use crate::ast::{BinOp, Literal};
use crate::ast::{BoolOp, Identifier};

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(from = "BinExprSerde", into = "BinExprSerde")]
struct BinaryExpression(TExpression, BinOp, TExpression);

impl From<BinExprSerde> for BinaryExpression {
    fn from(value: BinExprSerde) -> Self {
        Self(value.left, value.operator, value.right)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct BinExprSerde {
    operator: BinOp,
    left: TExpression,
    right: TExpression,
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
pub struct LogicalExpression(TExpression, BoolOp, TExpression);

#[derive(Debug, Clone, Serialize, Deserialize)]
struct LogicExprSerde {
    operator: BoolOp,
    left: TExpression,
    right: TExpression,
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

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(tag = "type")]
enum TExpr {
    Identifier(Identifier),

    Literal(Literal),

    #[serde(rename = "BinaryExpression")]
    BinaryOp(Box<BinaryExpression>),

    #[serde(rename = "LogicalExpression")]
    LogicalOp(Box<LogicalExpression>),
}

impl From<TExpression> for TExpr {
    fn from(value: TExpression) -> Self {
        value.expr
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
struct TExpression {
    #[serde(flatten)]
    expr: TExpr,

    #[serde(skip_serializing_if = "Option::is_none")]
    loc: Option<Box<crate::source::Location>>,
}

impl TExpression {}

impl From<TExpr> for TExpression {
    fn from(expr: TExpr) -> Self {
        Self { expr, loc: None }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{
        source::{Location, Position},
        JSString,
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
    fn expr_ser() {
        let inp = TExpr::Identifier(Identifier::from("x"));
        let got = serde_json::to_value(&inp).unwrap();
        assert_eq!(got, json!({ "type": "Identifier", "name": "x" }));

        let inp = TExpr::Literal(Literal::from(2));
        let got = serde_json::to_value(&inp).unwrap();
        assert_eq!(got, json!({ "type": "Literal", "value": 2 }));

        let inp = TExpr::BinaryOp(Box::new(BinaryExpression(
            TExpression::from(TExpr::Literal(Literal::from(2))),
            BinOp::Plus,
            TExpression::from(TExpr::Identifier(Identifier::from("x"))),
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
        )
    }

    #[test]
    fn expr_de() {
        let got: TExpr = serde_json::from_str(
            r#"
            { "type": "Identifier", "name": "x" }
        "#,
        )
        .unwrap();
        assert_eq!(got, TExpr::Identifier(Identifier::from("x")));

        let got: TExpr = serde_json::from_str(
            r#"
            { "type": "Literal", "value": 2 }
        "#,
        )
        .unwrap();
        assert_eq!(got, TExpr::Literal(Literal::from(2)));

        let inp = json!({
            "type": "BinaryExpression",
            "operator": "+",
            "left": {"type": "Literal", "value": 2},
            "right": {"type": "Identifier", "name": "x"},
        });
        let got: TExpr = serde_json::from_value(inp).unwrap();
        assert_eq!(
            got,
            TExpr::BinaryOp(Box::new(BinaryExpression(
                TExpression::from(TExpr::Literal(Literal::from(2))),
                BinOp::Plus,
                TExpression::from(TExpr::Identifier(Identifier::from("x"))),
            )))
        );

        let inp = json!({
            "type": "LogicalExpression",
            "operator": "&&",
            "left": {"type": "Identifier", "name": "a"},
            "right": {"type": "Identifier", "name": "b"},
        });
        let got: TExpr = serde_json::from_value(inp).unwrap();
        assert_eq!(
            got,
            TExpr::LogicalOp(Box::new(LogicalExpression(
                TExpression::from(TExpr::Identifier(Identifier::from("a"))),
                BoolOp::And,
                TExpression::from(TExpr::Identifier(Identifier::from("b"))),
            )))
        )
    }

    #[test]
    fn expression_ser() {
        let inp = TExpression::from(TExpr::Identifier(Identifier::from("x")));
        let got = serde_json::to_value(&inp).unwrap();
        assert_eq!(
            got,
            json!({
                "type": "Identifier",
                "name": "x",
            })
        );

        let loc = Location::new(Position::new(5, 6), Position::new(5, 7));
        let inp = TExpression {
            expr: TExpr::Identifier(Identifier::from("x")),
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
    }

    #[test]
    fn scratch() {}
}
