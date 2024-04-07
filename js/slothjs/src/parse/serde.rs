use alloc::fmt;

use serde::de::{self, IgnoredAny};
use serde::{de::Visitor, Deserialize, Serialize};
use serde::ser::SerializeStruct;

use crate::ast::{BinaryExpression, Literal};
use crate::{
    ast::Identifier,
    source::Location,
    JSString,
};

impl<'de> Deserialize<'de> for JSString {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct VisitJSString;

        impl<'de> Visitor<'de> for VisitJSString {
            type Value = JSString;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                write!(formatter, "JSString")
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(JSString::from(v))
            }
        }

        deserializer.deserialize_str(VisitJSString)
    }
}

#[derive(Debug, Clone, PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(from = "TExpression", into = "TExpression")]
enum TExpr{
    Identifier(Identifier),
    Literal(Literal),
    //BinaryOp(Box<BinaryExpression>),
}

impl From<TExpression> for TExpr {
    fn from(value: TExpression) -> Self {
        value.expr
    }
}

#[derive(Debug, Clone, PartialEq)]
struct TExpression {
    pub expr: TExpr,
    pub loc: Option<Box<crate::source::Location>>,
}

impl TExpression {
    const NAME: &'static str = "TExpr";
}

impl From<TExpr> for TExpression {
    fn from(expr: TExpr) -> Self {
        Self{ expr, loc: None }
    }
}

impl Serialize for TExpression {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer
    {
        match &self.expr {
            TExpr::Identifier(ident) => {
                let mut s = serializer.serialize_struct(Self::NAME, 3)?;
                s.serialize_field("type", "Identifier")?;
                s.serialize_field("name", &ident)?;
                if let Some(loc) = &self.loc {
                    s.serialize_field("loc", loc)?;
                }
                s.end()
            }
            TExpr::Literal(lit) => {
                let mut s = serializer.serialize_struct(Self::NAME, 2)?;
                s.serialize_field("type", "Literal")?;
                s.serialize_field("value", &lit)?;
                if let Some(loc) = &self.loc {
                    s.serialize_field("loc", loc)?;
                }
                s.end()
            }
        }
    }
}

impl<'de> Deserialize<'de> for TExpression {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where D: serde::Deserializer<'de> 
    {
        struct VisitExpr;

        impl<'de> Visitor<'de> for VisitExpr {
            type Value = TExpression;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                write!(formatter, "TExpr")
            }

            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
                where A: serde::de::MapAccess<'de>
            {
                // this relies on the "type" being the first field
                let ty = match map.next_entry()? {
                    Some(("type", ty)) => Ok(ty),
                    Some((key, _)) => Err(de::Error::unknown_field(key, &["type"])),
                    None => Err(de::Error::missing_field("type")),
                }?;
                let (expr, loc) = match ty {
                    "Identifier" => {
                        let mut ident: Option<Identifier> = None;
                        let mut loc: Option<Box<Location>> = None;
                        while let Some(key) = map.next_key()? {
                            match key {
                                "name" => ident = Some(map.next_value()?),
                                "loc" => loc = Some(map.next_value()?),
                                _ => { map.next_value::<IgnoredAny>()?; }
                            }
                        }
                        let Some(ident) = ident else {
                            return Err(de::Error::missing_field("name"));
                        };
                        Ok((TExpr::Identifier(ident), loc))
                    }
                    "Literal" => {
                        let mut lit: Option<Literal> = None;
                        let mut loc: Option<Box<Location>> = None;
                        while let Some(key) = map.next_key()? {
                            match key {
                                "value" => lit = Some(map.next_value()?),
                                "loc" => loc = Some(map.next_value()?),
                                _ => { map.next_value::<IgnoredAny>()?; }
                            }
                        }
                        match lit {
                            None => Err(de::Error::missing_field("value")),
                            Some(lit) => Ok((TExpr::Literal(lit), loc)),
                        }
                    }
                    _ => todo!("Unknown type {ty}")
                }?;
                Ok(TExpression{ expr, loc })
            }
        }

        deserializer.deserialize_map(VisitExpr)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{
        ast::*,
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
        assert_eq!(got, JSON::String("x".into()));
    }


    #[test]
    fn literals() {
        let inp = Literal::from(JSON::Null);
        let got = serde_json::to_string(&inp).unwrap();
        assert_eq!(got, "null");

        let inp = Literal::from(42);
        let got = serde_json::to_string(&inp).unwrap();
        assert_eq!(got, "42");

        let inp = Literal::from(true);
        let got = serde_json::to_string(&inp).unwrap();
        assert_eq!(got, "true");
    }

    #[test]
    fn expr_ser() {
        let inp = TExpr::Identifier(Identifier::from("x"));
        let got = serde_json::to_value(&inp).unwrap();
        assert_eq!(got, json!({ "type": "Identifier", "name": "x" }));
 
        let inp = TExpr::Literal(Literal::from(2));
        let got = serde_json::to_value(&inp).unwrap();
        assert_eq!(got, json!({ "type": "Literal", "value": 2 }));
    }

    #[test]
    fn expr_de() {
        let got: TExpr = serde_json::from_str(r#"
            { "type": "Identifier", "name": "x" }
        "#).unwrap();
        assert_eq!(got, TExpr::Identifier(Identifier::from("x")));

        let got: TExpr = serde_json::from_str(r#"
            { "type": "Literal", "value": 2 }
        "#).unwrap();
        assert_eq!(got, TExpr::Literal(Literal::from(2)));
    }

    #[test]
    fn expression_ser() {
        let inp = TExpression{
            expr: TExpr::Identifier(Identifier::from("x")),
            loc: None,
        };
        let got = serde_json::to_value(&inp).unwrap();
        assert_eq!(got, json!({
            "type": "Identifier",
            "name": "x",
        }));

        let loc = Location::new(
            Position::new(5, 6),
            Position::new(5, 7),
        );
        let inp = TExpression{
            expr: TExpr::Identifier(Identifier::from("x")),
            loc: Some(Box::new(loc)),
        };
        let got = serde_json::to_value(&inp).unwrap();
        assert_eq!(got, json!({
            "type": "Identifier",
            "name": "x",
            "loc": {
                "start": {"line": 5, "column": 6},
                "end": {"line": 5, "column": 7},
            }
        }));
    }

    #[test]
    fn scratch() {
    }
}
