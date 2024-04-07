use alloc::fmt;

use serde::de::{self, Error};
use serde::{de::Visitor, Deserialize, Serialize};
use serde::ser::SerializeStruct;
use serde_json::Value as JSON;

use crate::{
    ast::Identifier,
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
enum TExpr{
    Identifier(Identifier)
}

impl TExpr {
    const NAME: &'static str = "TExpr";
}

impl Serialize for TExpr {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer
    {
        match self {
            TExpr::Identifier(ident) => {
                let mut s = serializer.serialize_struct(Self::NAME, 2)?;
                s.serialize_field("type", "Identifier")?;
                s.serialize_field("name", &ident)?;
                s.end()
            }
        }
    }
}

impl<'de> Deserialize<'de> for TExpr {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where D: serde::Deserializer<'de> 
    {
        struct VisitExpr;

        impl<'de> Visitor<'de> for VisitExpr {
            type Value = TExpr;

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
                match ty {
                    "Identifier" => {
                        let ident = match map.next_entry()? {
                            Some(("name", ident)) => Ok(ident),
                            Some((key, _)) => Err(de::Error::unknown_field(key, &["name"])),
                            None => Err(de::Error::missing_field("name")),
                        }?;
                        Ok(TExpr::Identifier(ident))
                    }
                    _ => todo!("Unknown type {ty}")
                }
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
    fn expr_ser() {
        let inp = TExpr::Identifier(Identifier::from("x"));
        let got = serde_json::to_value(&inp).unwrap();
        assert_eq!(got, json!({ "type": "Identifier", "name": "x" }));
    }

    #[test]
    fn expr_de() {
        let got: TExpr = serde_json::from_str(r#"{
            "type": "Identifier",
            "name": "x"
        }"#).unwrap();
        assert_eq!(got, TExpr::Identifier(Identifier::from("x")));
    }

    #[test]
    fn scratch() {
    }
}
