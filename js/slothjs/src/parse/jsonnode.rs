use serde_json::json;

use crate::ast::*;
use crate::parse::{
    ParseResult,
    SourceNode,
};
use crate::prelude::*;
use crate::{
    error::ParseError,
    JSON,
};

use crate::source;

impl SourceNode for JSON {
    type Error = JSON;

    fn to_error(&self) -> Self::Error {
        self.clone()
    }

    fn get_location(&self) -> Option<source::Location> {
        let start = match self.map_node("start", |child| Ok(child.clone())) {
            Err(_) => return None,
            Ok(node) => node,
        };
        let end = match self.map_node("end", |child| Ok(child.clone())) {
            Err(_) => return None,
            Ok(node) => node,
        };
        let jloc = json!({"start": start, "end": end});
        serde_json::from_value::<source::Location>(jloc).ok()
    }

    fn get_literal(&self, property: &str) -> ParseResult<Literal> {
        let node =
            (self.get(property)).ok_or_else(|| ParseError::no_attr(property, self.to_error()))?;
        Ok(Literal(node.clone()))
    }

    fn map_node<T, F>(&self, property: &str, mut action: F) -> ParseResult<T>
    where
        F: FnMut(&Self) -> ParseResult<T>,
    {
        let child = self
            .get(property)
            .ok_or_else(|| ParseError::no_attr(property, self.clone()))?;
        if child.is_null() {
            return Err(ParseError::no_attr(property, self.clone()))?;
        }
        action(child)
    }

    fn get_bool(&self, property: &str) -> ParseResult<bool> {
        let child = self
            .get(property)
            .ok_or_else(|| ParseError::no_attr(property, self.clone()))?;
        child.as_bool().ok_or_else(|| ParseError::ShouldBeBool {
            value: self.clone(),
        })
    }

    fn get_str(&self, property: &str) -> ParseResult<String> {
        let child = self
            .get(property)
            .ok_or_else(|| ParseError::no_attr(property, self.to_error()))?;
        let s = child.as_str().ok_or_else(|| ParseError::ShouldBeString {
            value: self.to_error(),
        })?;
        Ok(s.to_string())
    }

    fn map_array<T, F>(&self, property: &str, func: F) -> ParseResult<Vec<T>>
    where
        F: FnMut(&Self) -> ParseResult<T>,
    {
        let jarray =
            (self.get(property)).ok_or_else(|| ParseError::no_attr(property, self.to_error()))?;
        let array = (jarray.as_array()).ok_or_else(|| ParseError::ShouldBeArray {
            value: JSON::Null.to_error(),
        })?;
        array.iter().map(func).collect()
    }
}
