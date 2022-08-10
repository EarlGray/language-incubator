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
    fn to_error(&self) -> JSON {
        // .clone() in a hot path bites again.
        // It was called by .get_location() for pretty much every node. When there's no
        // "loc" in the field, it was cloning the entire JSON tree.
        //self.clone()
        JSON::Null
    }

    fn get_location(&self) -> Option<source::Location> {
        self.map_opt_node("loc", |loc| {
            if let Ok(start) = loc.map_node("start", |child| Ok(child.clone())) {
                if let Ok(end) = loc.map_node("end", |child| Ok(child.clone())) {
                    if let Ok(start) = serde_json::from_value::<source::Position>(start) {
                        if let Ok(end) = serde_json::from_value::<source::Position>(end) {
                            return Ok(source::Location::new(start, end));
                        }
                    }
                }
            }
            Err(ParseError::no_attr("", JSON::Null))    // empty String does not allocate
        })
        .unwrap_or(None)
    }

    fn get_literal(&self, property: &str) -> ParseResult<Literal> {
        let node =
            (self.get(property)).ok_or_else(|| ParseError::no_attr(property, self.to_error()))?;
        Ok(Literal::from(node.clone()))
    }

    fn map_node<T, F>(&self, property: &str, mut action: F) -> ParseResult<T>
    where
        F: FnMut(&Self) -> ParseResult<T>,
    {
        let child = self
            .get(property)
            .ok_or_else(|| ParseError::no_attr(property, self.to_error()))?;
        if child.is_null() {
            return Err(ParseError::no_attr(property, self.to_error()))?;
        }
        action(child)
    }

    fn get_bool(&self, property: &str) -> ParseResult<bool> {
        let child = self
            .get(property)
            .ok_or_else(|| ParseError::no_attr(property, self.clone()))?;
        child
            .as_bool()
            .ok_or_else(|| ParseError::want("bool", self.to_error()))
    }

    fn get_str(&self, property: &str) -> ParseResult<JSString> {
        let child =
            (self.get(property)).ok_or_else(|| ParseError::no_attr(property, self.to_error()))?;
        let s = (child.as_str()).ok_or_else(|| ParseError::want("string", self.to_error()))?;
        Ok(s.into())
    }

    fn map_array<T, F>(&self, property: &str, func: F) -> ParseResult<Vec<T>>
    where
        F: FnMut(&Self) -> ParseResult<T>,
    {
        let jarray =
            (self.get(property)).ok_or_else(|| ParseError::no_attr(property, self.to_error()))?;
        let array =
            (jarray.as_array()).ok_or_else(|| ParseError::want("array", JSON::Null.to_error()))?;
        array.iter().map(func).collect()
    }
}
