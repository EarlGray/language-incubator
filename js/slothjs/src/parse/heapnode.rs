use crate::prelude::*;
use crate::{
    source,
    Heap,
    JSRef,
    JSResult,
    JSValue,
};

use super::*;

/// `HeapNode` contains a heap reference and a `JSRef` to an AST subtree in it.
/// It implements [`SourceNode`] for on-heap AST trees.
#[derive(Clone)]
pub struct HeapNode {
    heap: Rc<Heap>,
    node: JSRef,
}

impl HeapNode {
    pub fn with<T, F>(heap: &mut Heap, node: JSRef, mut action: F) -> T
    where
        F: FnMut(&HeapNode) -> T,
    {
        let mut tmp = Heap::new();
        core::mem::swap(heap, &mut tmp);
        let heapptr = Rc::new(tmp);
        let heapnode = HeapNode {
            heap: heapptr,
            node,
        };

        let result = action(&heapnode);

        tmp = Rc::try_unwrap(heapnode.heap).expect("only one reference left");
        core::mem::swap(heap, &mut tmp);
        result
    }

    pub fn with_node(&self, node: JSRef) -> Self {
        HeapNode {
            heap: Rc::clone(&self.heap),
            node,
        }
    }

    fn property(&self, name: &str) -> ParseResult<JSValue> {
        (self.heap.get(self.node).get_own_value(name))
            .ok_or_else(|| ParseError::no_attr(name, self.to_error()))
    }

    fn to_json(&self) -> JSResult<JSON> {
        self.heap.get(self.node).to_json(&self.heap)
    }
}

impl fmt::Debug for HeapNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "HeapNode{{ {:?} }}", self.node)
    }
}

impl SourceNode for HeapNode {
    type Error = Self;

    fn to_error(&self) -> JSON {
        self.to_json().expect("HeapNode::to_error")
    }

    fn get_location(&self) -> Option<source::Location> {
        self.map_opt_node("loc", |loc| {
            let jloc = loc.to_json().expect("json from loc");
            serde_json::from_value::<source::Location>(jloc).map_err(|e| {
                let err = format!("Can't get source::Location from JSON: {:?}", e);
                ParseError::InvalidJSON { err }
            })
        })
        .unwrap_or(None)
    }

    fn get_literal(&self, property: &str) -> ParseResult<Literal> {
        let child = self.property(property)?;
        let json = child.to_json(&self.heap).map_err(ParseError::invalid_ast)?;
        Ok(Literal(json))
    }

    fn get_bool(&self, property: &str) -> ParseResult<bool> {
        let value = self.property(property)?;
        match value {
            JSValue::Bool(b) => Ok(b),
            _ => Err(ParseError::want("bool", self.to_error())),
        }
    }

    fn get_str(&self, property: &str) -> ParseResult<String> {
        let value = self.property(property)?;
        match value {
            JSValue::String(s) => Ok(s),
            _ => Err(ParseError::want("string", self.to_error())),
        }
    }

    fn map_node<T, F>(&self, property: &str, mut action: F) -> ParseResult<T>
    where
        F: FnMut(&Self) -> ParseResult<T>,
    {
        match self.property(property)? {
            JSValue::Ref(childref) => {
                let child = self.with_node(childref);
                Ok(action(&child)?)
            }
            _ => Err(ParseError::no_attr(property, self.to_error())),
        }
    }

    fn map_array<T, F>(&self, property: &str, mut func: F) -> ParseResult<Vec<T>>
    where
        F: FnMut(&Self) -> ParseResult<T>,
    {
        let value = self.property(property)?;
        let arrref = (value.to_ref()).map_err(|_| ParseError::want("array", self.to_error()))?;
        let array = (self.heap.get(arrref).as_array())
            .ok_or_else(|| ParseError::want("array", self.to_error()))?;
        let mut result = vec![];
        for item in &array.storage {
            let childref =
                (item.to_ref()).map_err(|_| ParseError::want("objects", self.to_error()))?;
            let child = self.with_node(childref);
            let r = func(&child)?;
            result.push(r);
        }
        Ok(result)
    }
}
