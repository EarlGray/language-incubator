use crate::value::JSON;


#[derive(Debug)]
pub enum ParseError<V> {
    InvalidJSON{ err: serde_json::Error },
    ShouldBeBool{ value: V },
    ShouldBeString{ value: V },
    ShouldBeArray{ value: V },
    //ShouldBeObject{ value: V },
    ObjectWithout{ attr: &'static str, value: V},
    UnexpectedValue{ want: &'static str, value: V},
    UnknownType{ value: V},
}

#[derive(Debug)]
pub enum Exception {
    SyntaxError(ParseError<JSON>),
    ReferenceError(String),
}
