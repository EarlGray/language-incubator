use serde::{
    Deserialize,
    Serialize,
};

#[derive(Clone, Copy, Debug, Deserialize, Serialize)]
pub struct Position {
    line: usize,
    column: usize,
}

#[derive(Clone, Copy, Debug, Deserialize, Serialize)]
pub struct Location {
    start: Position,
    end: Position,
}

#[derive(Clone, Debug)]
pub struct Document {
    name: String,
}
