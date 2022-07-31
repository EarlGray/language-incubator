pub use core::{
    convert::TryFrom,
    fmt,
    fmt::Write,
    str::FromStr,
};

pub use alloc::{
    boxed::Box,
    format,
    rc::{Rc, Weak},
    string::{
        String,
        ToString,
    },
    vec,
    vec::Vec,
};

#[cfg(feature = "std")]
pub use std::collections::{
    HashMap,
    HashSet,
};

#[cfg(not(feature = "std"))]
pub use hashbrown::{
    hash_map::HashMap,
    hash_set::HashSet,
};
