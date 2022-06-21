pub mod array;
pub mod boolean;
mod console;
pub mod error;
pub mod function;
pub mod global;
pub mod object;
pub mod string;

use crate::{
    Heap,
    JSResult,
};

pub fn init(heap: &mut Heap) -> JSResult<()> {
    global::init(heap)?;

    let the_object = object::init(heap)?;
    heap.get_mut(Heap::GLOBAL)
        .set_hidden("Object", the_object)?;

    let the_function = function::init(heap)?;
    heap.get_mut(Heap::GLOBAL)
        .set_hidden("Function", the_function)?;

    let the_array = array::init(heap)?;
    heap.get_mut(Heap::GLOBAL).set_hidden("Array", the_array)?;

    let the_boolean = boolean::init(heap)?;
    heap.get_mut(Heap::GLOBAL)
        .set_hidden("Boolean", the_boolean)?;

    let the_string = string::init(heap)?;
    heap.get_mut(Heap::GLOBAL)
        .set_hidden("String", the_string)?;

    let the_error = error::init(heap)?;
    heap.get_mut(Heap::GLOBAL).set_hidden("Error", the_error)?;

    console::init(heap)?;

    Ok(())
}
