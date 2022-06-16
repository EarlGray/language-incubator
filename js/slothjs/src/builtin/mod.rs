pub mod array;
pub mod boolean;
mod console;
pub mod error;
pub mod function;
pub mod global;
pub mod object;
pub mod string;

use crate::{
    object::Content,
    Exception,
    Heap,
};

pub fn init(heap: &mut Heap) -> Result<(), Exception> {
    global::init(heap)?;

    let the_object = object::init(heap)?;
    heap.get_mut(Heap::GLOBAL)
        .set_hidden("Object", Content::from(the_object))?;

    let the_function = function::init(heap)?;
    heap.get_mut(Heap::GLOBAL)
        .set_hidden("Function", Content::from(the_function))?;

    let the_array = array::init(heap)?;
    heap.get_mut(Heap::GLOBAL)
        .set_hidden("Array", Content::from(the_array))?;

    let the_boolean = boolean::init(heap)?;
    heap.get_mut(Heap::GLOBAL)
        .set_hidden("Boolean", Content::from(the_boolean))?;

    let the_string = string::init(heap)?;
    heap.get_mut(Heap::GLOBAL)
        .set_hidden("String", Content::from(the_string))?;

    let the_error = error::init(heap)?;
    heap.get_mut(Heap::GLOBAL)
        .set_hidden("Error", Content::from(the_error))?;

    console::init(heap)?;

    Ok(())
}
