mod array;
mod boolean;
mod console;
mod function;
mod global;
mod object;

use crate::error::Exception;
use crate::heap::Heap;
use crate::object::Content;

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

    console::init(heap)?;

    Ok(())
}
