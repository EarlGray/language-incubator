mod array;
mod console;
mod function;
mod global;
mod object;

use crate::error::Exception;
use crate::object::Heap;

pub fn init(heap: &mut Heap) -> Result<(), Exception> {
    // NOTE: be careful with the order of initialization
    global::init(heap)?;
    object::init(heap)?;
    function::init(heap)?;
    array::init(heap)?;

    console::init(heap)?;

    Ok(())
}
