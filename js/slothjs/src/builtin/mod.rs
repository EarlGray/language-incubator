pub mod object;
pub mod function;
/*
pub mod array;
pub mod boolean;
pub mod error;
pub mod global;
pub mod string;
*/

use crate::{
    JSResult,
    Realm,
    value::HostFn,
    heap::With,
};

/// This describes a ES6 class implemented in native code.
pub struct HostClass{
    pub name: &'static str,
    pub constructor: HostFn,
    pub static_methods: &'static [(&'static str, HostFn)],
    pub proto_methods: &'static [(&'static str, HostFn)],
}

impl HostClass {
    pub fn init(&self, realm: &Realm) -> JSResult<()> {
        realm.with_new(|ctor| {
            realm.with_new(|proto| {
                Ok(())
            })
        })
    }
}

/*
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

    Ok(())
}
*/
