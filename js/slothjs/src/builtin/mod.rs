pub mod array;
pub mod boolean;
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
    heap.init_class(Heap::OBJECT_PROTO, &object::CLASS)?;
    {
        #[cfg(feature = "std")]
        let proto_dbg = heap.alloc_func(object::object_proto_dbg);

        let proto_object = heap.get_mut(Heap::OBJECT_PROTO);
        proto_object.proto = Heap::NULL;

        #[cfg(feature = "std")]
        proto_object.set_system("dbg".into(), proto_dbg)?;
    }

    heap.init_class(Heap::FUNCTION_PROTO, &function::CLASS)?;
    heap.init_class(Heap::ARRAY_PROTO, &array::CLASS)?;
    heap.init_class(Heap::BOOLEAN_PROTO, &boolean::CLASS)?;
    heap.init_class(Heap::STRING_PROTO, &string::CLASS)?;
    heap.init_class(Heap::ERROR_PROTO, &error::CLASS)?;
    {
        let error_proto = heap.get_mut(Heap::ERROR_PROTO);
        error_proto.set_hidden("name".into(), "Error")?;
        error_proto.set_hidden("message".into(), "")?;
    }
    Ok(())
}
