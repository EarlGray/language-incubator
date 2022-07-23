/// ```sh
/// $ cargo -q test --lib sizes -- --nocapture
/// ```
#[test]
fn test_sizes() {
    use crate::*;
    use crate::object::*;
    use std::mem::size_of;

    println!("============================");
    println!("size_of JSRef:  \t{}", size_of::<heap::JSRef>());
    println!("size_of JSValue:\t{}", size_of::<object::JSValue>());
    println!("size_of Interpreted:\t{}", size_of::<Interpreted>());
    println!("size_of JSObject:\t{}", size_of::<object::JSObject>());
    println!("size_of   HashMap:\t{}", size_of::<std::collections::HashMap<String, Property>>());
    println!("size_of   ObjectValue:\t{}", size_of::<ObjectValue>());
    println!("size_of     JSArray:\t{}", size_of::<object::JSArray>());
    println!("size_of     NativeFunc:\t{}", size_of::<function::NativeFunction>());
    println!("size_of     Closure:\t{}", size_of::<function::Closure>());
    println!("size_of Property:\t{}", size_of::<Property>());
    println!("size_of   Access:\t{}", size_of::<Access>());
    println!("size_of   Content:\t{}", size_of::<Content>());
    println!("============================");
}

#[test]
fn test_scratch() {
    // One-off experiments, don't commit anything here.
}
