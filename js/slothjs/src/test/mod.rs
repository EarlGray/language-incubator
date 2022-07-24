mod interp;

/// Runs interpretation of the first argument (a string literal),
/// then compares the result to the second argument (anything that `serde_json::json!`
/// understands).
macro_rules! assert_eval {
    ($json:tt, $program:expr) => {
        assert_eval!($json, $program, "");
    };
    ($json:tt, $program:expr, $desc:expr) => {
        let want = serde_json::json!($json);
        let interpretable = $program;
        let mut heap = crate::Heap::new();
        let result = match heap.evaluate(&interpretable) {
            Ok(r) => r,
            Err(exc) => panic!("\n  error: {:?}\n    want: {}", &exc, &want),
        };
        let got = result.to_json(&mut heap).expect("json");
        if got != want {
            panic!("{}\n     got: {}\n    want: {}", $desc, &got, &want);
        }
    };
}

pub(crate) use assert_eval;

/*
/// Run interpretation of the first argument (a string literal),
/// then expects it to fail with a given variant of Exception:
/// ```ignored
/// assert_exception!( "bla", Exception::ReferenceNotFound );
/// ```
macro_rules! assert_exception {
    ($js:literal, $exc:path) => {
    }
}

pub(crate) use assert_exception;
*/

/// ```sh
/// $ cargo -q test --lib sizes -- --nocapture
/// ```
#[test]
fn test_sizes() {
    use crate::object::*;
    use crate::*;
    use std::mem::size_of;

    println!("============================");
    println!("size_of JSRef:  \t{}", size_of::<heap::JSRef>());
    println!("size_of JSValue:\t{}", size_of::<object::JSValue>());
    println!("size_of Interpreted:\t{}", size_of::<Interpreted>());
    println!("size_of JSObject:\t{}", size_of::<object::JSObject>());
    println!(
        "size_of   HashMap:\t{}",
        size_of::<std::collections::HashMap<String, Property>>()
    );
    println!("size_of   ObjectValue:\t{}", size_of::<ObjectValue>());
    println!("size_of     JSArray:\t{}", size_of::<object::JSArray>());
    println!(
        "size_of     NativeFunc:\t{}",
        size_of::<function::NativeFunction>()
    );
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
