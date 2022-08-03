pub mod value;
mod object;

use core::num::{NonZeroU32, NonZeroUsize};
use core::cell::RefCell;
use core::pin::Pin;

use crate::prelude::*;

use crate::{
    JSResult,
    builtin,
};
use self::object::Object;

pub use self::value::{
    JSString,
    JSNumber,
    HostFn,
    HostFunc,
    StrKey,
    Value,
};

/// An index on the heap.
type HRef = u32;

const NULL: HRef = 0;


/// An external reference to an object on the [`Heap`].
///
/// Internal invariant for this module:
/// `JSRef::drop()` and `Realm::heap.borrow_mut()` must be mutually exclusive to avoid panicking.
#[derive(Debug)]
pub struct JSRef {
    heap: Weak<RefCell<Heap>>,
    href: HRef,         // external references make internal references "pinned"
}

impl Clone for JSRef {
    fn clone(&self) -> Self {
        if let Some(heaprc) = self.heap.upgrade() {
            heaprc.borrow().get(self.href).make_extref();
        }
        JSRef{
            heap: Weak::clone(&self.heap),
            href: self.href,
        }
    }
}

impl Drop for JSRef {
    fn drop(&mut self) {
        if let Some(heaprc) = self.heap.upgrade() {
            heaprc.borrow_mut().get(self.href).drop_extref();
        }
    }
}

/// ES6 Realm: a storage + its global object.
///
/// Technically, a `Realm` is a `JSRef` to a global object on a `Heap`
/// that actually owns this `Heap`.
pub struct Realm {
    heap: Rc<RefCell<Heap>>,
    global: HRef,
    object_proto: HRef,
    function_proto: HRef,
}

impl Realm {
    /// Create a new [`Realm`] with a new [`Heap`]
    pub fn init() -> JSResult<Realm> {
        let mut realm = Realm{
            heap: Rc::new(RefCell::new(Heap::new())),
            global: NULL,
            object_proto: NULL,
            function_proto: NULL,
        };

        realm.init_global()?;
        Ok(realm)
    }

    fn init_global(&mut self) -> JSResult<()> {
        let (global, object_proto, function_proto): (HRef, HRef, HRef) = {
            let mut heap = self.heap.borrow_mut();
            let global = heap.alloc(Object::with_proto(NULL));

            // Initialize it with the builtins it depends on
            let object_proto = heap.alloc(Object::with_proto(NULL));
            let function_proto = heap.alloc(Object::with_proto(object_proto));
            // TODO: set the methods

            let mut the_object = Object::with_func(builtin::object::constructor, function_proto);
            the_object.define("prototype", object_proto)?;
            let object_object = heap.alloc(the_object);

            let mut the_function = Object::with_func(
                builtin::function::constructor,
                function_proto,
            );
            the_function.define("prototype", function_proto)?;
            let function_object = heap.alloc(the_function);

            heap.get_mut(object_proto).define("constructor", object_object)?;
            heap.get_mut(function_proto).define("constructor", object_object)?;
            {
                let global_object = heap.get_mut(global);
                global_object.set_proto(object_proto);
                global_object.define("Object", object_object)?;
                global_object.define("Function", function_object)?;
                global_object.define("global", global)?;
                global_object.define("globalThis", global)?;
                global_object.make_extref();
            }
            (global, object_proto, function_proto)
        };

        self.with_heap(|heap| heap.get(object_proto).make_extref());
        self.with_heap(|heap| heap.get(function_proto).make_extref());
        self.with_heap(|heap| heap.get(global).make_extref());

        self.global = global;
        self.object_proto = object_proto;
        self.function_proto = function_proto;
        Ok(())
    }

    pub fn with<T, F, const N: usize>(&self, its: [With; N], mut action: F) -> JSResult<T>
        where F: FnMut([Pin<&mut Ref>; N]) -> JSResult<T>
    {
        use core::mem::MaybeUninit;
        let mut lrefs: [MaybeUninit<Ref>; N] = MaybeUninit::uninit_array();
        for i in 0..N {
            let mut heap = self.heap.borrow_mut();
            let innerref = match &its[i] {
                With::New => {
                    let href = heap.alloc(Object::with_proto(self.object_proto));
                    InnerRef::href(href)
                }
                With::Val(value) => {
                    let href = heap.alloc(Object::with_value(value.clone()));
                    InnerRef::href(href)
                }
                With::Var(varpath) => {
                    heap.lookup_path(self.global, varpath)
                }
            };
            let lref = heap.refstack.len();
            heap.refstack.push(innerref);
            lrefs[i].write(Ref{realm: &self, lref});
        }
        let mut lrefs: [Ref; N] = unsafe { MaybeUninit::array_assume_init(lrefs) };

        let mut lrefs_slice: &mut [Ref] = lrefs.as_mut_slice();
        let mut refpins: [MaybeUninit<Pin<&mut Ref>>; N] = MaybeUninit::uninit_array();
        let mut i = 0;
        while let Some((head, tail)) = lrefs_slice.split_first_mut() {
            lrefs_slice = tail;
            refpins[i].write(Pin::new(head));
            i += 1;
        }

        let refpins = unsafe { MaybeUninit::array_assume_init(refpins) };
        action(refpins)
    }

    pub fn with_new<T, F>(&self, mut action: F) -> JSResult<T>
        where F: FnMut(Pin<&mut Ref>) -> JSResult<T>
    {
        // but: panics and unwinding.
        let lref = {
            let mut heap = self.heap.borrow_mut();
            let href = heap.alloc(Object::with_proto(self.object_proto));
            heap.refstack.push(InnerRef::href(href));
            heap.refstack.len() - 1
        };

        let mut var = Ref{realm: &self, lref};
        let result = action(Pin::new(&mut var));
        {
            let mut heap = self.heap.borrow_mut();
            heap.refstack.pop();
        }
        result
    }

    fn with_heap<'b, 's: 'b, T, F>(&'s self, mut action: F) -> T
        where F: FnMut(core::cell::RefMut<'b, Heap>) -> T
    {
        let heap = self.heap.borrow_mut();
        action(heap)
    }

}

/*
impl Drop for Realm {
    fn drop(&mut self) {
        let heap = self.heap.borrow();
        heap.get(self.function_proto).drop_extref();
        heap.get(self.object_proto).drop_extref();
        heap.get(self.global).drop_extref();
    }
}
*/

/// Runtime heap: storage for objects.
#[derive(Debug)]
pub struct Heap{
    objects: Vec<Object>,
    refstack: Vec<InnerRef>,
}

impl Heap {
    fn new() -> Self {
        let null_object = Object::with_value(Value::Null);
        Heap{
            objects: vec![null_object],     // because HRef = 0 is always invalid:
            refstack: vec![],
        }
    }

    fn alloc(&mut self, object: Object) -> HRef {
        let href: u32 = self.objects.len() as u32;
        self.objects.push(object);
        href
    }

    fn get(&self, href: HRef) -> &Object {
        self.objects.get(href as usize)
            .unwrap_or_else(|| panic!("Heap::get({:?})", href))
    }

    fn get_mut(&mut self, href: HRef) -> &mut Object {
        self.objects.get_mut(href as usize)
            .unwrap_or_else(|| panic!("Heap::get_mut({:?})", href))
    }

    fn lookup_path(&self, global: HRef, varpath: &str) -> InnerRef {
        let mut href = global;
        let mut id = "";
        for part in varpath.split(".") {
            let Some(r) = self.get(href).get(id) else {
                return InnerRef::EMPTY;
            };
            href = r;
            id = part;
        }
        InnerRef::member(href, id)
    }
}



/// An index in [`Heap.refstack`].
type LRef = usize;

#[derive(Debug, Clone, PartialEq, Eq)]
struct InnerRef(Option<(HRef, Option<StrKey>)>);

impl InnerRef {
    const EMPTY: InnerRef = InnerRef(None);

    fn href(href: HRef) -> Self {
        InnerRef(Some((href, None)))
    }

    // A "Reference" in ES6
    fn member<S>(base: HRef, attr: S) -> Self
        where StrKey: From<S>
    {
        InnerRef(Some((base, Some(StrKey::from(attr)))))
    }
}

/// A "variable", either encapsulating a reference to an [`Object`] or holding a [`Value`].
///
/// These are only created by a [`Realm`] for fixed scopes where `Pin<&mut Ref>` can be used.
pub struct Ref<'r>{
    realm: &'r Realm,
    lref: LRef,
}

impl<'r> Ref<'r> {
    pub fn is_empty(self: Pin<&mut Self>) -> bool {
        let heap = self.realm.heap.borrow();
        heap.refstack[self.lref] == InnerRef::EMPTY
    }
}

/// An opaque reference to an [`Object`].
pub struct ObjectRef(LRef);

/// How to create a reference?
pub enum With<'a> {
    /// Allocate a new object
    New,

    /// Put a [`Value`] into a new [`Ref`]
    Val(Value),

    /// Lookup the given path in the [`Realm`].
    ///
    /// Lookup a given path (e.g. `Object.getPrototypeOf`, `global`),
    /// bind to a new [`Ref`] if exists. If not, create an empty `Ref`.
    Var(&'a str),
}


#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn realm_init() {
        let realm = Realm::init().expect("Realm::init");
        realm.with([With::Var("global")], |[the_object]| {
            Ok(())
        }).unwrap();
    }
}
