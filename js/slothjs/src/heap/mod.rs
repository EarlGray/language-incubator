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
            // TODO: move this into Heap.
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

    /// Run a closure `action` with an array of temporary [`Ref`]s
    /// created from an array of [`With`] initializers.
    ///
    /// E.g.
    /// ```
    /// # use core::pin::Pin;
    /// # use slothjs::{Realm, With, Ref, Value};
    /// let realm = Realm::init().unwrap();
    /// realm.with([With::Val(Value::from(4.0))], |[four]| {
    ///     assert_eq!(four.as_value(), Some(Value::from(4.0)));
    ///     Ok(())
    /// }).unwrap();
    /// ```
    /// TODO: some compile_fail examples for moving out `Pin<&mut Ref>`.
    pub fn with<T, F, const N: usize>(&self, withs: [With; N], mut action: F) -> JSResult<T>
        where F: FnMut([Pin<&mut Ref>; N]) -> JSResult<T>
    {
        use core::mem::MaybeUninit;     // because const-array programming is not fun.

        // create `Ref`s from descriptions in `withs`:
        let mut refs: [Ref; N] = {
            let mut refs: [MaybeUninit<Ref>; N] = MaybeUninit::uninit_array();
            for i in 0..N {
                let mut heap = self.heap.borrow_mut();
                let innerref = match &withs[i] {
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
                refs[i].write(Ref{realm: &self, lref});
            }
            unsafe { MaybeUninit::array_assume_init(refs) }
        };

        // Create `refpins: [Pin<&mut Ref>; N]` from `[Ref; N]`:
        let refpins: [Pin<&mut Ref>; N] = {
            let mut lrefs_slice: &mut [Ref] = refs.as_mut_slice();
            let mut refpins: [MaybeUninit<Pin<&mut Ref>>; N] = MaybeUninit::uninit_array();
            let mut i = 0;
            while let Some((head, tail)) = lrefs_slice.split_first_mut() {
                lrefs_slice = tail;
                refpins[i].write(Pin::new(head));
                i += 1;
            }
            unsafe { MaybeUninit::array_assume_init(refpins) }
        };

        // do it!
        let result = action(refpins);

        // refpins are now consumed by `action`, it's safe to clean up `refs`:
        let mut heap = self.heap.borrow_mut();
        for r in refs.as_slice() {
            heap.refstack.pop();
        }

        result
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

    /*
    pub fn with_prop<T, F>(&self, of: Pin<&mut Ref>, name: &str, mut action: F) -> JSResult<T>
        where F: FnMut(Pin<&mut Ref>, Pin<&mut Ref>) -> JSResult<T>
    {
        let lref = {
            let mut heap = self.heap.borrow_mut();
            heap.ref_to_inner(of).to_href();
            }
        }
        let mut var = Ref{realm: &self, lref};
        action(Pin::new(&mut var))
    }
    */

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

    fn innerref(&self, r: Pin<&mut Ref>) -> &InnerRef {
        &self.refstack[r.lref]
    }

    fn innerref_mut(&mut self, r: Pin<&mut Ref>) -> &mut InnerRef {
        &mut self.refstack[r.lref]
    }

    fn ref_to_href(&self, r: Pin<&mut Ref>) -> Option<HRef> {
        match &self.refstack[r.lref] {
            InnerRef(None) => None,
            InnerRef(Some((href, None))) => Some(*href),
            InnerRef(Some((base, Some(name)))) => {
                self.get(*base).get(name.as_str())
            }
        }
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
        *heap.innerref(self) == InnerRef::EMPTY
    }

    pub fn as_value(self: Pin<&mut Self>) -> Option<Value> {
        let heap = self.realm.heap.borrow_mut();
        match heap.innerref(self) {
            InnerRef(Some((href, None))) => {
                heap.get(*href).to_primitive()
            }
            InnerRef(None) => None,     // it's empty, nothing to see here
            InnerRef(_) => None,        // it's an uninitialized attribute
        }
    }

    /*
    pub fn eq(self: Pin<&mut Self>, other: Pin<&mut Self>) -> JSResult<bool> {
        if self.realm.heap != other.realm.heap {
            return Ok(false);
        }
        let heap = self.realm.heap.borrow();
        match (heap.ref_to_href(self), heap.ref_to_href(other)) {
            (None, None) => return true,
            (Some(r1), Some(r2)) if r1 == r2 => return true,
            (Some(r1), Some(r2))
        }
    }

    pub fn as_object(self: Pin<&mut Self>) -> Option<Pin<&mut ObjectRef>> {
        let heap = self.realm.heap.borrow_mut();
        if
    }
    */
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
        realm.with([
            With::Var("global"),
            With::Var("Object"),
            With::Var("Object.prototype"),
            With::Var("Function"),
            With::Var("Function.prototype"),
        ], |[global, object, objproto, function, funcproto]| {
            // assert_eq!( objctor, object )
            Ok(())
        }).unwrap();
    }
}
