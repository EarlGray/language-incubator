use std::collections::HashSet;
use std::convert::TryFrom;

use crate::ast::Identifier;
use crate::builtin;
use crate::error::Exception;
use crate::function::CallContext;
use crate::object::{
    Access,
    Content,
    Interpreted,
    JSObject,
    JSValue,
    ObjectValue,
    JSON,
};

/// A heap reference: a Heap index.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct JSRef(usize);

impl JSRef {
    pub fn isinstance(&self, constructor: JSRef, heap: &Heap) -> Result<bool, Exception> {
        let protoval = heap.get(constructor).get_value("prototype");
        let protoval = protoval.ok_or_else(|| {
            let what = Interpreted::from(constructor);
            Exception::TypeErrorNotCallable(what)
        })?;
        let protoref = protoval.to_ref()?;

        let found = (heap.get(*self))
            .protochain(heap)
            .find(|pref| *pref == protoref)
            .is_some();
        Ok(found)
    }

    /// Check if the object behind the reference `self` has a prototype of `constructor`.
    /// ```
    /// # use slothjs::{JSObject, Heap};
    /// # let mut heap = Heap::new();
    /// let array_ref = heap.alloc(JSObject::from_array(vec![]));
    /// array_ref.expect_instance("Array", &heap).unwrap();
    /// ```
    pub fn expect_instance(&self, constructor: &str, heap: &Heap) -> Result<(), Exception> {
        let ctrval = heap.scope().get_value(constructor);
        let ctrval = ctrval.ok_or(Exception::ReferenceNotFound(Identifier::from(constructor)))?;

        let ctrref = ctrval.to_ref()?;
        match self.isinstance(ctrref, heap)? {
            true => Ok(()),
            false => {
                let what = Interpreted::from(*self);
                let of = constructor.to_string();
                Err(Exception::TypeErrorInstanceRequired(what, of))
            }
        }
    }
}

/// Runtime heap
pub struct Heap(Vec<JSObject>);

impl Heap {
    // A set of fixed slots on the heap.
    // This untangles the builtins intialization and avoids frequent lookups
    // for e.g. `Array.prototype`.
    pub const NULL: JSRef = JSRef(0);
    pub const GLOBAL: JSRef = JSRef(1);
    pub const OBJECT_PROTO: JSRef = JSRef(2);
    pub const FUNCTION_PROTO: JSRef = JSRef(3);
    pub const ARRAY_PROTO: JSRef = JSRef(4);
    pub const BOOLEAN_PROTO: JSRef = JSRef(5);
    const USERSTART: usize = 6;

    pub(crate) const SCOPE_THIS: &'static str = "[[this]]";
    const LOCAL_SCOPE: &'static str = "[[local_scope]]";
    const SAVED_SCOPE: &'static str = "[[saved_scope]]";
    const CAPTURED_SCOPE: &'static str = "[[captured_scope]]";

    pub fn new() -> Self {
        let mut heap_vec = Vec::new();
        for _ in 0..Self::USERSTART {
            heap_vec.push(JSObject::new());
        }

        let mut heap = Heap(heap_vec);
        builtin::init(&mut heap).expect("failed to initialize builtin objects");
        heap
    }

    pub fn get(&self, objref: JSRef) -> &JSObject {
        self.0
            .get(objref.0)
            .unwrap_or_else(|| panic!("{:?} is invalid", objref))
    }

    pub fn get_mut(&mut self, objref: JSRef) -> &mut JSObject {
        self.0
            .get_mut(objref.0)
            .unwrap_or_else(|| panic!("{:?} is invalid", objref))
    }

    pub fn alloc(&mut self, object: JSObject) -> JSRef {
        let ind = self.0.len();
        self.0.push(object);
        JSRef(ind)
    }

    /// this is a hack to distingiush e.g. `new Boolean(true)` and `Boolean(true)` calls.
    pub(crate) fn smells_fresh(&self, objref: JSRef) -> bool {
        match objref {
            Heap::NULL => false,
            _ => match self.get(objref) {
                JSObject {
                    value: ObjectValue::None,
                    properties,
                    ..
                } if properties.len() == 0 => true,
                _ => false,
            },
        }
    }

    pub(crate) fn is_scope(&self, objref: JSRef) -> bool {
        objref == Self::GLOBAL || self.get(objref).get_value(Heap::SAVED_SCOPE).is_some()
    }

    /// Find out what `this` currently is.
    pub fn interpret_this(&mut self) -> Result<Interpreted, Exception> {
        let this_ref = self
            .lookup_var(Self::SCOPE_THIS)
            .expect("no this in the current scope")
            .to_ref(self)?;
        Ok(Interpreted::from(this_ref))
    }

    /// If there's a local scope, return a `JSRef` to it.
    pub(crate) fn local_scope(&self) -> Option<JSRef> {
        match self.get(Heap::GLOBAL).get_value(Heap::LOCAL_SCOPE) {
            Some(JSValue::Ref(scope_ref)) => Some(*scope_ref),
            _ => None,
        }
    }

    fn scope(&self) -> &JSObject {
        let scope_ref = self.local_scope().unwrap_or(Heap::GLOBAL);
        self.get(scope_ref)
    }

    pub fn scope_mut(&mut self) -> &mut JSObject {
        let scope_ref = self.local_scope().unwrap_or(Heap::GLOBAL);
        self.get_mut(scope_ref)
    }

    pub fn declare_variables(&mut self, variables: &HashSet<Identifier>) -> Result<(), Exception> {
        for var in variables.iter() {
            let name = var.as_str();
            if !self.scope().properties.contains_key(name) {
                let content = Content::Value(JSValue::Undefined);
                self.scope_mut().set(name, content, Access::NONCONF)?;
            }
        }
        Ok(())
    }

    pub fn lookup_var(&mut self, name: &str) -> Option<Interpreted> {
        if let Some(local_ref) = self.local_scope() {
            // local scope lookup
            let local = self.get(local_ref);
            if local.properties.contains_key(name) {
                return Some(Interpreted::member(local_ref, name));
            }

            // captured scopes lookup
            let mut scope_ref = (local.properties)
                .get(Self::CAPTURED_SCOPE)
                .and_then(|prop| prop.to_ref())
                .unwrap_or(Heap::NULL);
            while scope_ref != Heap::NULL {
                let scope = self.get(scope_ref);
                if scope.properties.contains_key(name) {
                    return Some(Interpreted::member(scope_ref, name));
                }

                scope_ref = (scope.properties)
                    .get(Self::CAPTURED_SCOPE)
                    .and_then(|prop| prop.to_ref())
                    .unwrap_or(Heap::NULL);
            }
        }

        // global scope lookup
        if self.get(Heap::GLOBAL).properties.contains_key(name) {
            Some(Interpreted::member(Heap::GLOBAL, name))
        } else {
            None
        }
    }

    /// Lookup a property chain starting from the current scope, e.g.
    /// ```
    /// # use slothjs::{Heap, Interpreted};
    /// # let mut heap = Heap::new();
    /// assert_eq!(
    ///     heap.lookup_path(&["Array", "prototype"]).unwrap(),
    ///     Interpreted::from(Heap::ARRAY_PROTO)
    /// );
    /// ```
    pub fn lookup_path(&self, mut names: &[&str]) -> Result<Interpreted, Exception> {
        let mut scoperef = self.local_scope().unwrap_or(Heap::GLOBAL);
        while let Some((name, rest)) = names.split_first() {
            names = rest;
            let nameval = self.get(scoperef).get_value(name).ok_or_else(|| {
                let what = Interpreted::from(scoperef);
                Exception::TypeErrorGetProperty(what, name.to_string())
            })?;
            scoperef = nameval.to_ref()?;
        }
        Ok(Interpreted::from(scoperef))
    }

    pub fn enter_new_scope<T, F>(
        &mut self,
        this_ref: JSRef,
        captured_scope: JSRef,
        mut action: F,
    ) -> Result<T, Exception>
    where
        F: FnMut(&mut Heap, JSRef) -> Result<T, Exception>,
    {
        let scoperef = self.push_scope(this_ref)?;
        if captured_scope != Heap::NULL {
            self.get_mut(scoperef)
                .set_system(Self::CAPTURED_SCOPE, Content::from(captured_scope))?;
        }
        let result = action(self, scoperef);
        self.pop_scope()?;
        result
    }

    fn push_scope(&mut self, this_ref: JSRef) -> Result<JSRef, Exception> {
        let old_scope_ref = self.local_scope().unwrap_or(Heap::GLOBAL);

        let mut scope_object = JSObject::new();

        scope_object.set_system(Self::SAVED_SCOPE, Content::from(old_scope_ref))?;
        scope_object.set_system(Self::SCOPE_THIS, Content::from(this_ref))?;

        let new_scope_ref = self.alloc(scope_object);
        self.get_mut(Heap::GLOBAL)
            .update_even_nonwritable(Self::LOCAL_SCOPE, JSValue::Ref(new_scope_ref))?;
        Ok(new_scope_ref)
    }

    fn pop_scope(&mut self) -> Result<(), Exception> {
        let this_scope_ref = self.local_scope().expect(".pop_scope without local scope"); // yes, panic, this interpreter is broken.
        let this_scope_object = self.get(this_scope_ref);
        let saved_scope_ref = this_scope_object
            .properties
            .get(Self::SAVED_SCOPE)
            .and_then(|prop| prop.to_ref())
            .expect("saved scope is not a reference"); // yes, panic, this interpreter is broken.

        let global = self.get_mut(Heap::GLOBAL);
        if saved_scope_ref == Heap::GLOBAL {
            global.properties.remove(Self::LOCAL_SCOPE);
        } else {
            global.update_even_nonwritable(Self::LOCAL_SCOPE, JSValue::Ref(saved_scope_ref))?;
        }

        Ok(())
    }

    /// Find the location of `propname` on the prototype chain of `objref`.
    /// Return `None` or `Some(Interpreted::Member{..})` pointing to the found own property.
    pub fn lookup_protochain(&self, mut objref: JSRef, propname: &str) -> Option<Interpreted> {
        while objref != Heap::NULL {
            let object = self.get(objref);
            if object.get_value(propname).is_some() {
                return Some(Interpreted::member(objref, propname));
            }

            objref = object.proto;
        }
        None
    }

    /// Given a `func_ref` to a closure or a native call and a set of arguments,
    /// executes the function. `this_ref` is bound as `this`.
    pub fn execute(
        &mut self,
        func_ref: JSRef,
        call: CallContext,
    ) -> Result<Interpreted, Exception> {
        // Yes, we do need a clone() to workaround borrow checker:
        match &self.get(func_ref).value {
            ObjectValue::VMCall(vmcall) => vmcall.clone().call(call, self),
            ObjectValue::Closure(closure) => closure.clone().call(call, self),
            _ => {
                let callee = Interpreted::Member {
                    of: call.this_ref,
                    name: call.method_name,
                };
                return Err(Exception::TypeErrorNotCallable(callee));
            }
        }
    }

    /// Deserializes JSON into objects on the heap
    pub fn object_from_json(&mut self, json: &JSON) -> JSValue {
        if let Some(jobj) = json.as_object() {
            let mut object = JSObject::new();
            for (key, jval) in jobj.iter() {
                let value = self.object_from_json(jval);
                object.set_property(key, Content::Value(value)).unwrap();
            }
            JSValue::Ref(self.alloc(object))
        } else if let Some(jarray) = json.as_array() {
            let storage = (jarray.iter())
                .map(|jval| self.object_from_json(jval))
                .collect();
            let object = JSObject::from_array(storage);
            JSValue::Ref(self.alloc(object))
        } else {
            JSValue::try_from(json).expect("primitive JSON") // not Object/Array, must be primitive
        }
    }
}
