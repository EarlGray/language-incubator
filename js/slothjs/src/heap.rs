use crate::ast::{
    FunctionDeclaration,
    Identifier,
};
use crate::function::{
    CallContext,
    NativeFunction,
};
use crate::prelude::*;
use crate::{
    builtin,
    object::ObjectValue,
    source,
    Exception,
    Interpretable,
    Interpreted,
    JSObject,
    JSResult,
    JSValue,
    JSON,
};

/// A heap reference: a Heap index.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct JSRef(usize);

impl JSRef {
    pub fn has_proto(&self, protoref: JSRef, heap: &Heap) -> bool {
        (heap.get(*self).protochain(heap)).any(|pref| pref == protoref)
    }

    pub fn isinstance(&self, constructor: JSRef, heap: &Heap) -> JSResult<bool> {
        let protoval = heap.get(constructor).get_own_value("prototype");
        let protoval = protoval.ok_or_else(|| {
            let what = Interpreted::from(constructor);
            Exception::TypeErrorNotCallable(what)
        })?;
        let protoref = protoval.to_ref()?;
        Ok(self.has_proto(protoref, heap))
    }

    /// Check if the object behind the reference `self` has a prototype of `constructor`.
    /// ```
    /// # use slothjs::{JSObject, Heap};
    /// # let mut heap = Heap::new();
    /// let array_ref = heap.alloc(JSObject::from_array(vec![]));
    /// array_ref.expect_instance("Array", &heap).unwrap();
    /// ```
    pub fn expect_instance(&self, constructor: &str, heap: &Heap) -> JSResult<()> {
        let ctrval = heap
            .lookup_var(constructor)
            .ok_or_else(|| Exception::ReferenceNotFound(Identifier::from(constructor)))?;
        let ctrref = ctrval.to_ref(heap)?;
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
#[derive(Debug)]
pub struct Heap {
    objects: Vec<JSObject>,
    pub loc: Option<Box<source::Location>>,
}

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
    //pub const NUMBER_PROTO: JSRef = JSRef(6);
    pub const STRING_PROTO: JSRef = JSRef(7);
    pub const REGEXP_PROTO: JSRef = JSRef(8);

    pub const ERROR_PROTO: JSRef = JSRef(9);

    const USERSTART: usize = 10;

    pub(crate) const SCOPE_THIS: &'static str = "[[this]]";
    const LOCAL_SCOPE: &'static str = "[[local_scope]]";
    pub(crate) const SAVED_SCOPE: &'static str = "[[saved_scope]]";
    const CAPTURED_SCOPE: &'static str = "[[captured_scope]]";

    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        let mut objects = Vec::new();
        for _ in 0..Self::USERSTART {
            objects.push(JSObject::new());
        }

        let mut heap = Heap { objects, loc: None };
        builtin::init(&mut heap).expect("failed to initialize builtin objects");
        heap
    }

    pub fn get(&self, objref: JSRef) -> &JSObject {
        self.objects
            .get(objref.0)
            .unwrap_or_else(|| panic!("{:?} is invalid", objref))
    }

    pub fn get_mut(&mut self, objref: JSRef) -> &mut JSObject {
        self.objects
            .get_mut(objref.0)
            .unwrap_or_else(|| panic!("{:?} is invalid", objref))
    }

    pub fn get_index(&self, index: usize) -> Option<&JSObject> {
        self.objects.get(index)
    }

    pub fn alloc(&mut self, object: JSObject) -> JSRef {
        let ind = self.objects.len();
        self.objects.push(object);
        JSRef(ind)
    }

    pub fn alloc_func(&mut self, func: NativeFunction) -> JSRef {
        let func_obj = JSObject::from_func(func);
        self.alloc(func_obj)
    }

    /// this is a hack to distingiush e.g. `new Boolean(true)` and `Boolean(true)` calls.
    #[allow(clippy::match_like_matches_macro)]
    pub(crate) fn smells_fresh(&self, objref: JSRef) -> bool {
        match objref {
            Heap::NULL => false,
            _ => match self.get(objref) {
                JSObject {
                    value: ObjectValue::None,
                    properties,
                    ..
                } if properties.is_empty() => true,
                _ => false,
            },
        }
    }

    /// Deserializes JSON into objects on the heap
    pub fn object_from_json(&mut self, json: &JSON) -> JSValue {
        if let Some(jobj) = json.as_object() {
            let mut object = JSObject::new();
            for (key, jval) in jobj.iter() {
                let value = self.object_from_json(jval);
                object.set_property(key, value).unwrap();
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

    /// Find out what `this` currently is.
    pub fn interpret_this(&mut self) -> JSRef {
        self.lookup_var(Self::SCOPE_THIS)
            .expect("no this in the current scope")
            .to_ref(self)
            .expect("this must be JSValue::Ref")
    }

    pub(crate) fn is_scope(&self, objref: JSRef) -> bool {
        objref == Self::GLOBAL || self.get(objref).get_own_value(Heap::SAVED_SCOPE).is_some()
    }

    /// If there's a local scope, return a `JSRef` to it.
    pub(crate) fn local_scope(&self) -> Option<JSRef> {
        match self.get(Heap::GLOBAL).get_own_value(Heap::LOCAL_SCOPE) {
            Some(JSValue::Ref(scope_ref)) => Some(scope_ref),
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

    fn declare_variable(&mut self, var: &Identifier) -> JSResult<()> {
        let name = var.as_str();
        if !self.scope().properties.contains_key(name) {
            self.scope_mut().set_nonconf(name, JSValue::Undefined)?;
        }
        Ok(())
    }

    pub fn declare<'a>(
        &mut self,
        variables: impl Iterator<Item = &'a Identifier>,
        functions: impl Iterator<Item = &'a FunctionDeclaration>,
    ) -> JSResult<()> {
        for var in variables {
            self.declare_variable(var)?;
        }
        for func in functions {
            let name = &func.id;
            self.declare_variable(name)?;

            let closure = func.function.interpret(self)?;
            let closure = closure.to_value(self)?;
            self.scope_mut().set_property(name.as_str(), closure)?;
        }
        Ok(())
    }

    pub fn lookup_var(&self, name: &str) -> Option<Interpreted> {
        if let Some(local_ref) = self.local_scope() {
            let local = self.get(local_ref);
            if local.get_own_value(name).is_some() {
                return Some(Interpreted::member(local_ref, name));
            }

            // captured scopes lookup
            let mut scope_ref = match local.get_own_value(Self::CAPTURED_SCOPE) {
                Some(JSValue::Ref(scope_ref)) => scope_ref,
                _ => Heap::NULL,
            };
            while scope_ref != Heap::NULL {
                let scope = self.get(scope_ref);
                if scope.get_own_value(name).is_some() {
                    return Some(Interpreted::member(scope_ref, name));
                }

                scope_ref = match scope.get_own_value(Self::CAPTURED_SCOPE) {
                    Some(JSValue::Ref(scope_ref)) => scope_ref,
                    _ => Heap::NULL,
                };
            }
        }

        self.get(Heap::GLOBAL)
            .get_own_value(name)
            .map(|_| Interpreted::member(Heap::GLOBAL, name))
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
    pub fn lookup_path(&self, mut names: &[&str]) -> JSResult<Interpreted> {
        let mut scoperef = self.local_scope().unwrap_or(Heap::GLOBAL);
        while let Some((name, rest)) = names.split_first() {
            names = rest;
            let nameval = self.get(scoperef).get_own_value(name).ok_or_else(|| {
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
    ) -> JSResult<T>
    where
        F: FnMut(&mut Heap) -> JSResult<T>,
    {
        self.push_scope(this_ref)?;
        if captured_scope != Heap::NULL {
            self.scope_mut()
                .set_system(Self::CAPTURED_SCOPE, captured_scope)?;
        }
        let result = action(self);
        self.pop_scope()?;
        result
    }

    fn push_scope(&mut self, this_ref: JSRef) -> JSResult<JSRef> {
        let old_scope_ref = self.local_scope().unwrap_or(Heap::GLOBAL);

        let mut scope_object = JSObject::new();

        scope_object.set_system(Self::SAVED_SCOPE, old_scope_ref)?;
        scope_object.set_system(Self::SCOPE_THIS, this_ref)?;

        let new_scope_ref = self.alloc(scope_object);
        self.get_mut(Heap::GLOBAL)
            .set_even_nonwritable(Self::LOCAL_SCOPE, new_scope_ref)?;
        Ok(new_scope_ref)
    }

    fn pop_scope(&mut self) -> JSResult<()> {
        let this_scope_ref = self.local_scope().expect(".pop_scope without local scope"); // yes, panic, this interpreter is broken.
        let this_scope_object = self.get(this_scope_ref);
        let saved_scope_ref = (this_scope_object.properties)
            .get(Self::SAVED_SCOPE)
            .and_then(|prop| prop.to_ref())
            .expect("saved scope is not a reference"); // yes, panic, this interpreter is broken.

        let global = self.get_mut(Heap::GLOBAL);
        if saved_scope_ref == Heap::GLOBAL {
            global.properties.remove(Self::LOCAL_SCOPE);
        } else {
            global.set_even_nonwritable(Self::LOCAL_SCOPE, saved_scope_ref)?;
        }

        Ok(())
    }

    /// Find the location of `propname` on the prototype chain of `objref`.
    /// Return `None` or `Some(Interpreted::Member{..})` pointing to the found own property.
    pub fn lookup_protochain(&self, mut objref: JSRef, propname: &str) -> Option<Interpreted> {
        while objref != Heap::NULL {
            let object = self.get(objref);
            if object.get_own_value(propname).is_some() {
                return Some(Interpreted::member(objref, propname));
            }

            objref = object.proto;
        }
        None
    }

    /// A shortcut for `interpretable.evaluate(&mut heap)`.
    pub fn evaluate<T: Interpretable>(&mut self, interpretable: &T) -> JSResult<JSValue> {
        interpretable.interpret(self)?.to_value(self)
    }

    /// Given a `func_ref` to a closure or a native call and a set of arguments,
    /// executes the function. `this_ref` is bound as `this`.
    pub fn execute(&mut self, func_ref: JSRef, mut call: CallContext) -> JSResult<Interpreted> {
        if call.loc.as_ref().is_none() {
            call.loc = self.loc.clone();
        }
        // Yes, we do need a clone() to workaround borrow checker:
        match &self.get(func_ref).value {
            ObjectValue::VMCall(vmcall) => vmcall.clone().call(call, self),
            ObjectValue::Closure(closure) => closure.clone().call(call, self),
            _ => {
                let callee = Interpreted::Member {
                    of: call.this_ref,
                    name: call.method_name,
                };
                Err(Exception::TypeErrorNotCallable(callee))
            }
        }
    }

    pub fn throw<T>(&self, exc: Exception) -> JSResult<T> {
        // TODO: capture the stack
        //let _ = source::print_callstack(self);
        Err(exc)
    }
}
