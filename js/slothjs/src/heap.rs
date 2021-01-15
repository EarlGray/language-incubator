use std::convert::TryFrom;

use crate::builtin;
use crate::error::Exception;
use crate::ast::{
    DeclarationKind,
    Identifier,
};
use crate::object::{
    Closure,
    Content,
    Interpreted,
    ObjectValue,
    Property,
    JSON,
    JSObject,
    JSValue,
};
use crate::interpret::Interpretable;


/// A heap reference: a Heap index.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct JSRef(usize);


/// Runtime heap
pub struct Heap(Vec<JSObject>);

impl Heap {
    pub const NULL: JSRef = JSRef(0);
    pub const GLOBAL: JSRef = JSRef(1);
    pub const LOCAL: JSRef = JSRef(2);
    pub const OBJECT_PROTO: JSRef = JSRef(3);
    pub const FUNCTION_PROTO: JSRef = JSRef(4);
    pub const ARRAY_PROTO: JSRef = JSRef(5);

    const USERSTART: usize = 6;

    const LOCAL_SCOPE: &'static str = "[[local_scope]]";
    const SAVED_SCOPE: &'static str = "[[saved_scope]]";
    const SCOPE_THIS: &'static str = "[[this]]";

    pub fn new() -> Self {
        let mut heap_vec = Vec::new();
        for _ in 0..Self::USERSTART {
            heap_vec.push(JSObject::new());
        }

        let mut heap = Heap(heap_vec);
        builtin::init(&mut heap)
            .expect("failed to initialize builtin objects");
        heap
    }

    pub fn get(&self, objref: JSRef) -> &JSObject {
        self.0.get(objref.0).unwrap_or_else(|| {
            let msg = format!("{:?} is invalid", objref);
            panic!(msg);
        })
    }

    pub fn get_mut(&mut self, objref: JSRef) -> &mut JSObject {
        self.0.get_mut(objref.0).unwrap_or_else(|| {
            let msg = format!("{:?} is invalid", objref);
            panic!(msg);
        })
    }

    pub fn alloc(&mut self, object: JSObject) -> JSRef {
        let ind = self.0.len();
        self.0.push(object);
        JSRef(ind)
    }

    pub fn allocate(&mut self) -> JSRef {
        self.alloc(JSObject::new())
    }

    fn local_scope(&self) -> Option<JSRef> {
        match self.get(Heap::GLOBAL).property_value(Heap::LOCAL_SCOPE) {
            Some(JSValue::Ref(scope_ref)) => Some(*scope_ref),
            _ => None,
        }
    }

    pub fn scope(&self) -> &JSObject {
        let scope_ref = self.local_scope().unwrap_or(Heap::GLOBAL);
        self.get(scope_ref)
    }

    pub fn scope_mut(&mut self) -> &mut JSObject {
        let scope_ref = self.local_scope().unwrap_or(Heap::GLOBAL);
        self.get_mut(scope_ref)
    }

    /// Variable declaration in the current scope.
    // NOTE: This should not try to assign an initial value.
    pub fn declare_var(&mut self, kind: Option<DeclarationKind>, name: &str) -> Result<(), Exception> {
        // TODO: let and const should be block-scoped
        if !self.scope().properties.contains_key(name) {
            if kind.is_none() {
                self.scope_mut().set_property(name, Content::Value(JSValue::Undefined));
            } else {
                self.scope_mut().set_nonconf(name, Content::Value(JSValue::Undefined));
            }
        }
        Ok(())
    }

    pub fn lookup_var(&mut self, name: &str) -> Option<Interpreted> {
        if let Some(local_scope) = self.local_scope() {
            if self.get(local_scope).properties.contains_key(name) {
                return Some(Interpreted::member(local_scope, name));
            }
        }

        // TODO: lookup free variables of the current call

        if self.get(Heap::GLOBAL).properties.contains_key(name) {
            Some(Interpreted::member(Heap::GLOBAL, name))
        } else {
            None
        }
    }

    fn push_scope(&mut self,
        params: Vec<Identifier>,
        values: Vec<Interpreted>,
        this_ref: JSRef,
    ) -> Result<(), Exception> {
        let old_scope_ref = self.local_scope().unwrap_or(Heap::GLOBAL);

        let mut scope_object = JSObject::new();
        for (i, param) in params.iter().enumerate() {
            let name = &param.0;
            let value = values.get(i).unwrap_or(&Interpreted::VOID).to_value(self)?;
            scope_object.set_nonconf(name, Content::Value(value));
        }
        scope_object.set_system(
            Self::SAVED_SCOPE,
            Content::Value(JSValue::Ref(old_scope_ref)),
        );
        scope_object.set_system(
            Self::SCOPE_THIS,
            Content::Value(JSValue::Ref(this_ref)),
        );

        let new_scope_ref = self.alloc(scope_object);
        self.get_mut(Heap::GLOBAL).set_system(
            Self::LOCAL_SCOPE,
            Content::Value(JSValue::Ref(new_scope_ref)),
        );
        Ok(())
    }

    fn pop_scope(&mut self) -> Result<(), Exception> {
        let this_scope_ref = self.local_scope()
            .expect(".pop_scope without local scope, something is very wrong");
        let this_scope_object = self.get(this_scope_ref);
        let saved_scope_ref = match this_scope_object.properties.get(Self::SAVED_SCOPE) {
            Some(prop) => match prop.content {
                Content::Value(JSValue::Ref(r)) => r,
                _ => panic!(".pop scope without saved scope, something is very wrong")
            },
            other => {
                panic!("saved_scope is invalid: {:?}, something is very wrong", other)
            }
        };

        if saved_scope_ref == Heap::GLOBAL {
            self.get_mut(Heap::GLOBAL).properties.remove(Self::LOCAL_SCOPE);
        } else {
            self.get_mut(Heap::GLOBAL).set_system(
                Self::LOCAL_SCOPE,
                Content::Value(JSValue::Ref(saved_scope_ref)),
            );
        }

        Ok(())
    }

    pub fn lookup_protochain(&self, mut objref: JSRef, propname: &str) -> Option<Interpreted> {
        while objref != Heap::NULL {
            let object = self.get(objref);
            if object.properties.contains_key(propname) {
                return Some(Interpreted::Member{ of: objref, name: propname.to_string() });
            }

            objref = object.proto;
        }
        None
    }

    /// Given a `func_ref` to a closure or a native call and a set of arguments,
    /// executes the function. `this_ref` is bound as `this`.
    pub fn execute(&mut self,
        func_ref: JSRef,
        this_ref: JSRef,
        method_name: &str,
        arguments: Vec<Interpreted>
    ) -> Result<Interpreted, Exception> {
        match self.get(func_ref).value.clone() {
            ObjectValue::VMCall(vmcall) => {
                vmcall.call(this_ref, method_name.to_string(), arguments, self)
            }
            ObjectValue::Closure(closure) => {
                let Closure{params, body, ..} = &*closure;
                self.push_scope(params.clone(), arguments, this_ref)?;
                let result = body.interpret(self);
                self.pop_scope()?;
                match result {
                    Ok(_) => // BlockStatement result
                        Ok(Interpreted::VOID),
                    Err(Exception::JumpReturn(returned)) =>
                        Ok(returned),
                    Err(e) =>
                        Err(e)
                }
            }
            _ => {
                let callee = Interpreted::member(this_ref, method_name);
                return Err(Exception::TypeErrorNotCallable(callee));
            }
        }
    }

    /// Performs a method lookup on the prototype chain of `this_ref` for `method_name`,
    /// then runs a found method via `Heap::execute`, binding `this_ref` to `this`.
    pub fn execute_method(&mut self,
        this_ref: JSRef,
        method_name: &str,
        arguments: Vec<Interpreted>,
    ) -> Result<Interpreted, Exception> {
        let callee = Interpreted::member(this_ref, method_name);
        let (of, name) = match self.lookup_protochain(this_ref, method_name) {
            Some(Interpreted::Member{ of, name }) => (of, name),
            Some(_) => unreachable!(),
            None => return Err(Exception::TypeErrorNotCallable(callee.clone()))
        };
        let funcobj_ref = match self.get(of).properties.get(&name) {
            Some(Property{ content: Content::Value(JSValue::Ref(func_ref)), ..}) => *func_ref,
            _ => return Err(Exception::TypeErrorNotCallable(Interpreted::member(of, &name)))
        };

        self.execute(funcobj_ref, this_ref, method_name, arguments)
    }

    /// Deserializes JSON into objects on the heap
    pub fn object_from_json(&mut self, json: &JSON) -> JSValue {
        if let Some(jobj) = json.as_object() {
            let mut object = JSObject::new();
            for (key, jval) in jobj.iter() {
                let value = self.object_from_json(jval);
                object.set_property(key, Content::Value(value))
            }
            JSValue::Ref(self.alloc(object))
        } else if let Some(jarray) = json.as_array() {
            let storage = jarray.iter().map(|jval|
                self.object_from_json(jval)
            ).collect();
            let object = JSObject::from_array(storage);
            JSValue::Ref(self.alloc(object))
        } else {
            JSValue::try_from(json).expect("primitive JSON")
        }
    }
}
