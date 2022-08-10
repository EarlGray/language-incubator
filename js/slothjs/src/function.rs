use crate::prelude::*;

use crate::{
    ast,
    source,
    Exception,
    Heap,
    Interpretable,
    Interpreted,
    JSObject,
    JSRef,
    JSResult,
    JSValue,
};

/// Call context information (e.g. arguments) for [`Heap::execute()`].
///
/// Whenever you want to call a JS function, it needs to know:
///
/// - `arguments`
/// - `this_ref`: what `this` is for this call
///
/// The regular usage is:
/// ```
/// # use slothjs::{Heap, JSRef, JSValue, JSON, Interpreted, CallContext};
/// # let mut heap = Heap::new();
/// let func_ref: JSRef = heap
///     .lookup_var("parseInt").expect("parseInt")
///     .to_ref(&heap).expect("to_ref");
///
/// let arguments = vec![ Interpreted::from("42") ];
/// let result = heap.execute( func_ref, CallContext::from(arguments)).expect("execute");
/// let result = result.to_value(&heap).unwrap();
///
/// assert_eq!(result, JSValue::from(42));
/// ```
///
pub struct CallContext {
    pub this_ref: JSRef,
    pub method_name: JSString,
    pub arguments: Vec<Interpreted>,
    pub loc: Option<Box<source::Location>>,
}

impl CallContext {
    pub fn with_this(mut self, this_ref: JSRef) -> Self {
        self.this_ref = this_ref;
        self
    }

    pub fn with_name(mut self, name: JSString) -> Self {
        self.method_name = name;
        self
    }

    pub fn arg_value(&self, index: usize, heap: &mut Heap) -> JSResult<JSValue> {
        self.arguments
            .get(index)
            .unwrap_or(&Interpreted::VOID)
            .to_value(heap)
    }

    pub fn arg_as_number(&self, argnum: usize, heap: &Heap) -> JSResult<Option<i64>> {
        let arg = match self.arguments.get(argnum) {
            Some(arg) => arg.to_value(heap)?,
            None => return Ok(None),
        };
        Ok(Some(arg.numberify(heap).unwrap_or(0.0) as i64))
    }
}

impl From<Vec<Interpreted>> for CallContext {
    fn from(arguments: Vec<Interpreted>) -> CallContext {
        CallContext {
            arguments,
            method_name: JSString::from(""),
            loc: None,
            this_ref: Heap::NULL,
        }
    }
}

/// What a host function callable from JS looks like.
pub type HostFn = fn(ctx: CallContext, heap: &'_ mut Heap) -> JSResult<Interpreted>;

/// A wrapper for [`HostFn`] to give it `fmt::Debug`.
#[derive(Clone)]
pub struct HostFunc(HostFn);

impl HostFunc {
    pub fn call(self, call: CallContext, heap: &mut Heap) -> JSResult<Interpreted> {
        self.0(call, heap)
    }
    pub fn ptr(&self) -> usize {
        self.0 as *const () as usize
    }
}

impl From<HostFn> for HostFunc {
    fn from(func: HostFn) -> Self {
        Self(func)
    }
}

impl fmt::Debug for HostFunc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "VMCall(*{:x})", self.ptr())
    }
}

#[derive(Clone, Debug)]
pub struct Closure {
    pub function: Rc<ast::Function>,
    pub captured_scope: JSRef, // TODO: capture free variables only
}

impl Closure {
    pub fn call(&self, call: CallContext, heap: &mut Heap) -> JSResult<Interpreted> {
        let result = heap.enter_new_scope(call.this_ref, self.captured_scope, |heap| {
            // `arguments`
            let argv = (call.arguments.iter())
                .map(|v| v.to_value(heap))
                .collect::<JSResult<Vec<JSValue>>>()?;
            let arguments_ref = heap.alloc(JSObject::from_array(argv));
            heap.scope_mut().set_nonconf("arguments".into(), arguments_ref)?;

            // set each argument
            for (i, param) in self.function.params.iter().enumerate() {
                let value = (call.arguments.get(i))
                    .unwrap_or(&Interpreted::VOID)
                    .to_value(heap)?;
                heap.scope_mut().set_nonconf(param.0.clone(), value)?;
            }

            let _ = source::save_caller(call.loc.clone(), heap);

            heap.declare(
                self.function.variables.iter(),
                self.function.functions.iter(),
            )?;

            self.function.body.interpret(heap)
        });
        match result {
            Ok(_) => Ok(Interpreted::VOID), // BlockStatement result
            Err(Exception::JumpReturn(returned)) => Ok(returned),
            Err(e) => Err(e),
        }
    }
}
