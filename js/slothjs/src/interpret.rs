use crate::object;
use crate::object::{
    Content,
    Heap,
    Interpreted,
    JSObject,
    JSRef,
    JSValue,
};
use crate::error::Exception;
use crate::ast::*;      // yes, EVERYTHING


// ==============================================

pub struct RuntimeState {
    pub heap: Heap,
}

impl RuntimeState {
    const LOCAL_SCOPE: &'static str = "[[local_scope]]";
    const SAVED_SCOPE: &'static str = "[[saved_scope]]";
    const SCOPE_THIS: &'static str = "[[this]]";

    pub fn new() -> Self {
        RuntimeState{ heap: Heap::new() }
    }

    /// Return a JSRef to the current scope.
    fn scope_ref(&self) -> JSRef {
        self.heap.lookup_ref(&[Self::LOCAL_SCOPE]).unwrap_or(Heap::GLOBAL)
    }

    /// Return a reference to the current scope.
    fn scope(&self) -> &JSObject {
        let scope = self.scope_ref();
        self.heap.object(scope)
            .expect("current scope is not an object, something is wrong")
    }

    /// Return a mutable reference to the current scope.
    fn scope_mut(&mut self) -> &mut JSObject {
        let scope = self.scope_ref();
        self.heap.object_mut(scope)
            .expect("current scope is not an object, something is wrong")
    }

    /// Variable declaration in the current scope.
    // NOTE: This should not try to assign an initial value.
    fn declare_var(&mut self, kind: Option<DeclarationKind>, name: &str) -> Result<(), Exception> {
        // TODO: let and const should be block-scoped
        if !self.scope().properties.contains_key(name) {
            let valueref = self.heap.allocate(JSValue::Undefined);
            if kind.is_none() {
                self.scope_mut().set_property_ref(name, valueref);
            } else {
                self.scope_mut().set_nonconf(name, Content::Data(valueref));
            }
        }
        Ok(())
    }

    fn lookup_var(&mut self, name: &str) -> Option<Interpreted> {
        if let Ok(local_scope) = self.heap.lookup_ref(&[Self::LOCAL_SCOPE]) {
            let scope_object = self.heap.object(local_scope)
                .expect("scope is not an object, something is messed up");

            if scope_object.properties.contains_key(name) {
                let of = Box::new(Interpreted::Ref(local_scope));
                let found = Interpreted::Member{ of, name: name.to_string() };
                return Some(found);
            }
        }

        // TODO: lookup free variables of the current call

        if self.heap.global().properties.contains_key(name) {
            let of = Box::new(Interpreted::Ref(Heap::GLOBAL));
            Some(Interpreted::Member{ of, name: name.to_string() })
        } else {
            None
        }
    }

    fn push_scope(&mut self,
        params: Vec<Identifier>,
        values: Vec<Interpreted>,
        this_ref: JSRef,
    ) -> Result<(), Exception> {
        let old_scope_ref = self.scope_ref();

        let mut scope_object = JSObject::new();
        for (i, param) in params.iter().enumerate() {
            let name = &param.0;
            let value = values.get(i).unwrap_or(&Interpreted::VOID);
            let paramref = value.to_ref_or_allocate(&mut self.heap)?;
            scope_object.set_property_ref(name, paramref);
        }
        scope_object.set_system(
            Self::SAVED_SCOPE,
            Content::Data(old_scope_ref),
        );
        scope_object.set_system(
            Self::SCOPE_THIS,
            Content::Data(this_ref),
        );
        let new_scope_ref = self.heap.allocate(JSValue::Object(scope_object));

        self.heap.global_mut().set_system(
            Self::LOCAL_SCOPE,
            Content::Data(new_scope_ref),
        );
        Ok(())
    }

    fn pop_scope(&mut self) -> Result<(), Exception> {
        let this_scope_ref = self.heap.lookup_ref(&[Self::LOCAL_SCOPE])
            .expect(".pop_scope without local scope, something is very wrong");
        let this_scope_object = self.heap.get(this_scope_ref).to_object()?;
        let saved_scope_ref = this_scope_object.property_ref(Self::SAVED_SCOPE)
            .expect(".pop scope without saved scope, something is very wrong");

        if saved_scope_ref == Heap::GLOBAL {
            self.heap.global_mut().properties.remove(Self::LOCAL_SCOPE);
        } else {
            self.heap.global_mut().set_system(
                Self::LOCAL_SCOPE,
                Content::Data(saved_scope_ref),
            );
        }

        Ok(())
    }
}

// ==============================================

pub trait Interpretable {
    fn interpret(&self, state: &mut RuntimeState) -> Result<Interpreted, Exception>;
}


// ==============================================

impl Interpretable for Program {
    fn interpret(&self, state: &mut RuntimeState) -> Result<Interpreted, Exception> {
        let mut result = Interpreted::VOID;
        for stmt in self.body.iter() {
            result = stmt.interpret(state)?;
        }
        Ok(result)
    }
}

// ==============================================

impl Interpretable for Statement {
    fn interpret(&self, state: &mut RuntimeState) -> Result<Interpreted, Exception> {
        match self {
            Statement::Empty                        => Ok(Interpreted::VOID),
            Statement::Expr(stmt)                   => stmt.interpret(state),
            Statement::Block(stmt)                  => stmt.interpret(state),
            Statement::If(stmt)                     => stmt.interpret(state),
            Statement::For(stmt)                    => stmt.interpret(state),
            Statement::Return(stmt)                 => stmt.interpret(state),
            Statement::VariableDeclaration(stmt)    => stmt.interpret(state),
        }
    }
}


// ==============================================

impl Interpretable for BlockStatement {
    fn interpret(&self, state: &mut RuntimeState) -> Result<Interpreted, Exception> {
        for stmt in self.body.iter() {
            stmt.interpret(state)?;
        }
        Ok(Interpreted::VOID)
    }
}

impl Interpretable for IfStatement {
    fn interpret(&self, state: &mut RuntimeState) -> Result<Interpreted, Exception> {
        let jbool = self.test.interpret(state)?;
        let cond = jbool.to_value(&state.heap)?;
        if cond.boolify() {
            self.consequent.interpret(state)
        } else if let Some(else_stmt) = self.alternate.as_ref() {
            else_stmt.interpret(state)
        } else {
            Ok(Interpreted::VOID)
        }
    }
}

impl Interpretable for ForStatement {
    fn interpret(&self, state: &mut RuntimeState) -> Result<Interpreted, Exception> {
        self.init.interpret(state)?;
        loop {
            // test
            let testval = match self.test.as_ref() {
                None => true,
                Some(testexpr) => {
                    let result = testexpr.interpret(state)?;
                    result.to_value(&state.heap)?.boolify()
                }
            };
            if !testval { break }

            // body
            self.body.interpret(state)?;

            // update
            if let Some(updateexpr) = self.update.as_ref() {
                updateexpr.interpret(state)?;
            }
        }
        Ok(Interpreted::VOID)
    }
}

impl Interpretable for ExpressionStatement {
    fn interpret(&self, state: &mut RuntimeState) -> Result<Interpreted, Exception> {
        let result = self.expression.interpret(state)?;
        let value = result.to_value(&state.heap)?;
        Ok(Interpreted::Value(value))
    }
}

impl Interpretable for ReturnStatement {
    fn interpret(&self, state: &mut RuntimeState) -> Result<Interpreted, Exception> {
        let ReturnStatement(argument) = self;
        let returned = match argument {
            None => Interpreted::VOID,
            Some(argexpr) => argexpr.interpret(state)?,
        };
        Err(Exception::JumpReturn(returned))
    }
}

impl Interpretable for VariableDeclaration {
    fn interpret(&self, state: &mut RuntimeState) -> Result<Interpreted, Exception> {
        for decl in self.declarations.iter() {
            let optinit = decl.init.as_ref()
                .map(|initexpr| initexpr.interpret(state))
                .transpose()?;
            let name = &decl.name;
            state.declare_var(Some(self.kind), name)?;
            if let Some(init) = optinit {
                let scope_ref = state.scope_ref();
                state.heap.property_assign(scope_ref, name, &init)?;
            }
        }
        Ok(Interpreted::VOID)
    }
}

impl Interpretable for Expr {
    fn interpret(&self, state: &mut RuntimeState) -> Result<Interpreted, Exception> {
        match self {
            Expr::Literal(expr) =>              expr.interpret(state),
            Expr::Identifier(expr) =>           expr.interpret(state),
            Expr::BinaryOp(expr) =>             expr.interpret(state),
            Expr::Call(expr) =>                 expr.interpret(state),
            Expr::Array(_expr) =>               todo!(),
            Expr::Member(expr) =>               expr.interpret(state),
            Expr::Object(expr) =>               expr.interpret(state),
            Expr::Assign(expr) =>               expr.interpret(state),
            Expr::Conditional(expr) =>          expr.interpret(state),
            Expr::Unary(expr) =>                expr.interpret(state),
            Expr::Function(expr) =>             expr.interpret(state),
        }
    }
}


impl Interpretable for Literal {
    fn interpret(&self, state: &mut RuntimeState) -> Result<Interpreted, Exception> {
        let value = state.heap.object_from_json(&self.0);
        Ok(Interpreted::Value(value))
    }
}

impl Interpretable for Identifier {
    fn interpret(&self, state: &mut RuntimeState) -> Result<Interpreted, Exception> {
        let name = &self.0;
        state.lookup_var(name)
            .ok_or(Exception::ReferenceNotFound(name.to_string()))
    }
}

impl Interpretable for ConditionalExpression {
    fn interpret(&self, state: &mut RuntimeState) -> Result<Interpreted, Exception> {
        let ConditionalExpression(condexpr, thenexpr, elseexpr) = self;
        let cond = condexpr.interpret(state)?.to_value(&state.heap)?;
        if cond.boolify() {
            thenexpr.interpret(state)
        } else {
            elseexpr.interpret(state)
        }
    }
}

impl Interpretable for BinaryExpression {
    fn interpret(&self, state: &mut RuntimeState) -> Result<Interpreted, Exception> {
        let BinaryExpression(lexpr, op, rexpr) = self;
        let lval = lexpr.interpret(state)?.to_value(&state.heap)?;
        let rval = rexpr.interpret(state)?.to_value(&state.heap)?;
        let result = match op {
            BinOp::EqEq => JSValue::from(lval.loose_eq(&rval)),
            BinOp::NotEq => JSValue::from(!lval.loose_eq(&rval)),
            BinOp::Less => JSValue::less(&lval, &rval, &state.heap),
            BinOp::Plus => JSValue::plus(&lval, &rval, &state.heap),
            BinOp::Minus => JSValue::minus(&lval, &rval, &state.heap),
            BinOp::Star => JSValue::numerically(&lval, &rval, |a, b| a * b),
        };
        Ok(Interpreted::Value(result))
    }
}

impl Interpretable for UnaryExpression {
    fn interpret(&self, state: &mut RuntimeState) -> Result<Interpreted, Exception> {
        let UnaryExpression(op, argexpr) = self;
        let arg = argexpr.interpret(state)?;
        let argvalue = arg.to_value(&state.heap)?;
        let value = match op {
            UnOp::Delete => JSValue::from(arg.delete(&mut state.heap).is_ok()),
            UnOp::Exclamation => JSValue::Bool(!argvalue.boolify()),
            UnOp::Minus => JSValue::Number(- argvalue.numberify().unwrap_or(f64::NAN)),
            UnOp::Plus => JSValue::Number(argvalue.numberify().unwrap_or(f64::NAN)),
            UnOp::Typeof => JSValue::from(argvalue.type_of()),
            UnOp::Tilde => {
                let num = argvalue.numberify().unwrap_or(f64::NAN);
                let num = if f64::is_nan(num) { 0.0 } else { num };
                JSValue::from(-(1.0 + num))
            }
            UnOp::Void => JSValue::Undefined,
        };
        Ok(Interpreted::Value(value))
    }
}

impl Interpretable for MemberExpression {
    fn interpret(&self, state: &mut RuntimeState) -> Result<Interpreted, Exception> {
        let MemberExpression(objexpr, propexpr, computed) = self;

        // compute the name of the property:
        let propname = if *computed {
            let propval = propexpr.interpret(state)?.to_value(&state.heap)?;
            propval.stringify(&state.heap)
        } else {
            match &**propexpr {
                Expr::Identifier(name) => name.0.clone(),
                _ => panic!("Member(computed=false) property is not an identifier")
            }
        };

        // get the object reference for member computation:
        let objresult = objexpr.interpret(state)?;
        let objref = objresult.to_ref(&state.heap).map_err(|_| {
            Exception::ReferenceNotAnObject(objresult.clone())
        })?;

        let result = Interpreted::Member{
            of: Box::new(Interpreted::Ref(objref)),
            name: propname.to_string()
        };
        Ok(result)
    }
}

impl Interpretable for ObjectExpression {
    fn interpret(&self, state: &mut RuntimeState) -> Result<Interpreted, Exception> {
        let mut object = JSObject::new();

        let object_proto_ref = state.heap.lookup_ref(&["Object", "prototype"])?;
        object.set_system(
            JSObject::PROTO,
            Content::Data(object_proto_ref),
        );

        for (key, valexpr) in self.0.iter() {
            let keyname = match key {
                ObjectKey::Identifier(ident) =>
                    ident.clone(),
                ObjectKey::Computed(expr) => {
                    let result = expr.interpret(state)?.to_value(&state.heap)?;
                    result.stringify(&mut state.heap)
                }
            };
            let valresult = valexpr.interpret(state)?;
            let propref = valresult.to_ref_or_allocate(&mut state.heap)?;
            object.set_property_ref(&keyname, propref);
        }

        Ok(Interpreted::Value(JSValue::Object(object)))
    }
}

impl Interpretable for AssignmentExpression {
    fn interpret(&self, state: &mut RuntimeState) -> Result<Interpreted, Exception> {
        let AssignmentExpression(leftexpr, AssignOp(modop), valexpr) = self;

        // this must happen before a possible declare_var()
        let value = valexpr.interpret(state)?;

        if let Some(op) = modop {
            let assignee = leftexpr.interpret(state)?;
            let objref = assignee.to_ref(&state.heap)?;
            let oldvalue = state.heap.get(objref);
            let value = value.to_value(&state.heap)?;
            let newvalue = match op {
                BinOp::Plus => JSValue::plus(oldvalue, &value, &state.heap),
                BinOp::Minus => JSValue::minus(oldvalue, &value, &state.heap),
                BinOp::Star => JSValue::numerically(oldvalue, &value, |a, b| a * b),
                _ => panic!(format!("Binary operation {:?} cannot be used in assignment", op))
            };
            *state.heap.get_mut(objref) = newvalue.clone();
            Ok(Interpreted::Value(newvalue))
        } else {
            if let Expr::Identifier(name) = leftexpr.as_ref() {
                // `a = 1` should create a variable;
                // `a.one = 1` without `a` should fail.
                state.declare_var(None, &name.0)?;
            }
            let assignee = leftexpr.interpret(state)?;
            match assignee {
                Interpreted::Member{of, name} => {
                    let ofref = of.to_ref(&state.heap)?;
                    state.heap.property_assign(ofref, &name, &value)?;
                    Ok(value)
                }
                Interpreted::Ref(_objref) => {
                    /*
                    let value = value.to_value(&state.heap)?;
                    *state.heap.get_mut(objref) = value.clone();
                    Ok(Interpreted::Value(value))
                    */
                    unreachable!()
                }
                _ => Err(Exception::TypeErrorCannotAssign(assignee))
            }
        }
    }
}

impl Interpretable for CallExpression {
    fn interpret(&self, state: &mut RuntimeState) -> Result<Interpreted, Exception> {
        let CallExpression(callee_expr, argument_exprs) = self;

        let mut arguments = vec![];
        for argexpr in argument_exprs.iter() {
            let arg = argexpr.interpret(state)?;
            arguments.push(arg);
        }

        let callee = callee_expr.interpret(state)?;

        let (this_ref, method_name) = match &callee {
            Interpreted::Member{ of, name } => {
                let this_ref = of.to_ref(&state.heap)?;
                (this_ref, name.clone())
            }
            Interpreted::Ref(_func_ref) =>
                todo!(),
            Interpreted::Value(_) =>
                return Err(Exception::TypeErrorNotCallable(callee.clone()))
        };

        let this_object = state.heap.get(this_ref).to_object()?;
        let prop = this_object.properties.get(&method_name)
            .ok_or(Exception::TypeErrorNotCallable(callee.clone()))?;
        let prop = match prop.content {
            Content::Data(function_ref) => {
                let function_object = state.heap.get(function_ref).to_object()?;
                // TODO: check if this is a Function
                function_object.properties.get(JSObject::VALUE)
                    .ok_or(Exception::TypeErrorNotCallable(callee.clone()))?
            }
            _ => prop,
        };
        match &prop.content {
            Content::NativeFunction(func) =>
                func(this_ref, method_name, arguments, &mut state.heap),
            Content::Closure(closure) => {
                let object::Closure{ params, body, .. } = closure.clone();
                state.push_scope(params, arguments, this_ref)?;
                let result = body.interpret(state);
                state.pop_scope()?;
                match result {
                    Ok(_) => // BlockStatement result
                        Ok(Interpreted::VOID),
                    Err(Exception::JumpReturn(returned)) =>
                        Ok(returned),
                    Err(e) =>
                        Err(e)
                }
            }
            _ => Err(Exception::TypeErrorNotCallable(callee.clone()))
        }
    }
}

impl Interpretable for FunctionExpression {
    fn interpret(&self, state: &mut RuntimeState) -> Result<Interpreted, Exception> {
        let mut function_object = JSObject::new();

        // __proto__ => Function.prototype
        let function_prototype_ref = state.heap.lookup_ref(&["Function", "prototype"])?;
        function_object.set_system(
            JSObject::PROTO,
            Content::Data(function_prototype_ref),
        );

        let length_ref = state.heap.allocate(JSValue::Number(self.params.len() as f64));
        function_object.set_nonconf(
            "length",
            Content::Data(length_ref),
        );

        let closure = object::Closure {
            id: self.id.clone(),
            params: self.params.clone(),
            body: self.body.clone(),
            // TODO: compute and capture free variables
        };
        function_object.set_system(
            JSObject::VALUE,
            Content::Closure(closure),
        );

        Ok(Interpreted::Value(JSValue::Object(function_object)))
    }
}
