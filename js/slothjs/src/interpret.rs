use crate::object;
use crate::object::{
    Content,
    Interpreted,
    JSArray,
    JSObject,
    JSValue,
};
use crate::heap::Heap;
use crate::error::Exception;
use crate::ast::*;      // yes, EVERYTHING


// ==============================================

pub struct RuntimeState {
    pub heap: Heap,
}

impl RuntimeState {
    pub fn new() -> Self {
        RuntimeState{ heap: Heap::new() }
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
        if cond.boolify(&state.heap) {
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
                    result.to_value(&state.heap)?.boolify(&state.heap)
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
            state.heap.declare_var(Some(self.kind), name)?;
            if let Some(init) = optinit {
                let value = init.to_value(&state.heap)?;
                state.heap.scope_mut().update(name, value)?;
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
            Expr::Array(expr) =>                expr.interpret(state),
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
        state.heap.lookup_var(name)
            .ok_or(Exception::ReferenceNotFound(name.to_string()))
    }
}

impl Interpretable for ConditionalExpression {
    fn interpret(&self, state: &mut RuntimeState) -> Result<Interpreted, Exception> {
        let ConditionalExpression(condexpr, thenexpr, elseexpr) = self;
        let cond = condexpr.interpret(state)?.to_value(&state.heap)?;
        if cond.boolify(&state.heap) {
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
            BinOp::EqEq => JSValue::from(JSValue::loose_eq(&lval, &rval, &state.heap)),
            BinOp::NotEq => JSValue::from(!JSValue::loose_eq(&lval, &rval, &state.heap)),
            BinOp::Less => JSValue::less(&lval, &rval, &state.heap),
            BinOp::Plus => JSValue::plus(&lval, &rval, &state.heap),
            BinOp::Minus => JSValue::minus(&lval, &rval, &state.heap),
            BinOp::Star => JSValue::numerically(&lval, &rval, &state.heap, |a, b| a * b),
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
            UnOp::Exclamation => JSValue::Bool(!argvalue.boolify(&state.heap)),
            UnOp::Minus => JSValue::Number(- argvalue.numberify(&state.heap).unwrap_or(f64::NAN)),
            UnOp::Plus => JSValue::Number(argvalue.numberify(&state.heap).unwrap_or(f64::NAN)),
            UnOp::Typeof => JSValue::from(argvalue.type_of(&state.heap)),
            UnOp::Tilde => {
                let num = argvalue.numberify(&state.heap).unwrap_or(f64::NAN);
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

        if propname == "__proto__" {
            let proto = state.heap.get(objref).proto;
            return Ok(Interpreted::from(proto));
        }

        Ok(Interpreted::member(objref, &propname))
    }
}

impl Interpretable for ObjectExpression {
    fn interpret(&self, state: &mut RuntimeState) -> Result<Interpreted, Exception> {
        let mut object = JSObject::new();

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
            let value = valresult.to_value(&state.heap)?;
            object.set_property(&keyname, Content::Value(value));
        }

        let object_ref = state.heap.alloc(object);
        Ok(Interpreted::from(object_ref))
    }
}

impl Interpretable for ArrayExpression {
    fn interpret(&self, state: &mut RuntimeState) -> Result<Interpreted, Exception> {
        let ArrayExpression(exprs) = self;

        let mut object = JSObject::new();
        object.proto = Heap::ARRAY_PROTO;

        let storage = exprs.iter().map(|expr| {
            let value = expr.interpret(state)?;
            value.to_value(&mut state.heap)
        }).collect::<Result<Vec<JSValue>, Exception>>()?;
        object.value = object::ObjectValue::Array(JSArray{ storage });

        let object_ref = state.heap.alloc(object);
        Ok(Interpreted::from(object_ref))
    }
}

impl Interpretable for AssignmentExpression {
    fn interpret(&self, state: &mut RuntimeState) -> Result<Interpreted, Exception> {
        let AssignmentExpression(leftexpr, AssignOp(modop), valexpr) = self;

        // this must happen before a possible declare_var()
        let value = valexpr.interpret(state)?;

        if let Some(op) = modop {
            let assignee = leftexpr.interpret(state)?;
            let oldvalue = assignee.to_value(&state.heap)?;
            let value = value.to_value(&state.heap)?;
            let newvalue = match op {
                BinOp::Plus => JSValue::plus(&oldvalue, &value, &state.heap),
                BinOp::Minus => JSValue::minus(&oldvalue, &value, &state.heap),
                BinOp::Star => JSValue::numerically(&oldvalue, &value, &state.heap, |a, b| a * b),
                _ => panic!(format!("Binary operation {:?} cannot be used in assignment", op))
            };
            match assignee {
                Interpreted::Member{of, name} => {
                    state.heap.get_mut(of).update(&name, newvalue.clone())?;
                    Ok(Interpreted::Value(newvalue))
                }
                _ => Err(Exception::TypeErrorCannotAssign(assignee.clone()))
            }
        } else {
            if let Expr::Identifier(name) = leftexpr.as_ref() {
                // `a = 1` should create a variable;
                // `a.one = 1` without `a` should fail.
                state.heap.declare_var(None, &name.0)?;
            }
            let assignee = leftexpr.interpret(state)?;
            match assignee {
                Interpreted::Member{of, name} => {
                    let value = value.to_value(&state.heap)?;
                    state.heap.get_mut(of).update(&name, value.clone())?;
                    Ok(Interpreted::Value(value))
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
                (*of, name.clone())
            }
            _ => return Err(Exception::TypeErrorNotCallable(callee.clone()))
        };

        let (of, name) = match state.heap.lookup_protochain(this_ref, &method_name) {
            Some(Interpreted::Member{ of, name }) => (of, name),
            Some(_) => unreachable!(),
            None => return Err(Exception::TypeErrorNotCallable(callee.clone()))
        };
        let funcobj_ref = match state.heap.get(of).properties.get(&name) {
            Some(object::Property{ content: Content::Value(JSValue::Ref(func_ref)), ..}) => *func_ref,
            _ => return Err(Exception::TypeErrorNotCallable(Interpreted::member(of, &name)))
        };
        match state.heap.get(funcobj_ref).value.clone() {
            object::ObjectValue::VMCall(vmcall) => {
                vmcall.call(this_ref, method_name, arguments, &mut state.heap)
            }
            object::ObjectValue::Closure(closure) => {
                let object::Closure{params, body, ..} = &*closure;
                state.heap.push_scope(params.clone(), arguments, this_ref)?;
                let result = body.interpret(state);
                state.heap.pop_scope()?;
                match result {
                    Ok(_) => // BlockStatement result
                        Ok(Interpreted::VOID),
                    Err(Exception::JumpReturn(returned)) =>
                        Ok(returned),
                    Err(e) =>
                        Err(e)
                }
            }
            _ => return Err(Exception::TypeErrorNotCallable(Interpreted::Member{of, name}))
        }
    }
}

impl Interpretable for FunctionExpression {
    fn interpret(&self, state: &mut RuntimeState) -> Result<Interpreted, Exception> {
        let closure = object::Closure {
            id: self.id.clone(),
            params: self.params.clone(),
            body: self.body.clone(),
            // TODO: compute and capture free variables
        };

        let function_object = JSObject::from_closure(closure);
        let function_ref = state.heap.alloc(function_object);
        Ok(Interpreted::from(function_ref))
    }
}
