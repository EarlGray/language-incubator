use crate::object;
use crate::object::{
    Access,
    Content,
    Interpreted,
    JSObject,
    JSValue,
};
use crate::heap::Heap;
use crate::error::Exception;
use crate::ast::*;      // yes, EVERYTHING


// ==============================================

pub trait Interpretable {
    fn interpret(&self, heap: &mut Heap) -> Result<Interpreted, Exception>;
}


// ==============================================

impl Interpretable for Program {
    fn interpret(&self, heap: &mut Heap) -> Result<Interpreted, Exception> {
        let mut result = Interpreted::VOID;
        for stmt in self.body.iter() {
            result = stmt.interpret(heap)?;
        }
        Ok(result)
    }
}

// ==============================================

impl Interpretable for Statement {
    fn interpret(&self, heap: &mut Heap) -> Result<Interpreted, Exception> {
        match self {
            Statement::Empty                        => Ok(Interpreted::VOID),
            Statement::Expr(stmt)                   => stmt.interpret(heap),
            Statement::Block(stmt)                  => stmt.interpret(heap),
            Statement::If(stmt)                     => stmt.interpret(heap),
            Statement::For(stmt)                    => stmt.interpret(heap),
            Statement::Break(stmt)                  => stmt.interpret(heap),
            Statement::Continue(stmt)               => stmt.interpret(heap),
            Statement::Return(stmt)                 => stmt.interpret(heap),
            Statement::VariableDeclaration(stmt)    => stmt.interpret(heap),
            Statement::FunctionDeclaration(stmt)    => stmt.interpret(heap),
        }
    }
}


// ==============================================

impl Interpretable for BlockStatement {
    fn interpret(&self, heap: &mut Heap) -> Result<Interpreted, Exception> {
        for stmt in self.body.iter() {
            stmt.interpret(heap)?;
        }
        Ok(Interpreted::VOID)
    }
}

impl Interpretable for IfStatement {
    fn interpret(&self, heap: &mut Heap) -> Result<Interpreted, Exception> {
        let jbool = self.test.interpret(heap)?;
        let cond = jbool.to_value(heap)?;
        if cond.boolify(heap) {
            self.consequent.interpret(heap)
        } else if let Some(else_stmt) = self.alternate.as_ref() {
            else_stmt.interpret(heap)
        } else {
            Ok(Interpreted::VOID)
        }
    }
}

impl Interpretable for ForStatement {
    fn interpret(&self, heap: &mut Heap) -> Result<Interpreted, Exception> {
        self.init.interpret(heap)?;
        loop {
            // test
            let testval = match self.test.as_ref() {
                None => true,
                Some(testexpr) => {
                    let result = testexpr.interpret(heap)?;
                    result.to_value(heap)?.boolify(heap)
                }
            };
            if !testval { break }

            // body
            let result = self.body.interpret(heap);
            match result {
                Ok(_) => (),
                Err(Exception::JumpContinue(None)) => continue,
                Err(Exception::JumpBreak(None)) => break,
                Err(e) => return Err(e),
            };

            // update
            if let Some(updateexpr) = self.update.as_ref() {
                updateexpr.interpret(heap)?;
            }
        }
        Ok(Interpreted::VOID)
    }
}

impl Interpretable for BreakStatement {
    fn interpret(&self, _heap: &mut Heap) -> Result<Interpreted, Exception> {
        let BreakStatement(maybe_label) = self;
        Err(Exception::JumpBreak(maybe_label.clone()))
    }
}

impl Interpretable for ContinueStatement {
    fn interpret(&self, _heap: &mut Heap) -> Result<Interpreted, Exception> {
        let ContinueStatement(maybe_label) = self;
        Err(Exception::JumpContinue(maybe_label.clone()))
    }
}

impl Interpretable for ExpressionStatement {
    fn interpret(&self, heap: &mut Heap) -> Result<Interpreted, Exception> {
        let result = self.expression.interpret(heap)?;
        let value = result.to_value(heap)?;
        Ok(Interpreted::Value(value))
    }
}

impl Interpretable for ReturnStatement {
    fn interpret(&self, heap: &mut Heap) -> Result<Interpreted, Exception> {
        let ReturnStatement(argument) = self;
        let returned = match argument {
            None => Interpreted::VOID,
            Some(argexpr) => argexpr.interpret(heap)?,
        };
        Err(Exception::JumpReturn(returned))
    }
}

impl Interpretable for VariableDeclaration {
    fn interpret(&self, heap: &mut Heap) -> Result<Interpreted, Exception> {
        for decl in self.declarations.iter() {
            let optinit = decl.init.as_ref()
                .map(|initexpr| initexpr.interpret(heap))
                .transpose()?;
            let name = &decl.name;
            heap.declare_var(Some(self.kind), name)?;
            if let Some(init) = optinit {
                let value = init.to_value(heap)?;
                heap.scope_mut()
                    .update(name, value)
                    .or_else(crate::error::ignore_set_readonly)?;
            }
        }
        Ok(Interpreted::VOID)
    }
}

impl Interpretable for FunctionDeclaration {
    fn interpret(&self, heap: &mut Heap) -> Result<Interpreted, Exception> {
        let function_ref = self.function.interpret(heap)?;

        let name = &self.id.0;
        heap.declare_var(Some(DeclarationKind::Var), name)?;

        let value = function_ref.to_value(heap)?;
        heap.scope_mut()
            .update(name, value)
            .or_else(crate::error::ignore_set_readonly)?;
        Ok(Interpreted::VOID)
    }
}

impl Interpretable for Expr {
    fn interpret(&self, heap: &mut Heap) -> Result<Interpreted, Exception> {
        match self {
            Expr::Literal(expr) =>              expr.interpret(heap),
            Expr::Identifier(expr) =>           expr.interpret(heap),
            Expr::BinaryOp(expr) =>             expr.interpret(heap),
            Expr::LogicalOp(expr) =>            expr.interpret(heap),
            Expr::Call(expr) =>                 expr.interpret(heap),
            Expr::Array(expr) =>                expr.interpret(heap),
            Expr::Member(expr) =>               expr.interpret(heap),
            Expr::Object(expr) =>               expr.interpret(heap),
            Expr::Assign(expr) =>               expr.interpret(heap),
            Expr::Conditional(expr) =>          expr.interpret(heap),
            Expr::Unary(expr) =>                expr.interpret(heap),
            Expr::Update(expr) =>               expr.interpret(heap),
            Expr::Function(expr) =>             expr.interpret(heap),
            Expr::New(expr) =>                  expr.interpret(heap),
            Expr::This =>                       heap.interpret_this(),
        }
    }
}


impl Interpretable for Literal {
    fn interpret(&self, heap: &mut Heap) -> Result<Interpreted, Exception> {
        let value = heap.object_from_json(&self.0);
        Ok(Interpreted::Value(value))
    }
}

impl Interpretable for Identifier {
    fn interpret(&self, heap: &mut Heap) -> Result<Interpreted, Exception> {
        let name = &self.0;
        heap.lookup_var(name)
            .ok_or(Exception::ReferenceNotFound(name.to_string()))
    }
}

impl Interpretable for ConditionalExpression {
    fn interpret(&self, heap: &mut Heap) -> Result<Interpreted, Exception> {
        let ConditionalExpression(condexpr, thenexpr, elseexpr) = self;
        let cond = condexpr.interpret(heap)?.to_value(heap)?;
        if cond.boolify(heap) {
            thenexpr.interpret(heap)
        } else {
            elseexpr.interpret(heap)
        }
    }
}

impl Interpretable for LogicalExpression {
    fn interpret(&self, heap: &mut Heap) -> Result<Interpreted, Exception> {
        let LogicalExpression(lexpr, op, rexpr) = self;
        let lval = lexpr.interpret(heap)?.to_value(heap)?;
        let value = match (lval.boolify(heap), op) {
            (true, BoolOp::And) |
            (false, BoolOp::Or) =>
                rexpr.interpret(heap)?.to_value(heap)?,
            _ => lval,
        };
        Ok(Interpreted::Value(value))
    }
}

impl Interpretable for BinaryExpression {
    fn interpret(&self, heap: &mut Heap) -> Result<Interpreted, Exception> {
        let BinaryExpression(lexpr, op, rexpr) = self;
        let lval = lexpr.interpret(heap)?.to_value(heap)?;
        let rval = rexpr.interpret(heap)?.to_value(heap)?;
        let result = match op {
            BinOp::EqEq => JSValue::from(JSValue::loose_eq(&lval, &rval, heap)),
            BinOp::NotEq => JSValue::from(!JSValue::loose_eq(&lval, &rval, heap)),
            BinOp::EqEqEq => JSValue::from(JSValue::strict_eq(&lval, &rval, heap)),
            BinOp::NotEqEq => JSValue::from(!JSValue::strict_eq(&lval, &rval, heap)),
            BinOp::Less => JSValue::less(&lval, &rval, heap),
            BinOp::Plus => JSValue::plus(&lval, &rval, heap)?,
            BinOp::Minus => JSValue::minus(&lval, &rval, heap)?,
            BinOp::Star => JSValue::numerically(&lval, &rval, heap, |a, b| a * b),
        };
        Ok(Interpreted::Value(result))
    }
}

impl Interpretable for UnaryExpression {
    fn interpret(&self, heap: &mut Heap) -> Result<Interpreted, Exception> {
        let UnaryExpression(op, argexpr) = self;
        let arg = argexpr.interpret(heap)?;
        let argvalue = arg.to_value(heap)?;
        let value = match op {
            UnOp::Delete => JSValue::from(arg.delete(heap).is_ok()),
            UnOp::Exclamation => JSValue::Bool(!argvalue.boolify(heap)),
            UnOp::Minus => JSValue::Number(- argvalue.numberify(heap).unwrap_or(f64::NAN)),
            UnOp::Plus => JSValue::Number(argvalue.numberify(heap).unwrap_or(f64::NAN)),
            UnOp::Typeof => JSValue::from(argvalue.type_of(heap)),
            UnOp::Tilde => {
                let num = argvalue.numberify(heap).unwrap_or(f64::NAN);
                let num = if f64::is_nan(num) { 0.0 } else { num };
                JSValue::from(-(1.0 + num))
            }
            UnOp::Void => JSValue::Undefined,
        };
        Ok(Interpreted::Value(value))
    }
}

impl Interpretable for UpdateExpression {
    fn interpret(&self, heap: &mut Heap) -> Result<Interpreted, Exception> {
        let UpdateExpression(op, prefix, argexpr) = self;
        let assignee = argexpr.interpret(heap)?;
        let oldvalue = assignee.to_value(heap)?;
        let oldnum = oldvalue.numberify(heap).unwrap_or(f64::NAN);

        let newnum = match op {
            UpdOp::Increment => oldnum + 1.0,
            UpdOp::Decrement => oldnum - 1.0,
        };

        match assignee {
            Interpreted::Member{of, name} => {
                heap.get_mut(of)
                    .update(&name, JSValue::from(newnum))
                    .or_else(crate::error::ignore_set_readonly)?;
            }
            _ => return Err(Exception::TypeErrorCannotAssign(assignee))
        }
        let resnum = if *prefix { newnum } else { oldnum };
        Ok(Interpreted::from(resnum))
    }
}

impl Interpretable for MemberExpression {
    fn interpret(&self, heap: &mut Heap) -> Result<Interpreted, Exception> {
        let MemberExpression(objexpr, propexpr, computed) = self;

        // compute the name of the property:
        let propname = if *computed {
            let propval = propexpr.interpret(heap)?.to_value(heap)?;
            propval.stringify(heap)?
        } else {
            match &**propexpr {
                Expr::Identifier(name) => name.0.clone(),
                _ => panic!("Member(computed=false) property is not an identifier")
            }
        };

        // get the object reference for member computation:
        let objresult = objexpr.interpret(heap)?;
        let objresult = match objresult {
            // autowrap primitive values into Boolean/String/Number
            Interpreted::Value(JSValue::Bool(b)) =>
                Interpreted::from(heap.alloc(JSObject::from_bool(b))),
            _ => objresult,
        };
        let objref = objresult.to_ref(heap).map_err(|_| {
            Exception::ReferenceNotAnObject(objresult.clone())
        })?;

        if &propname == "__proto__" {
            let proto = heap.get(objref).proto;
            return Ok(Interpreted::from(proto));
        }

        Ok(Interpreted::member(objref, &propname))
    }
}

impl Interpretable for ObjectExpression {
    fn interpret(&self, heap: &mut Heap) -> Result<Interpreted, Exception> {
        let mut object = JSObject::new();

        for (key, valexpr) in self.0.iter() {
            let keyname = match key {
                ObjectKey::Identifier(ident) =>
                    ident.clone(),
                ObjectKey::Computed(expr) => {
                    let result = expr.interpret(heap)?.to_value(heap)?;
                    result.stringify(heap)?
                }
            };
            let valresult = valexpr.interpret(heap)?;
            let value = valresult.to_value(heap)?;
            object.set_property(&keyname, Content::Value(value))?;
        }

        let object_ref = heap.alloc(object);
        Ok(Interpreted::from(object_ref))
    }
}

impl Interpretable for ArrayExpression {
    fn interpret(&self, heap: &mut Heap) -> Result<Interpreted, Exception> {
        let ArrayExpression(exprs) = self;
        let storage = exprs.iter().map(|expr| {
            let value = expr.interpret(heap)?;
            value.to_value(heap)
        }).collect::<Result<Vec<JSValue>, Exception>>()?;

        let object = JSObject::from_array(storage);
        let object_ref = heap.alloc(object);
        Ok(Interpreted::from(object_ref))
    }
}

impl Interpretable for AssignmentExpression {
    fn interpret(&self, heap: &mut Heap) -> Result<Interpreted, Exception> {
        let AssignmentExpression(leftexpr, AssignOp(modop), valexpr) = self;

        // this must happen before a possible declare_var()
        let value = valexpr.interpret(heap)?;

        if let Some(op) = modop {
            let assignee = leftexpr.interpret(heap)?;
            let oldvalue = assignee.to_value(heap)?;
            let value = value.to_value(heap)?;
            let newvalue = match op {
                BinOp::Plus => JSValue::plus(&oldvalue, &value, heap)?,
                BinOp::Minus => JSValue::minus(&oldvalue, &value, heap)?,
                BinOp::Star => JSValue::numerically(&oldvalue, &value, heap, |a, b| a * b),
                _ => panic!(format!("Binary operation {:?} cannot be used in assignment", op))
            };
            match assignee {
                Interpreted::Member{of, name} => {
                    heap.get_mut(of).update(&name, newvalue.clone())?;
                    Ok(Interpreted::Value(newvalue))
                }
                _ => Err(Exception::TypeErrorCannotAssign(assignee.clone()))
            }
        } else {
            if let Expr::Identifier(name) = leftexpr.as_ref() {
                // `a = 1` should create a variable;
                // `a.one = 1` without `a` should fail.
                heap.declare_var(None, &name.0)?;
            }
            let assignee = leftexpr.interpret(heap)?;
            match assignee {
                Interpreted::Member{of, name} => {
                    let value = value.to_value(heap)?;
                    heap.get_mut(of)
                        .update(&name, value.clone())
                        .or_else(crate::error::ignore_set_readonly)?;
                    Ok(Interpreted::Value(value))
                }
                _ => Err(Exception::TypeErrorCannotAssign(assignee))
            }
        }
    }
}

impl Interpretable for CallExpression {
    fn interpret(&self, heap: &mut Heap) -> Result<Interpreted, Exception> {
        let CallExpression(callee_expr, argument_exprs) = self;

        let mut arguments = vec![];
        for argexpr in argument_exprs.iter() {
            let arg = argexpr.interpret(heap)?;
            arguments.push(arg);
        }

        let callee = callee_expr.interpret(heap)?;
        match &callee {
            Interpreted::Member{ of, name } =>
                heap.execute_method(*of, &name, arguments),
            Interpreted::Value(JSValue::Ref(funcref)) => {
                let this_ref = Heap::GLOBAL;  // TODO: figure out what is this
                heap.execute(*funcref, this_ref, "<anonymous>", arguments)
            }
            _ => return Err(Exception::TypeErrorNotCallable(callee.clone()))
        }
    }
}

impl Interpretable for NewExpression {
    fn interpret(&self, heap: &mut Heap) -> Result<Interpreted, Exception> {
        let NewExpression(callee_expr, argument_exprs) = self;

        let arguments = argument_exprs.iter()
            .map(|expr| expr.interpret(heap))
            .collect::<Result<Vec<Interpreted>, Exception>>()?;

        let callee = callee_expr.interpret(heap)?;
        let funcref = callee.to_ref(heap)?;
        let prototype_ref = heap.get_mut(funcref).get_value("prototype")
            .ok_or_else(|| Exception::TypeErrorGetProperty(callee, "prototype".to_string()))?
            .to_ref()?;

        // allocate the object
        let mut object = JSObject::new();
        object.proto = prototype_ref;

        let object_ref = heap.alloc(object);

        // call its constructor
        let result = heap.execute(funcref, object_ref, "<constructor>", arguments)?;
        match result {
            Interpreted::Value(JSValue::Ref(r)) if r != Heap::NULL => Ok(result),
            _ => Ok(Interpreted::from(object_ref)),
        }
    }
}

impl Interpretable for FunctionExpression {
    fn interpret(&self, heap: &mut Heap) -> Result<Interpreted, Exception> {
        let closure = object::Closure {
            id: self.id.clone(),
            params: self.params.clone(),
            body: self.body.clone(),
            captured_scope: heap.local_scope().unwrap_or(Heap::NULL)
        };

        let function_object = JSObject::from_closure(closure);
        let function_ref = heap.alloc(function_object);

        let prototype_ref = heap.alloc(JSObject::new());
        heap.get_mut(function_ref).set("prototype", Content::from(prototype_ref), Access::WRITE)?;
        heap.get_mut(prototype_ref).set_hidden("constructor", Content::from(function_ref))?;

        Ok(Interpreted::from(function_ref))
    }
}
