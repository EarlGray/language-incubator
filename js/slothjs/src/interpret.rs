use crate::ast::*; // yes, EVERYTHING

use crate::error::Exception;
use crate::heap::Heap;
use crate::object;
use crate::object::{
    Access,
    Content,
    Interpreted,
    JSObject,
    JSValue,
};

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
            Statement::Empty => Ok(Interpreted::VOID),
            Statement::Expr(stmt) => stmt.interpret(heap),
            Statement::Block(stmt) => stmt.interpret(heap),
            Statement::If(stmt) => stmt.interpret(heap),
            Statement::Switch(stmt) => stmt.interpret(heap),
            Statement::For(stmt) => stmt.interpret(heap),
            Statement::ForIn(stmt) => stmt.interpret(heap),
            Statement::Break(stmt) => stmt.interpret(heap),
            Statement::Continue(stmt) => stmt.interpret(heap),
            Statement::Label(stmt) => stmt.interpret(heap),
            Statement::Return(stmt) => stmt.interpret(heap),
            Statement::Throw(stmt) => stmt.interpret(heap),
            Statement::Try(stmt) => stmt.interpret(heap),
            Statement::VariableDeclaration(stmt) => stmt.interpret(heap),
            Statement::FunctionDeclaration(stmt) => stmt.interpret(heap),
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

impl Interpretable for SwitchStatement {
    fn interpret(&self, heap: &mut Heap) -> Result<Interpreted, Exception> {
        let matchee = self.discriminant.interpret(heap)?;
        let switchval = matchee.to_value(heap)?;

        let mut default: Option<usize> = None; // the index of the default case, if any
        let mut found_case: Option<usize> = None;

        // search
        for (i, case) in self.cases.iter().enumerate() {
            let caseval = match &case.test {
                None => {
                    default = Some(i);
                    continue;
                }
                Some(test) => test.interpret(heap)?.to_value(heap)?,
            };

            if JSValue::strict_eq(&switchval, &caseval, heap) {
                found_case = Some(i);
                break;
            }
        }

        let end = self.cases.len();
        let restart_index = found_case.or(default).unwrap_or(end);

        // execute
        for i in restart_index..end {
            for stmt in self.cases[i].consequent.iter() {
                match stmt.interpret(heap) {
                    Ok(_) => (),
                    Err(Exception::JumpBreak(None)) => {
                        return Ok(Interpreted::VOID);
                    }
                    Err(e) => return Err(e),
                }
            }
        }
        Ok(Interpreted::VOID)
    }
}

impl ForStatement {
    /// `do_loop()` executes the loop except its `init` statement.
    /// `init` must be interpreted before this, if needed.
    fn do_loop(&self, heap: &mut Heap) -> Result<(), Exception> {
        while self.should_iterate(heap)? {
            // body
            let result = self.body.interpret(heap);
            match result {
                Ok(_) => (),
                Err(Exception::JumpContinue(None)) => (),
                Err(Exception::JumpBreak(None)) => break,
                Err(e) => return Err(e),
            };

            self.do_update(heap)?;
        }
        Ok(())
    }

    fn should_iterate(&self, heap: &mut Heap) -> Result<bool, Exception> {
        match self.test.as_ref() {
            None => Ok(true),
            Some(testexpr) => {
                let result = testexpr.interpret(heap)?;
                Ok(result.to_value(heap)?.boolify(heap))
            }
        }
    }

    fn do_update(&self, heap: &mut Heap) -> Result<(), Exception> {
        if let Some(updateexpr) = self.update.as_ref() {
            updateexpr.interpret(heap)?;
        }
        Ok(())
    }
}

impl Interpretable for ForStatement {
    fn interpret(&self, heap: &mut Heap) -> Result<Interpreted, Exception> {
        self.init.interpret(heap)?;
        self.do_loop(heap)?;
        Ok(Interpreted::VOID)
    }
}

impl ForInStatement {}

impl Interpretable for ForInStatement {
    fn interpret(&self, heap: &mut Heap) -> Result<Interpreted, Exception> {
        use std::collections::HashSet;

        let iteratee = self.right.interpret(heap)?;
        let iteratee = iteratee.to_value(heap)?.objectify(heap);

        let assignexpr = match &self.left {
            ForInTarget::Expr(expr) => expr.clone(),
            ForInTarget::Var(vardecl) => {
                if vardecl.declarations.len() != 1 {
                    return Err(Exception::SyntaxErrorForInMultipleVar());
                }
                let name = &vardecl.declarations[0].name.0;
                heap.declare_var(Some(vardecl.kind), &name)?;
                Expr::Identifier(Identifier::from(name.as_str()))
            }
        };

        let mut visited = HashSet::new();
        let mut objref = iteratee;
        while objref != Heap::NULL {
            let object = heap.get(objref);
            let mut keys = (object.properties.keys())
                .map(|s| s.clone())
                .collect::<HashSet<String>>();
            if let Some(array) = object.as_array() {
                let indices = 0..array.storage.len();
                keys.extend(indices.map(|i| i.to_string()));
            }
            // TODO: strings iteration

            for propname in keys.drain() {
                if visited.contains(&propname) {
                    continue;
                }
                visited.insert(propname.clone());

                let object = heap.get(objref);
                match object.properties.get(&propname) {
                    Some(p) if p.access.enumerable() => (),
                    None if object.as_array().is_some() && propname.parse::<usize>().is_ok() => (),
                    Some(_) => continue, // not enumerable, skip
                    None => continue,    // the property has disappeared!
                };

                let assignee = assignexpr.interpret(heap)?;
                assignee
                    .put_value(JSValue::from(propname.as_str()), heap)
                    .or_else(crate::error::ignore_set_readonly)?;

                match self.body.interpret(heap) {
                    Ok(_) => (),
                    Err(Exception::JumpContinue(None)) => continue,
                    Err(Exception::JumpBreak(None)) => {
                        return Ok(Interpreted::VOID);
                    }
                    Err(e) => {
                        return Err(e);
                    }
                }
            }

            objref = heap.get(objref).proto;
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

impl LabelStatement {
    fn continue_loop(&self, heap: &mut Heap) -> Result<Interpreted, Exception> {
        let LabelStatement(label, body) = self;
        loop {
            // must be a loop to continue
            let loop_stmt = match &**body {
                Statement::For(stmt) => stmt,
                Statement::ForIn(_) => todo!(),
                _ => return Err(Exception::SyntaxErrorContinueLabelNotALoop(label.clone())),
            };

            loop_stmt.do_update(heap)?;
            let result = loop_stmt.do_loop(heap);
            match result {
                Err(Exception::JumpContinue(Some(target))) if &target == label => continue,
                Err(Exception::JumpBreak(Some(target))) if &target == label => break,
                Err(e) => return Err(e),
                Ok(()) => break,
            }
        }
        Ok(Interpreted::VOID)
    }
}

impl Interpretable for LabelStatement {
    fn interpret(&self, heap: &mut Heap) -> Result<Interpreted, Exception> {
        let LabelStatement(label, body) = self;

        let result = body.interpret(heap);
        match result {
            Err(Exception::JumpBreak(Some(target))) if &target == label => Ok(Interpreted::VOID),
            Err(Exception::JumpContinue(Some(target))) if &target == label => {
                self.continue_loop(heap)
            }
            _ => result,
        }
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

impl Interpretable for ThrowStatement {
    fn interpret(&self, heap: &mut Heap) -> Result<Interpreted, Exception> {
        let ThrowStatement(exc_expr) = self;
        let exc_value = exc_expr.interpret(heap)?;
        let exc_value = exc_value.to_value(heap)?;
        Err(Exception::UserThrown(exc_value))
    }
}

impl TryStatement {
    fn run_finalizer(&self, heap: &mut Heap) -> Result<(), Exception> {
        if let Some(finalizer) = self.finalizer.as_ref() {
            finalizer.interpret(heap)?;
        }
        Ok(())
    }
}

impl Interpretable for TryStatement {
    fn interpret(&self, heap: &mut Heap) -> Result<Interpreted, Exception> {
        let result = self.block.interpret(heap);
        match &result {
            Ok(_)
            | Err(Exception::JumpReturn(_))
            | Err(Exception::JumpBreak(_))
            | Err(Exception::JumpContinue(_)) => {
                self.run_finalizer(heap)?;
                result
            }
            Err(_err) => {
                let result = match &self.handler {
                    None => result,
                    Some(catch) => {
                        // TODO: let $(catch.param) = $(err)
                        // TODO: error mapping into Javascript
                        catch.body.interpret(heap)
                    }
                };
                self.run_finalizer(heap)?;
                result
            }
        }
    }
}

impl Interpretable for VariableDeclaration {
    fn interpret(&self, heap: &mut Heap) -> Result<Interpreted, Exception> {
        for decl in self.declarations.iter() {
            let optinit = (decl.init.as_ref())
                .map(|initexpr| initexpr.interpret(heap))
                .transpose()?;
            let name = &decl.name.0;
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
            Expr::Literal(expr) => expr.interpret(heap),
            Expr::Identifier(expr) => expr.interpret(heap),
            Expr::BinaryOp(expr) => expr.interpret(heap),
            Expr::LogicalOp(expr) => expr.interpret(heap),
            Expr::Call(expr) => expr.interpret(heap),
            Expr::Array(expr) => expr.interpret(heap),
            Expr::Member(expr) => expr.interpret(heap),
            Expr::Object(expr) => expr.interpret(heap),
            Expr::Assign(expr) => expr.interpret(heap),
            Expr::Conditional(expr) => expr.interpret(heap),
            Expr::Unary(expr) => expr.interpret(heap),
            Expr::Update(expr) => expr.interpret(heap),
            Expr::Sequence(expr) => expr.interpret(heap),
            Expr::Function(expr) => expr.interpret(heap),
            Expr::New(expr) => expr.interpret(heap),
            Expr::This => heap.interpret_this(),
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
            .ok_or(Exception::ReferenceNotFound(self.clone()))
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
            (true, BoolOp::And) | (false, BoolOp::Or) => rexpr.interpret(heap)?.to_value(heap)?,
            _ => lval,
        };
        Ok(Interpreted::Value(value))
    }
}

impl BinOp {
    fn compute(
        &self,
        lval: &JSValue,
        rval: &JSValue,
        heap: &mut Heap,
    ) -> Result<JSValue, Exception> {
        Ok(match self {
            BinOp::EqEq => JSValue::from(JSValue::loose_eq(lval, rval, heap)),
            BinOp::NotEq => JSValue::from(!JSValue::loose_eq(lval, rval, heap)),
            BinOp::EqEqEq => JSValue::from(JSValue::strict_eq(lval, rval, heap)),
            BinOp::NotEqEq => JSValue::from(!JSValue::strict_eq(lval, rval, heap)),
            BinOp::Less => JSValue::compare(lval, &rval, heap, |a, b| a < b, |a, b| a < b),
            BinOp::Greater => JSValue::compare(lval, rval, heap, |a, b| a > b, |a, b| a > b),
            BinOp::LtEq => JSValue::compare(lval, rval, heap, |a, b| a <= b, |a, b| a <= b),
            BinOp::GtEq => JSValue::compare(lval, rval, heap, |a, b| a >= b, |a, b| a >= b),
            BinOp::Plus => JSValue::plus(lval, rval, heap)?,
            BinOp::Minus => JSValue::minus(lval, rval, heap)?,
            BinOp::Star => JSValue::numerically(lval, rval, heap, |a, b| a * b),
            BinOp::Slash => JSValue::numerically(lval, rval, heap, |a, b| a / b),
            BinOp::Percent => JSValue::numerically(lval, rval, heap, |a, b| a % b),
            BinOp::Pipe => {
                let bitor = |a, b| (a as i32 | b as i32) as f64;
                JSValue::numerically(lval, rval, heap, bitor)
            }
            BinOp::Hat => {
                let bitxor = |a, b| (a as i32 ^ b as i32) as f64;
                JSValue::numerically(lval, rval, heap, bitxor)
            }
            BinOp::Ampersand => {
                let bitand = |a, b| (a as i32 & b as i32) as f64;
                JSValue::numerically(lval, rval, heap, bitand)
            }
            BinOp::LtLt => {
                let bitshl = |a, b| ((a as i32) << ((b as u32) & 0x1f) as i32) as f64;
                JSValue::numerically(lval, rval, heap, bitshl)
            }
            BinOp::GtGt => {
                let bitshr = |a, b| ((a as i32) >> ((b as u32) & 0x1f) as i32) as f64;
                JSValue::numerically(lval, rval, heap, bitshr)
            }
            BinOp::GtGtGt => {
                let bitshru = |a, b| ((a as u32) >> (b as u32) & 0x1f) as f64;
                JSValue::numerically(lval, rval, heap, bitshru)
            }
            BinOp::In => {
                let prop = lval.stringify(heap)?;
                let objref = rval.to_ref()?;
                let object = heap.get(objref);
                let found = object.lookup_value(&prop, heap).is_some();
                JSValue::from(found)
            }
            BinOp::InstanceOf => {
                let constructor = rval.to_ref()?;
                let found = match lval.to_ref() {
                    Err(_) => false,
                    Ok(objref) => objref.isinstance(constructor, heap)?,
                };
                JSValue::from(found)
            }
        })
    }
}

impl Interpretable for BinaryExpression {
    fn interpret(&self, heap: &mut Heap) -> Result<Interpreted, Exception> {
        let BinaryExpression(lexpr, op, rexpr) = self;
        let lval = lexpr.interpret(heap)?.to_value(heap)?;
        let rval = rexpr.interpret(heap)?.to_value(heap)?;
        let result = op.compute(&lval, &rval, heap)?;
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
            UnOp::Minus => JSValue::Number(-argvalue.numberify(heap).unwrap_or(f64::NAN)),
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

        assignee
            .put_value(JSValue::from(newnum), heap)
            .or_else(crate::error::ignore_set_readonly)?;

        let resnum = if *prefix { newnum } else { oldnum };
        Ok(Interpreted::from(resnum))
    }
}

impl Interpretable for SequenceExpression {
    fn interpret(&self, heap: &mut Heap) -> Result<Interpreted, Exception> {
        let SequenceExpression(exprs) = self;

        let mut value = JSValue::Undefined;
        for expr in exprs.iter() {
            value = expr.interpret(heap)?.to_value(heap)?;
        }
        Ok(Interpreted::from(value))
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
                _ => panic!("Member(computed=false) property is not an identifier"),
            }
        };

        // get the object reference for member computation:
        let objresult = objexpr.interpret(heap)?;
        let objref = match objresult.to_value(heap)? {
            JSValue::Undefined => return Err(Exception::ReferenceNotAnObject(objresult.clone())),
            value => value.objectify(heap),
        };

        // TODO: __proto__ as (getPrototypeOf, setPrototypeOf) property
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
                ObjectKey::Identifier(ident) => ident.clone(),
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
        let storage = exprs
            .iter()
            .map(|expr| expr.interpret(heap)?.to_value(heap))
            .collect::<Result<Vec<JSValue>, Exception>>()?;

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
            let newvalue = op.compute(&oldvalue, &value, heap)?;
            assignee
                .put_value(newvalue.clone(), heap)
                .or_else(crate::error::ignore_set_readonly)?;
            Ok(Interpreted::Value(newvalue))
        } else {
            if let Expr::Identifier(name) = leftexpr.as_ref() {
                // `a = 1` should create a variable;
                // `a.one = 1` without `a` should fail.
                heap.declare_var(None, &name.0)?;
            }
            let value = value.to_value(heap)?;
            let assignee = leftexpr.interpret(heap)?;
            assignee
                .put_value(value.clone(), heap)
                .or_else(crate::error::ignore_set_readonly)?;
            Ok(Interpreted::Value(value))
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
            Interpreted::Member { of, name } => heap.execute_method(*of, &name, arguments),
            Interpreted::Value(JSValue::Ref(funcref)) => {
                let this_ref = Heap::GLOBAL; // TODO: figure out what is this
                heap.execute(*funcref, this_ref, "<anonymous>", arguments)
            }
            _ => return Err(Exception::TypeErrorNotCallable(callee.clone())),
        }
    }
}

impl Interpretable for NewExpression {
    fn interpret(&self, heap: &mut Heap) -> Result<Interpreted, Exception> {
        let NewExpression(callee_expr, argument_exprs) = self;

        let arguments = (argument_exprs.iter())
            .map(|expr| expr.interpret(heap))
            .collect::<Result<Vec<Interpreted>, Exception>>()?;

        let callee = callee_expr.interpret(heap)?;
        let funcref = callee.to_ref(heap)?;
        let prototype_ref = (heap.get_mut(funcref))
            .get_value("prototype")
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
            captured_scope: heap.local_scope().unwrap_or(Heap::NULL),
        };

        let function_object = JSObject::from_closure(closure);
        let function_ref = heap.alloc(function_object);

        let prototype_ref = heap.alloc(JSObject::new());
        heap.get_mut(function_ref)
            .set("prototype", Content::from(prototype_ref), Access::WRITE)?;
        heap.get_mut(prototype_ref)
            .set_hidden("constructor", Content::from(function_ref))?;

        Ok(Interpreted::from(function_ref))
    }
}
