use crate::prelude::*;

use crate::ast::*; // yes, EVERYTHING
use crate::builtin;
use crate::error::Exception;
use crate::function::{
    CallContext,
    Closure,
};
use crate::heap::Heap;
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
        heap.declare(self.variables.iter(), self.functions.iter())?;

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
        heap.loc = self.loc.clone();
        match &self.stmt {
            Stmt::Empty => Ok(Interpreted::VOID),
            Stmt::Expr(stmt) => stmt.interpret(heap),
            Stmt::Block(stmt) => stmt.interpret(heap),
            Stmt::If(stmt) => stmt.interpret(heap),
            Stmt::Switch(stmt) => stmt.interpret(heap),
            Stmt::For(stmt) => stmt.interpret(heap),
            Stmt::ForIn(stmt) => stmt.interpret(heap),
            Stmt::Break(stmt) => stmt.interpret(heap),
            Stmt::Continue(stmt) => stmt.interpret(heap),
            Stmt::Label(stmt) => stmt.interpret(heap),
            Stmt::Return(stmt) => stmt.interpret(heap),
            Stmt::Throw(stmt) => stmt.interpret(heap),
            Stmt::Try(stmt) => stmt.interpret(heap),
            Stmt::Variable(stmt) => stmt.interpret(heap),
            Stmt::Function(stmt) => stmt.interpret(heap),
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
        let iteratee = self.right.interpret(heap)?;
        let iteratee = iteratee.to_value(heap)?.objectify(heap);

        let assignexpr = match &self.left {
            ForInTarget::Expr(expr) => expr.clone(),
            ForInTarget::Var(vardecl) => {
                if vardecl.declarations.len() != 1 {
                    return Err(Exception::SyntaxErrorForInMultipleVar());
                }
                let ident = &vardecl.declarations[0].name;
                let idexpr = Expr::Identifier(Identifier::from(ident.as_str()));
                Expression {
                    expr: idexpr,
                    loc: None,
                }
            }
        };

        let mut visited = HashSet::new();
        let mut objref = iteratee;
        while objref != Heap::NULL {
            let object = heap.get(objref);
            let mut keys = object
                .properties
                .keys()
                .cloned()
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

                assignexpr
                    .interpret(heap)?
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
            let loop_stmt = match &body.stmt {
                Stmt::For(stmt) => stmt,
                Stmt::ForIn(_) => todo!(),
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

        heap.throw(Exception::UserThrown(exc_value))
    }
}

impl CatchClause {
    fn interpret(&self, exc: &Exception, heap: &mut Heap) -> Result<Interpreted, Exception> {
        let this_ref = heap.interpret_this()?.to_ref(heap)?;
        let scope_ref = heap.local_scope().unwrap_or(Heap::GLOBAL);

        heap.enter_new_scope(this_ref, scope_ref, |heap| {
            let error_value: JSValue = match exc {
                Exception::UserThrown(errval) => errval.clone(),
                Exception::JumpBreak(_)
                | Exception::JumpContinue(_)
                | Exception::JumpReturn(_) => {
                    panic!("Impossible to catch: {:?}", exc)
                }
                //Exception::ReferenceNotFound(ident) => { // TODO: ReferenceError
                _ => {
                    let message = format!("{:?}", exc);
                    let errval = builtin::error::error_constructor(
                        CallContext {
                            this_ref,
                            method_name: "ReferenceError".to_string(),
                            arguments: vec![Interpreted::from(message)],
                            loc: None, // TODO: get the location of the reference
                        },
                        heap,
                    )?;
                    errval.to_value(heap)?
                }
            };

            heap.scope_mut()
                .set_nonconf(self.param.as_str(), Content::Value(error_value))?;
            self.body.interpret(heap)
        })
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
            Err(exc) => {
                let result = match &self.handler {
                    None => result,
                    Some(catch) => catch.interpret(exc, heap),
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
            // compute and assign the initializer value:
            let optinit = (decl.init.as_ref())
                .map(|initexpr| initexpr.interpret(heap))
                .transpose()?;
            let name = &decl.name.0;
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
    fn interpret(&self, _heap: &mut Heap) -> Result<Interpreted, Exception> {
        // no-op: the work in done in Closure::call()
        Ok(Interpreted::VOID)
    }
}

impl Interpretable for Expression {
    fn interpret(&self, heap: &mut Heap) -> Result<Interpreted, Exception> {
        heap.loc = self.loc.clone();
        match &self.expr {
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
        if let Some(place) = heap.lookup_var(name) {
            Ok(place)
        } else {
            Ok(Interpreted::member(Heap::GLOBAL, name))
        }
    }
}

impl Interpretable for ConditionalExpression {
    fn interpret(&self, heap: &mut Heap) -> Result<Interpreted, Exception> {
        let cond = self.condexpr.interpret(heap)?.to_value(heap)?;
        if cond.boolify(heap) {
            self.thenexpr.interpret(heap)
        } else {
            self.elseexpr.interpret(heap)
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
            BinOp::Less => JSValue::compare(lval, rval, heap, |a, b| a < b, |a, b| a < b),
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
        let argvalue = || arg.to_value(heap);
        let argnum = || argvalue().map(|val| val.numberify(heap).unwrap_or(f64::NAN));
        let value = match op {
            UnOp::Exclamation => JSValue::Bool(!argvalue()?.boolify(heap)),
            UnOp::Minus => JSValue::Number(-argnum()?),
            UnOp::Plus => JSValue::Number(argnum()?),
            UnOp::Tilde => {
                let num = argnum()?;
                let num = if f64::is_nan(num) { 0.0 } else { num };
                JSValue::from(-(1.0 + num))
            }
            UnOp::Void => JSValue::Undefined,
            UnOp::Typeof => JSValue::from(
                argvalue()
                    .map(|val| val.type_of(heap))
                    .unwrap_or("undefined"),
            ),
            UnOp::Delete => JSValue::from(arg.delete(heap).is_ok()),
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
            match &propexpr.expr {
                Expr::Identifier(name) => name.0.clone(),
                _ => panic!("Member(computed=false) property is not an identifier"),
            }
        };

        // get the object reference for member computation:
        let objresult = objexpr.interpret(heap)?;
        let objref = match objresult.to_value(heap)? {
            JSValue::Undefined => return Err(Exception::ReferenceNotAnObject(objresult)),
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
        let storage = (exprs.iter())
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

        let value = valexpr.interpret(heap)?;

        // This can be:
        // - Interpreted::Member{ existing object, attribute }
        // - Interpreted::Member{ scope, existing variable }
        // - Interpreted::Member{ global, non-existing variable }
        // - Interpreted::Value
        let assignee = leftexpr.interpret(heap)?;

        let newvalue = match modop {
            None => value.to_value(heap)?,
            Some(op) => {
                let oldvalue = assignee.to_value(heap)?;
                let value = value.to_value(heap)?;
                op.compute(&oldvalue, &value, heap)?
            }
        };
        assignee
            .put_value(newvalue.clone(), heap)
            .or_else(crate::error::ignore_set_readonly)?;
        Ok(Interpreted::Value(newvalue))
    }
}

impl Interpretable for CallExpression {
    fn interpret(&self, heap: &mut Heap) -> Result<Interpreted, Exception> {
        let CallExpression(callee_expr, argument_exprs) = self;
        let loc = heap.loc.clone();

        let arguments = (argument_exprs.iter())
            .map(|argexpr| argexpr.interpret(heap))
            .collect::<Result<Vec<Interpreted>, Exception>>()?;

        let callee = callee_expr.interpret(heap)?;
        let (func_ref, this_ref, name) = callee.resolve_call(heap)?;

        let method_name = name.to_string();
        let call = CallContext {
            this_ref,
            method_name,
            arguments,
            loc,
        };
        call.execute(func_ref, heap)
    }
}

impl Interpretable for NewExpression {
    fn interpret(&self, heap: &mut Heap) -> Result<Interpreted, Exception> {
        let NewExpression(callee_expr, argument_exprs) = self;

        let loc = heap.loc.clone();

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
        let call = CallContext {
            this_ref: object_ref,
            method_name: "<constructor>".to_string(),
            arguments,
            loc,
        };
        let result = call.execute(funcref, heap)?;
        match result {
            Interpreted::Value(JSValue::Ref(r)) if r != Heap::NULL => Ok(result),
            _ => Ok(Interpreted::from(object_ref)),
        }
    }
}

impl Interpretable for FunctionExpression {
    fn interpret(&self, heap: &mut Heap) -> Result<Interpreted, Exception> {
        let closure = Closure {
            function: self.func.clone(),
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
