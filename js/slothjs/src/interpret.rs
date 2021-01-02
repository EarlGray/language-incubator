use std::convert::TryFrom;

use crate::object;
use crate::object::{JSValue, Heap, Interpreted};
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

    fn declare_var(&mut self, name: &str, mut init: Option<Interpreted>) -> Result<(), Exception> {
        let global_object = self.heap.get(Heap::GLOBAL).to_object()?;
        if !global_object.properties.contains_key(name) && init.is_none() {
            init = Some(Interpreted::VOID);
        }
        if let Some(value) = init {
            self.heap.property_assign(Heap::GLOBAL, name, &value)?;
        }
        Ok(())
    }

    fn lookup_var(&mut self, name: &str) -> Option<Interpreted> {
        let global_object = self.heap.get(Heap::GLOBAL).to_object().unwrap();
        global_object.properties.get(name).map(|prop|
            match prop.content {
                object::Content::Data(dataref) =>
                    Interpreted::Ref(dataref),
                object::Content::NativeFunction(_) =>
                    Interpreted::Member{
                        of: Box::new(Interpreted::Ref(Heap::GLOBAL)),
                        name: name.to_string()
                    },
            }
        )
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

impl Interpretable for VariableDeclaration {
    fn interpret(&self, state: &mut RuntimeState) -> Result<Interpreted, Exception> {
        for decl in self.declarations.iter() {
            let mut init = None;
            if let Some(initexpr) = decl.init.as_ref() {
                let result = initexpr.interpret(state)?;
                init = Some(result);
            };
            state.declare_var(&decl.name, init)?;
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
        }
    }
}


impl Interpretable for Literal {
    fn interpret(&self, state: &mut RuntimeState) -> Result<Interpreted, Exception> {
        let value = JSValue::try_from(&self.0).unwrap_or_else(|_|
            state.heap.object_from_json(&self.0)
        );
        Ok(Interpreted::Value(value))
    }
}

impl Interpretable for Identifier {
    fn interpret(&self, state: &mut RuntimeState) -> Result<Interpreted, Exception> {
        let name = &self.0;
        // TODO: this should be a readonly property of global
        if name == "undefined" {
            Ok(Interpreted::Value(JSValue::Undefined))
        } else if let Some(ret) = state.lookup_var(name) {
            Ok(ret)
        // TODO: this should be a readonly property of global
        } else if name == "NaN" {
            Ok(Interpreted::Value(JSValue::from(f64::NAN)))
        } else {
            Err(Exception::ReferenceNotFound(name.to_string()))
        }
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
            BinOp::Plus => JSValue::plus(&lval, &rval, &state.heap),
            BinOp::Less => JSValue::less(&lval, &rval, &state.heap),
        };
        Ok(Interpreted::Value(result))
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

        let object = state.heap.get(objref).to_object().map_err(|_|
            Exception::ReferenceNotAnObject(objresult.clone())
        )?;

        let result = match object.property_ref(&propname) {
            Some(propref) => Interpreted::Ref(propref),
            None => Interpreted::Member{
                of: Box::new(Interpreted::Ref(objref)),
                name: propname.to_string()
            }
        };
        Ok(result)
    }
}

impl Interpretable for ObjectExpression {
    fn interpret(&self, state: &mut RuntimeState) -> Result<Interpreted, Exception> {
        let mut object = object::JSObject::new();

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

        let object = JSValue::Object(object);
        Ok(Interpreted::Value(object))
    }
}

impl Interpretable for AssignmentExpression {
    fn interpret(&self, state: &mut RuntimeState) -> Result<Interpreted, Exception> {
        let AssignmentExpression(leftexpr, op, valexpr) = self;

        // this must go before a possible declare_var()
        let value = valexpr.interpret(state)?;

        if let Expr::Identifier(name) = leftexpr.as_ref() {
            // `a = 1` should create a variable;
            // `a.one = 1` without `a` should fail.
            state.declare_var(&name.0, None)?;
        };
        let assignee = leftexpr.interpret(state)?;

        match assignee {
            Interpreted::Member{of, name} => {
                match op {
                    AssignOp::Equal => Ok(()),
                    // _ => Err()
                }?;
                let objref = of.to_ref(&state.heap)?;
                state.heap.property_assign(objref, &name, &value)?;
            }
            Interpreted::Ref(href) => {
                let value = match op {
                    AssignOp::Equal => value.to_value(&state.heap)?
                };
                *state.heap.get_mut(href) = value;
            }
            _ => {
                return Err(Exception::TypeErrorCannotAssign(assignee));
            }
        };

        Ok(value)
    }
}

impl Interpretable for CallExpression {
    fn interpret(&self, state: &mut RuntimeState) -> Result<Interpreted, Exception> {
        let CallExpression(callee_expr, arguments_expr) = self;

        let callee = callee_expr.interpret(state)?;

        let (this_ref, method_name) = match &callee {
            Interpreted::Ref(_func_ref) =>
                todo!(),
            Interpreted::Member{ of, name } => {
                let this_ref = of.to_ref(&state.heap)?;
                (this_ref, name.clone())
            }
            Interpreted::Value(_) =>
                return Err(Exception::TypeErrorNotCallable(callee.clone()))
        };

        let mut arguments = vec![];
        for argexpr in arguments_expr.iter() {
            let arg = argexpr.interpret(state)?;
            arguments.push(arg);
        }

        let this_object = state.heap.get(this_ref).to_object()?;
        let property = this_object.properties.get(&method_name)
            .ok_or(Exception::TypeErrorNotCallable(callee.clone()))?;
        let func = match property.content {
            object::Content::NativeFunction(func) => func,
            _ => return Err(Exception::TypeErrorNotCallable(callee.clone()))
        };
        func(this_ref, method_name, arguments, &mut state.heap)
    }
}
