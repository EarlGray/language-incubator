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

    fn declare_var(&mut self, name: &str, init: Option<Interpreted>) -> Result<(), Exception> {
        let value = init.unwrap_or(Interpreted::VOID);
        self.heap.property_assign(Heap::GLOBAL, name, &value)?;
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
    fn interpret(&self, _state: &mut RuntimeState) -> Result<Interpreted, Exception> {
        if self.0 == "undefined" {
            return Ok(Interpreted::Value(JSValue::Undefined));
        }
        let name = self.0.clone();

        Ok(Interpreted::Member{
            of: Box::new(Interpreted::Ref(Heap::GLOBAL)),
            name
        })
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
        match op {
            BinOp::Plus => {
                if !(lval.is_string() || rval.is_string()) {
                    if let Some(lnum) = lval.numberify() {
                        if let Some(rnum) = rval.numberify() {
                            return Ok(Interpreted::Value(JSValue::from(lnum + rnum)));
                        }
                    }
                }
                let lvalstr = lval.stringify(&state.heap);
                let rvalstr = rval.stringify(&state.heap);
                let result = JSValue::from(lvalstr + &rvalstr);
                return Ok(Interpreted::Value(result));
            }
            BinOp::EqEq => {
                let result = lval.loose_eq(&rval);
                Ok(Interpreted::Value(JSValue::Bool(result)))
            }
            BinOp::Less => {
                // TODO: Abstract Relational Comparison
                // TODO: toPrimitive()
                if let JSValue::String(lstr) = lval.clone() {
                    if let JSValue::String(rstr) = rval.clone() {
                        let result = JSValue::from(lstr < rstr);
                        return Ok(Interpreted::Value(result));
                    }
                };
                let lnum = lval.numberify().unwrap_or(f64::NAN);
                let rnum = rval.numberify().unwrap_or(f64::NAN);
                let result = JSValue::from(lnum < rnum);
                Ok(Interpreted::Value(result))
            }
        }
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
            let err = format!("Cannot set property {} of undefined", propname);
            Exception::TypeError(err)
        })?;

        let object = state.heap.get(objref).to_object().map_err(|_| {
            let err = format!("Cannot set property {} of undefined", propname);
            Exception::TypeError(err)
        })?;

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
            object.set_property(&keyname, propref);
        }

        let object = JSValue::Object(object);
        Ok(Interpreted::Value(object))
    }
}

impl Interpretable for AssignmentExpression {
    fn interpret(&self, state: &mut RuntimeState) -> Result<Interpreted, Exception> {
        let AssignmentExpression(leftexpr, op, valexpr) = self;

        let value = valexpr.interpret(state)?;
        let assignee = leftexpr.interpret(state)?;
        match assignee {
            Interpreted::Member{of, name} => {
                let objref = of.to_ref_or_allocate(&mut state.heap)?;
                state.heap.property_assign(objref, &name, &value)?;
            }
            Interpreted::Ref(href) => {
                *state.heap.get_mut(href) = value.to_value(&state.heap)?;
            }
            _ => {
                let msg = format!("Invalid left hand side: {:?}", &value);
                return Err(Exception::TypeError(msg));
            }
        };

        Ok(value)
    }
}

impl Interpretable for CallExpression {
    fn interpret(&self, state: &mut RuntimeState) -> Result<Interpreted, Exception> {
        let CallExpression(callee_expr, arguments_expr) = self;

        let callee = callee_expr.interpret(state)?;

        let (this_ref, method_name) = match callee {
            Interpreted::Ref(_func_ref) =>
                todo!(),
            Interpreted::Member{ of, name } => {
                let this_ref = of.to_ref(&state.heap)?;
                (this_ref, name.clone())
            }
            Interpreted::Value(value) => {
                let msg = format!("{:?} is not a function", value);
                return Err(Exception::TypeError(msg));
            }
        };

        let mut arguments = vec![];
        for argexpr in arguments_expr.iter() {
            let arg = argexpr.interpret(state)?;
            arguments.push(arg);
        }

        let this_object = state.heap.get(this_ref).to_object()?;
        let property = match this_object.properties.get(&method_name) {
            Some(prop) => prop,
            None => {
                let msg = format!("{:?}.{} is not a function", callee_expr, &method_name);
                return Err(Exception::TypeError(msg));
            }
        };
        match property.content {
            object::Content::Data(_) => {
                let msg = format!("{:?}.{} is not a function", callee_expr, &method_name);
                return Err(Exception::TypeError(msg));
            }
            object::Content::NativeFunction(func) => {
                func(this_ref, method_name, arguments, &mut state.heap)
            }
        }
    }
}
