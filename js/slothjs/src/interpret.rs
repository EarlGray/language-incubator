use std::convert::TryFrom;

//use crate::object;
use crate::object::{JSObject, JSRef, JSValue, Heap, Place};
use crate::error::Exception;
use crate::ast::*;      // yes, EVERYTHING


// ==============================================

pub struct RuntimeState {
    pub heap: Heap,

    pub global_object: JSRef,
}

impl RuntimeState {
    pub fn new() -> Self {
        let mut heap = Heap::new();
        let global_object = heap.new_object();
        RuntimeState{ heap, global_object }
    }

    fn get_global(&self) -> &JSObject {
        match self.heap.get(self.global_object) {
            JSValue::Object(global) => global,
            _ => panic!("global is not an object"),
        }
    }

    /*
    fn get_global_mut(&mut self) -> &mut JSObject {
        match self.heap.get_mut(self.global_object) {
            JSValue::Object(global) => global,
            _ => panic!("global is not an object"),
        }
    }
    */

    fn declare_var(&mut self, name: &str, init: Option<JSValue>) -> Result<(), Exception> {
        let value = init.unwrap_or(JSValue::Undefined);
        self.set_identifier(name, value)
    }

    fn set_identifier(&mut self, name: &str, value: JSValue) -> Result<(), Exception> {
        let propref = self.heap.property_or_create(self.global_object, name)
            .expect("Global is not an object");
        *self.heap.get_mut(propref) = value;
        Ok(())
    }

    fn lookup_identifier(&self, name: &str) -> Result<JSRef, Exception> {
        let global = self.get_global();
        global.property_ref(name)
            .ok_or(Exception::ReferenceError(name.to_string()))
    }
}

// ==============================================

pub trait Interpretable {
    fn interpret(&self, state: &mut RuntimeState) -> Result<JSValue, Exception>;
}

pub trait InterpretableMut {
    fn interpret_mut(&self, state: &mut RuntimeState) -> Result<Place, Exception>;

    /*
    fn interpret(&self, state: &mut RuntimeState) -> Result<JSValue, Exception> {
        let objref = self.interpret_mut(state)?;
        let value = state.heap.get(objref).clone();
        Ok(value)
    }
    */
}


// ==============================================

impl Interpretable for Program {
    fn interpret(&self, state: &mut RuntimeState) -> Result<JSValue, Exception> {
        let mut result = JSValue::Undefined;
        for stmt in self.body.iter() {
            result = stmt.interpret(state)?;
        }
        Ok(result)
    }
}

// ==============================================

impl Interpretable for Statement {
    fn interpret(&self, state: &mut RuntimeState) -> Result<JSValue, Exception> {
        match self {
            Statement::Empty                        => Ok(JSValue::Undefined),
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
    fn interpret(&self, state: &mut RuntimeState) -> Result<JSValue, Exception> {
        for stmt in self.body.iter() {
            stmt.interpret(state)?;
        }
        Ok(JSValue::Undefined)
    }
}

impl Interpretable for IfStatement {
    fn interpret(&self, state: &mut RuntimeState) -> Result<JSValue, Exception> {
        let jbool = self.test.interpret(state)?;
        let cond = jbool.boolify();
        if cond {
            self.consequent.interpret(state)
        } else if let Some(else_stmt) = self.alternate.as_ref() {
            else_stmt.interpret(state)
        } else {
            Ok(JSValue::Undefined)
        }
    }
}

impl Interpretable for ForStatement {
    fn interpret(&self, state: &mut RuntimeState) -> Result<JSValue, Exception> {
        self.init.interpret(state)?;
        loop {
            // test
            let testval = match self.test.as_ref() {
                None => true,
                Some(testexpr) =>
                    testexpr.interpret(state)?.boolify(),
            };
            if !testval { break }

            // body
            self.body.interpret(state)?;

            // update
            if let Some(updateexpr) = self.update.as_ref() {
                updateexpr.interpret(state)?;
            }
        }
        Ok(JSValue::Undefined)
    }
}

impl Interpretable for ExpressionStatement {
    fn interpret(&self, state: &mut RuntimeState) -> Result<JSValue, Exception> {
        self.expression.interpret(state)
    }
}

impl Interpretable for VariableDeclaration {
    fn interpret(&self, state: &mut RuntimeState) -> Result<JSValue, Exception> {
        for decl in self.declarations.iter() {
            let mut init = None;
            if let Some(initexpr) = decl.init.as_ref() {
                init = Some(initexpr.interpret(state)?);
            };
            state.declare_var(&decl.name, init)?;
        }
        Ok(JSValue::Undefined)
    }
}

impl Interpretable for Expr {
    fn interpret(&self, state: &mut RuntimeState) -> Result<JSValue, Exception> {
        match self {
            Expr::Literal(expr) =>              expr.interpret(state),
            Expr::Identifier(expr) =>           expr.interpret(state),
            Expr::BinaryOp(expr) =>             expr.interpret(state),
            Expr::Call(_expr) =>                todo!(),
            Expr::Array(_expr) =>               todo!(),
            Expr::Member(expr) =>               expr.interpret(state),
            Expr::Object(expr) =>               expr.interpret(state),
            Expr::Assign(expr) =>               expr.interpret(state),
            Expr::Conditional(expr) =>          expr.interpret(state),
        }
    }
}

impl InterpretableMut for Expr {
    fn interpret_mut(&self, state: &mut RuntimeState) -> Result<Place, Exception> {
        match self {
            Expr::Identifier(expr) =>       expr.interpret_mut(state),
            Expr::Assign(expr) =>           expr.interpret_mut(state),
            Expr::Member(expr) =>           expr.interpret_mut(state),
            _ => {
                let msg = format!("Invalid left-hand side: {:?}", self);
                Err(Exception::ReferenceError(msg))
            }
        }
    }
}

impl Interpretable for Literal {
    fn interpret(&self, state: &mut RuntimeState) -> Result<JSValue, Exception> {
        if let Ok(value) = JSValue::try_from(&self.0) {
            Ok(value)
        } else {
            let object = state.heap.object_from_json(&self.0);
            Ok(object)
        }
    }
}

impl InterpretableMut for Identifier {
    fn interpret_mut(&self, state: &mut RuntimeState) -> Result<Place, Exception> {
        state.lookup_identifier(&self.0).map(|href| Place::Ref(href))
    }
}

impl Interpretable for Identifier {
    fn interpret(&self, state: &mut RuntimeState) -> Result<JSValue, Exception> {
        if self.0 == "undefined" {
            return Ok(JSValue::Undefined);
        }
        let place = self.interpret_mut(state)?;
        Ok(state.heap.place(place).clone())
    }
}

impl Interpretable for BinaryExpression {
    fn interpret(&self, state: &mut RuntimeState) -> Result<JSValue, Exception> {
        let BinaryExpression(lexpr, op, rexpr) = self;
        let lval = lexpr.interpret(state)?;
        let rval = rexpr.interpret(state)?;
        match op {
            BinOp::Plus => {
                if !(lval.is_string() || rval.is_string()) {
                    if let Some(lnum) = lval.numberify() {
                        if let Some(rnum) = rval.numberify() {
                            return Ok(JSValue::from(lnum + rnum));
                        }
                    }
                }
                let lvalstr = lval.stringify(&state.heap);
                let rvalstr = rval.stringify(&state.heap);
                return Ok(JSValue::from(lvalstr + &rvalstr));
            }
            BinOp::EqEq => {
                // TODO: Abstract Equality Comparison
                Ok(JSValue::from(lval == rval))
            }
            BinOp::Less => {
                // TODO: Abstract Relational Comparison
                // TODO: toPrimitive()
                if let JSValue::String(lstr) = lval.clone() {
                    if let JSValue::String(rstr) = rval.clone() {
                        return Ok(JSValue::from(lstr < rstr));
                    }
                };
                let lnum = lval.numberify().unwrap_or(f64::NAN);
                let rnum = rval.numberify().unwrap_or(f64::NAN);
                Ok(JSValue::from(lnum < rnum))
            }
        }
    }
}

impl InterpretableMut for MemberExpression {
    fn interpret_mut(&self, state: &mut RuntimeState) -> Result<Place, Exception> {
        let MemberExpression(objexpr, propexpr, computed) = self;

        // compute the name of the property:
        let propname = if *computed {
            propexpr.interpret(state)?.stringify(&state.heap)
        } else {
            match &**propexpr {
                Expr::Identifier(name) => name.0.clone(),
                _ => panic!("Member(computed=false) property is not an identifier")
            }
        };

        // get the object reference for member computation:
        let objref = match objexpr.interpret_mut(state)? {
            Place::Ref(objref) => objref,
            Place::ObjectMember{..} => {
                let err = format!("Cannot set property {} of undefined", propname);
                return Err(Exception::TypeError(err))
            }
        };

        if let JSValue::Object(object) = state.heap.get(objref) {
            match object.property_ref(&propname) {
                Some(propref) => Ok(Place::Ref(propref)),
                None => Ok(Place::ObjectMember{ objref, member: propname })
            }
        } else {
            let err = format!("Cannot set property {} of undefined", propname);
            Err(Exception::TypeError(err))
        }
    }
}

impl Interpretable for MemberExpression {
    fn interpret(&self, state: &mut RuntimeState) -> Result<JSValue, Exception> {
        let mut place = self.interpret_mut(state)?;
        Ok(state.heap.place_mut(&mut place).clone())
    }
}

impl Interpretable for ObjectExpression {
    fn interpret(&self, state: &mut RuntimeState) -> Result<JSValue, Exception> {
        let mut object = JSObject::new();

        for (key, valexpr) in self.0.iter() {
            let keyname = match key {
                ObjectKey::Identifier(ident) =>
                    ident.clone(),
                ObjectKey::Computed(expr) => {
                    let result = expr.interpret(state)?;
                    result.stringify(&mut state.heap)
                }
            };
            let value = valexpr.interpret(state)?;

            let propref = state.heap.allocate(value);
            object.set_property(&keyname, propref);
        }

        Ok(JSValue::Object(object))
    }
}

impl InterpretableMut for AssignmentExpression {
    fn interpret_mut(&self, state: &mut RuntimeState) -> Result<Place, Exception> {
        let AssignmentExpression(leftexpr, op, rightexpr) = self;
        let value = rightexpr.interpret(state)?;
        let mut place = leftexpr.interpret_mut(state)?;
        match op {
            AssignOp::Equal => {
                *state.heap.place_mut(&mut place) = value;
            }
        };
        Ok(place)
    }
}

impl Interpretable for AssignmentExpression {
    fn interpret(&self, state: &mut RuntimeState) -> Result<JSValue, Exception> {
        let place = self.interpret_mut(state)?;
        Ok(state.heap.place(place).clone())
    }
}


impl Interpretable for ConditionalExpression {
    fn interpret(&self, state: &mut RuntimeState) -> Result<JSValue, Exception> {
        let ConditionalExpression(condexpr, thenexpr, elseexpr) = self;
        let cond = condexpr.interpret(state)?;
        let value = if cond.boolify() {
            thenexpr.interpret(state)?
        } else {
            elseexpr.interpret(state)?
        };
        Ok(value)
    }
}
