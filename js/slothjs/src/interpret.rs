use std::collections::HashMap;

use serde_json::json;

use crate::value::{JSON, JSValue, UNDEFINED};
use crate::error::Exception;
use crate::ast::*;      // yes, EVERYTHING


// ==============================================

pub struct RuntimeState {
    variables: HashMap<String, JSValue>,
}

impl RuntimeState {
    pub fn new() -> Self {
        let variables = HashMap::new();
        RuntimeState{ variables }
    }

    fn declare_identifier(&mut self, name: String) -> Result<(), Exception> {
        self.variables.entry(name).or_insert(UNDEFINED);
        Ok(())
    }

    fn set_identifier(&mut self, name: &str, value: JSValue) -> Result<(), Exception> {
        let entry = self.variables.get_mut(name)
            .ok_or(Exception::ReferenceError(name.to_string()))?;
        *entry = value;
        Ok(())
    }

    fn lookup_identifier(&self, name: &str) -> Result<JSValue, Exception> {
        match self.variables.get(name) {
            None => Err(Exception::ReferenceError(name.to_string())),
            Some(value) => Ok(value.clone()),
        }
    }
}

// ==============================================

pub trait Interpretable {
    fn interpret(&self, state: &mut RuntimeState) -> Result<JSValue, Exception>;
}

// ==============================================

impl Interpretable for Program {
    fn interpret(&self, state: &mut RuntimeState) -> Result<JSValue, Exception> {
        let mut result = UNDEFINED;
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
            Statement::Empty                        => Ok(UNDEFINED),
            Statement::Expression(stmt)             => stmt.interpret(state),
            Statement::Block(stmt)                  => stmt.interpret(state),
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
        Ok(UNDEFINED)
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
            state.declare_identifier(decl.name.clone())?;
            if let Some(init) = &decl.init {
                let value = init.interpret(state)?;
                state.set_identifier(&decl.name, value)?;
            }
        }
        Ok(UNDEFINED)
    }
}

impl Interpretable for Expr {
    fn interpret(&self, state: &mut RuntimeState) -> Result<JSValue, Exception> {
        match self {
            Expr::Literal(val) =>
                Ok(val.clone()),
            Expr::Identifier(name) =>
                state.lookup_identifier(&name),
            Expr::BinaryOp(lexpr, op, rexpr) => {
                let lval = lexpr.interpret(state)?;
                let rval = rexpr.interpret(state)?;
                match op {
                    BinOp::Add => {
                        if !(lval.0.is_string() || rval.0.is_string()) {
                            if let Some(lnum) = lval.numberify() {
                                if let Some(rnum) = rval.numberify() {
                                    return Ok(JSValue::from(lnum + rnum));
                                }
                            }
                        }
                        let lvalstr = lval.stringify();
                        let rvalstr = rval.stringify();
                        return Ok(JSValue::from(lvalstr + &rvalstr));
                    }
                    BinOp::KindaEqual => {
                        Ok(JSValue::from(JSON::Bool(lval == rval)))
                    }
                }
            }
            Expr::Call(_callee, _arguments) => {
                panic!("TODO: Expr::Call");
            }
            Expr::Array(elements) => {
                let mut arr = vec![];
                for elem in elements.iter() {
                    let value = elem.interpret(state)?;
                    arr.push(value.0);
                }
                Ok(JSValue::from(JSON::Array(arr)))
            }
            Expr::Member(objexpr, propexpr, computed) => {
                let object = objexpr.interpret(state)?;
                let property = if *computed {
                    propexpr.interpret(state)?
                } else {
                    match &**propexpr {
                        Expr::Identifier(name) => JSValue::from(name.clone()),
                        _ => panic!("Member(computed=false) property is not an identifier")
                    }
                };
                if let Some(arr) = object.0.as_array() {
                    match property.0.as_i64() {
                        Some(index) => {
                            let j = arr.get(index as usize).unwrap_or(&UNDEFINED.0).clone();
                            Ok(JSValue::from(j))
                        }
                        None => Ok(UNDEFINED),
                    }
                } else if let Some(obj) = object.0.as_object() {
                    let prop = property.stringify();
                    let j = obj.get(&prop).unwrap_or(&UNDEFINED.0).clone();
                    Ok(JSValue::from(j))
                } else {
                    Ok(UNDEFINED)
                }
            }
            Expr::Object(properties) => {
                let mut object = json!({});
                for (key, valexpr) in properties.iter() {
                    let keyname = match key {
                        ObjectKey::Identifier(ident) =>
                            ident.clone(),
                        ObjectKey::Computed(expr) => {
                            let result = expr.interpret(state)?;
                            result.stringify()
                        }
                    };
                    let value = valexpr.interpret(state)?;
                    object[keyname] = value.0;
                }
                Ok(JSValue::from(object))
            }
        }
    }
}

