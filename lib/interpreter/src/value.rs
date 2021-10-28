use std::{cell::RefCell, rc::Rc};

use ast::{Identifier, Literal, Statement};

use crate::{builtin::Builtin, environment::Environment};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),
    Array(Vec<Value>),
    Function(FunctionValue),
    NativeFunction(NativeFunction),
    Null,
    Empty,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionValue {
    pub name: Option<String>,
    pub params: Vec<Identifier>,
    pub body: Vec<Statement>,
    pub closure: Rc<RefCell<Environment>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NativeFunction {
    pub name: String,
    pub arity: u8,
    pub builtin: Builtin,
}

impl Value {
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            _ => false,
        }
    }

    pub fn function(
        name: Option<String>,
        params: Vec<Identifier>,
        body: Vec<Statement>,
        closure: Rc<RefCell<Environment>>,
    ) -> Value {
        Value::Function(FunctionValue {
            name,
            params,
            body,
            closure,
        })
    }

    pub fn native_function(name: impl Into<String>, arity: u8, builtin: Builtin) -> Value {
        Value::NativeFunction(NativeFunction {
            name: name.into(),
            arity,
            builtin,
        })
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Int(v) => write!(f, "{}", v),
            Value::Float(v) => write!(f, "{}", v),
            Value::String(v) => write!(f, "{}", v),
            Value::Bool(v) => write!(f, "{}", v),
            Value::Array(v) => {
                let mut out = String::new();
                out += "[";
                for (i, val) in v.iter().enumerate() {
                    out += &val.to_string();
                    if v.len() < i {
                        out += ",";
                    }
                }
                out += "]";
                write!(f, "{}", out)
            }
            Value::Null => write!(f, "null"),
            Value::Empty => write!(f, ""),
            Value::Function(v) => {
                let n = if let Some(n) = &v.name {
                    n.to_string()
                } else {
                    "anonymous".to_string()
                };
                write!(f, "<fun {}/>", n)
            }
            Value::NativeFunction(v) => {
                write!(f, "<nativeFun {}>", v.name)
            }
        }
    }
}
