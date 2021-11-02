use std::{
    cell::{Ref, RefCell},
    rc::Rc,
    sync::Arc,
};

use ast::{Identifier, Statement};

use crate::{builtin::Builtin, environment::Environment};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),
    Array(Rc<RefCell<Array>>),
    Function(FunctionValue),
    DataClass(DataClass),
    NativeFunction(NativeFunction),
    ReturnVal(Box<Value>),
    Null,
    Unit,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Array {
    pub values: Vec<Value>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DataClass {
    pub name: Identifier,
    pub fields: Vec<Identifier>,
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

    pub fn return_val(val: Value) -> Value {
        Value::ReturnVal(Box::new(val))
    }

    pub fn data_class(name: Identifier, fields: Vec<Identifier>) -> Value {
        Value::DataClass(DataClass { name, fields })
    }

    pub fn array(values: Vec<Value>) -> Value {
        Value::Array(Rc::new(RefCell::new(Array { values })))
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Int(v) => write!(f, "{}", v),
            Value::Float(v) => write!(f, "{}", v),
            Value::String(v) => write!(f, r#""{}""#, v),
            Value::Bool(v) => write!(f, "{}", v),
            Value::Array(v) => {
                write!(f, "[")?;
                let mut out = vec![];
                for val in &v.borrow().values {
                    out.push(val.to_string());
                }
                write!(f, "{}]", out.join(", "))
            }
            Value::Null => write!(f, "null"),
            Value::Unit => write!(f, "Unit"),
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
            Value::DataClass(v) => {
                write!(f, "<dataClass {}>", v.name.value())
            }
            Value::ReturnVal(_) => write!(f, ""),
        }
    }
}
