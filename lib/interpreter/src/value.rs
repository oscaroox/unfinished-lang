use std::{cell::RefCell, collections::HashMap, rc::Rc};

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
    DataClassInstance(Rc<RefCell<DataClassInstance>>),
    NativeFunction(NativeFunction),
    ReturnVal(Box<Value>),
    Break,
    Continue,
    Null,
    Unit,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DataClassInstance {
    pub name: Identifier,
    pub data_class: DataClass,
    pub keys: Vec<Identifier>,
    pub fields: HashMap<String, Value>,
}

impl DataClassInstance {
    pub fn get(&self, name: String) -> Value {
        match self.fields.get(&name) {
            Some(v) => v.clone(),
            None => match self.data_class.instance_methods.get(&name) {
                Some(v) => Value::Function(v.clone()),
                None => panic!("Undefined property {}", name),
            },
        }
    }

    pub fn get_method(&self, name: String) -> Value {
        match self.data_class.instance_methods.get(&name) {
            Some(v) => Value::Function(v.clone()),
            None => panic!("Undefined method {}", name),
        }
    }

    pub fn set(&mut self, name: String, value: Value) {
        self.fields.insert(name, value.clone());
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct DataClass {
    pub name: Identifier,
    pub fields: Vec<Identifier>,
    pub static_methods: HashMap<String, FunctionValue>,
    pub instance_methods: HashMap<String, FunctionValue>,
}

impl DataClass {
    pub fn get(&self, name: String) -> FunctionValue {
        match self.static_methods.get(&name) {
            Some(v) => v.clone(),
            None => panic!("Undefined method {}", name),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Array {
    pub values: Vec<Value>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionValue {
    pub name: Option<String>,
    pub arity: u8,
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
        arity: u8,
        body: Vec<Statement>,
        closure: Rc<RefCell<Environment>>,
    ) -> Value {
        Value::Function(FunctionValue {
            name,
            params,
            arity,
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

    pub fn data_class(
        name: Identifier,
        fields: Vec<Identifier>,
        static_methods: HashMap<String, FunctionValue>,
        instance_methods: HashMap<String, FunctionValue>,
    ) -> Value {
        Value::DataClass(DataClass {
            name,
            fields,
            static_methods,
            instance_methods,
        })
    }

    pub fn array(values: Vec<Value>) -> Value {
        Value::Array(Rc::new(RefCell::new(Array { values })))
    }

    pub fn data_class_instance(
        name: Identifier,
        data_class: DataClass,
        fields: HashMap<String, Value>,
        keys: Vec<Identifier>,
    ) -> Value {
        Value::DataClassInstance(Rc::new(RefCell::new(DataClassInstance {
            name,
            data_class,
            fields,
            keys,
        })))
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Int(v) => write!(f, "{}", v),
            Value::Float(v) => write!(f, "{}", v),
            Value::String(v) => write!(f, r#"{}"#, v),
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
                write!(f, "<dataClass {}>", v.name.value)
            }
            Value::DataClassInstance(v) => {
                let name = v.borrow().name.value.to_string();
                write!(f, "{} {{ ", name)?;
                let mut out = vec![];

                for key in &v.borrow().keys {
                    if let Some(field) = &v.borrow().fields.get(&key.value) {
                        out.push(format!("{}: {}", key, field))
                    }
                }
                write!(f, "{} }}", out.join(", "))
            }
            Value::ReturnVal(_) => write!(f, ""),
            Value::Break => write!(f, ""),
            Value::Continue => write!(f, ""),
        }
    }
}
