use std::{cell::RefCell, collections::HashMap, rc::Rc};

use ast::{Expression, Identifier};

use crate::{builtin::Builtin, environment::Environment};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),
    Array(Rc<RefCell<Array>>),
    Range(i64, i64),
    Function(FunctionValue),
    DataClass(DataStruct),
    DataClassInstance(Rc<RefCell<DataStructInstance>>),
    NativeFunction(NativeFunction),
    ReturnVal(Box<Value>),
    ImplicitReturnVal(Box<Value>),
    Break,
    Continue,
    Null,
    Unit,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DataStructInstance {
    pub name: Identifier,
    pub data_struct: DataStruct,
    pub keys: Vec<Identifier>,
    pub fields: HashMap<String, Value>,
}

impl DataStructInstance {
    pub fn get(&self, name: String) -> Value {
        match self.fields.get(&name) {
            Some(v) => v.clone(),
            None => match self.data_struct.instance_methods.get(&name) {
                Some(v) => Value::Function(v.clone()),
                None => panic!("Undefined property {}", name),
            },
        }
    }

    pub fn get_method(&self, name: String) -> Value {
        match self.data_struct.instance_methods.get(&name) {
            Some(v) => Value::Function(v.clone()),
            None => panic!("Undefined method {}", name),
        }
    }

    pub fn set(&mut self, name: String, value: Value) {
        self.fields.insert(name, value.clone());
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct DataStruct {
    pub name: Identifier,
    pub fields: Vec<Identifier>,
    pub static_methods: HashMap<String, FunctionValue>,
    pub instance_methods: HashMap<String, FunctionValue>,
}

impl DataStruct {
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
    pub body: Expression,
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
        body: Expression,
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

    pub fn implicit_return_val(val: Value) -> Value {
        Value::ImplicitReturnVal(Box::new(val))
    }

    pub fn data_struct(
        name: Identifier,
        fields: Vec<Identifier>,
        static_methods: HashMap<String, FunctionValue>,
        instance_methods: HashMap<String, FunctionValue>,
    ) -> Value {
        Value::DataClass(DataStruct {
            name,
            fields,
            static_methods,
            instance_methods,
        })
    }

    pub fn array(values: Vec<Value>) -> Value {
        Value::Array(Rc::new(RefCell::new(Array { values })))
    }

    pub fn data_struct_instance(
        name: Identifier,
        data_struct: DataStruct,
        fields: HashMap<String, Value>,
        keys: Vec<Identifier>,
    ) -> Value {
        Value::DataClassInstance(Rc::new(RefCell::new(DataStructInstance {
            name,
            data_struct,
            fields,
            keys,
        })))
    }
}

impl Value {
    pub fn to_type(&self) -> String {
        match self {
            Value::Int(_) => String::from("int"),
            Value::Float(_) => String::from("float"),
            Value::String(_) => String::from("string"),
            Value::Bool(_) => String::from("bool"),
            Value::Array(_) => String::from("array"),
            Value::Range(_, _) => String::from("range"),
            Value::Function(_) => String::from("function"),
            Value::DataClass(_) => String::from("data_struct"),
            Value::DataClassInstance(_) => String::from("data_struct_instance"),
            Value::NativeFunction(_) => String::from("native_function"),
            Value::ReturnVal(_) => String::from("return"),
            Value::ImplicitReturnVal(_) => String::from("implicit_return"),
            Value::Break => String::from("break"),
            Value::Continue => String::from("continue"),
            Value::Null => String::from("null"),
            Value::Unit => String::from("unit"),
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Int(v) => write!(f, "{}", v),
            Value::Float(v) => write!(f, "{}", v),
            Value::String(v) => write!(f, r#""{}""#, v),
            Value::Bool(v) => write!(f, "{}", v),
            Value::Range(start, end) => write!(f, "range({}, {})", start, end),
            Value::Array(v) => {
                write!(f, "[")?;
                let mut out = vec![];
                for val in &v.borrow().values {
                    out.push(val.to_string());
                }
                write!(f, "{}]", out.join(", "))
            }
            Value::Null => write!(f, "null"),
            Value::Unit => write!(f, "unit"),
            Value::Function(v) => {
                let n = if let Some(n) = &v.name {
                    n.to_string()
                } else {
                    "anonymous".to_string()
                };
                write!(f, "<fun {}>", n)
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
            Value::ImplicitReturnVal(_) => write!(f, ""),
            Value::ReturnVal(v) => write!(f, "<return {}>", v),
            Value::Break => write!(f, "<break>"),
            Value::Continue => write!(f, "<continue>"),
        }
    }
}
