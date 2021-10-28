use std::collections::HashMap;

use crate::{RuntimeError, Value};

type BuiltinResult = Result<Value, RuntimeError>;

#[derive(Debug, Clone, PartialEq)]
pub enum Builtin {
    Println = 1,
    Len = 2,
}

impl Builtin {
    pub fn call(&self, args: Vec<Value>) -> BuiltinResult {
        match &self {
            Builtin::Println => builtin_println(args),
            Builtin::Len => builtin_len(args),
        }
    }

    pub fn to_name(&self) -> String {
        match &self {
            Builtin::Println => String::from("last"),
            Builtin::Len => String::from("len"),
        }
    }

    pub fn builtins() -> Vec<Builtin> {
        vec![Builtin::Println, Builtin::Len]
    }
}

impl From<u8> for Builtin {
    fn from(f: u8) -> Self {
        match f {
            0x1 => Builtin::Println,
            0x2 => Builtin::Len,
            _ => panic!("can get builtin from {}", f),
        }
    }
}

pub fn get_builtins() -> HashMap<String, Value> {
    let mut map = HashMap::new();

    map.insert(
        String::from("println"),
        Value::native_function("println", 1, Builtin::Println),
    );

    map.insert(
        String::from("len"),
        Value::native_function("len", 1, Builtin::Len),
    );

    map
}

fn builtin_println(args: Vec<Value>) -> BuiltinResult {
    match &args[0] {
        obj => {
            println!("{}", obj);
            Ok(Value::Null)
        }
    }
}

fn builtin_len(args: Vec<Value>) -> BuiltinResult {
    match &args[0] {
        Value::Array(arr) => Ok(Value::Int(arr.len() as i64)),
        Value::String(s1) => Ok(Value::Int(s1.len() as i64)),
        _ => {
            panic!("invalid len")
        }
    }
}
