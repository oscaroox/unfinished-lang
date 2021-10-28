use std::collections::HashMap;

use crate::{RuntimeError, Value};

type BuiltinResult = Result<Value, RuntimeError>;

#[derive(Debug, Clone, PartialEq)]
pub enum Builtin {
    Println = 1,
}

impl Builtin {
    pub fn call(&self, args: Vec<Value>) -> BuiltinResult {
        match &self {
            Builtin::Println => builtin_println(args),
        }
    }

    pub fn to_name(&self) -> String {
        match &self {
            Builtin::Println => String::from("last"),
        }
    }

    pub fn builtins() -> Vec<Builtin> {
        vec![Builtin::Println]
    }
}

impl From<u8> for Builtin {
    fn from(f: u8) -> Self {
        match f {
            0x1 => Builtin::Println,
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
