use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{RuntimeError, Value};

type BuiltinResult = Result<Value, RuntimeError>;

#[derive(Debug, Clone, PartialEq)]
pub enum Builtin {
    Println = 1,
    Len = 2,
    Clone = 3,
    Range = 4,
}

impl Builtin {
    pub fn call(&self, args: Vec<Value>) -> BuiltinResult {
        match &self {
            Builtin::Println => builtin_println(args),
            Builtin::Len => builtin_len(args),
            Builtin::Clone => builtin_clone(args),
            Builtin::Range => builtin_range(args),
        }
    }

    pub fn to_name(&self) -> String {
        match &self {
            Builtin::Println => String::from("last"),
            Builtin::Len => String::from("len"),
            Builtin::Clone => String::from("clone"),
            Builtin::Range => String::from("range"),
        }
    }

    pub fn builtins() -> Vec<Builtin> {
        vec![
            Builtin::Println,
            Builtin::Len,
            Builtin::Clone,
            Builtin::Range,
        ]
    }
}

impl From<u8> for Builtin {
    fn from(f: u8) -> Self {
        match f {
            0x1 => Builtin::Println,
            0x2 => Builtin::Len,
            0x3 => Builtin::Clone,
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

    map.insert(
        String::from("clone"),
        Value::native_function("clone", 1, Builtin::Clone),
    );

    map.insert(
        Builtin::Range.to_name(),
        Value::native_function(Builtin::Range.to_name(), 2, Builtin::Range),
    );

    map
}

/**
 * Prints the value to the console
 */
fn builtin_println(args: Vec<Value>) -> BuiltinResult {
    match &args[0] {
        obj => {
            match &obj {
                Value::String(v) => println!("{}", v),
                _ => println!("{}", obj),
            }
            Ok(Value::Null)
        }
    }
}

/**
 * Returns the length of an Array or String value
 */
fn builtin_len(args: Vec<Value>) -> BuiltinResult {
    match &args[0] {
        Value::Array(arr) => Ok(Value::Int(arr.borrow().values.len() as i64)),
        Value::String(s1) => Ok(Value::Int(s1.len() as i64)),
        _ => {
            panic!("invalid use of len on {}", &args[0])
        }
    }
}

/**
 * Clones a Data struct instance or Array
 * Data structs and Arrays are references e.g
 *
 * let arr1 = [];
 * let arr2 = arr1;
 *
 * arr2 points the arr1.
 * using the builtin "clone" function creates a new value
 *
 * let arr1 = [];
 * let arr2 = clone(arr1); // creates a new value from arr1
 *
 * Mutations made on arr2 wont affect arr1, mutations on arr1 will also not affect arr2
 */
fn builtin_clone(args: Vec<Value>) -> BuiltinResult {
    match &args[0] {
        Value::DataClassInstance(instance) => {
            let i = instance.borrow();
            Ok(Value::DataClassInstance(Rc::new(RefCell::new(i.clone()))))
        }
        Value::Array(arr) => {
            let a = arr.borrow();
            Ok(Value::Array(Rc::new(RefCell::new(a.clone()))))
        }
        _ => panic!("can only clone data structs"),
    }
}

fn builtin_range(args: Vec<Value>) -> BuiltinResult {
    let start = &args[0];
    let end = &args[1];

    match (start, end) {
        (Value::Int(start), Value::Int(end)) => Ok(Value::Range(*start, *end)),
        _ => panic!("Expected int got {} and {}", start, end),
    }
}
