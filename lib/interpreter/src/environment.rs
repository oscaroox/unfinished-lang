use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::Value;

#[derive(Debug, Clone, PartialEq)]
pub struct Environment {
    outer: Option<Rc<RefCell<Environment>>>,
    values: HashMap<String, Option<Value>>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            outer: None,
            values: HashMap::new(),
        }
    }

    pub fn with_outer(outer: Rc<RefCell<Environment>>) -> Environment {
        Environment {
            outer: Some(outer),
            values: HashMap::new(),
        }
    }

    pub fn extend(&mut self, vals: HashMap<String, Value>) {
        for (k, v) in vals.iter() {
            self.values.insert(k.to_string(), Some(v.clone()));
        }
    }

    pub fn define(&mut self, name: String, value: Value) {
        self.values.insert(name, Some(value));
    }

    pub fn define_none(&mut self, name: String) {
        self.values.insert(name, None);
    }

    pub fn assign(&mut self, name: String, value: Value) -> Option<Value> {
        match self.values.get(&name) {
            Some(_) => {
                self.values.insert(name.to_string(), Some(value.clone()));
                Some(value)
            }
            None => match &self.outer {
                Some(env) => env.borrow_mut().assign(name, value),
                None => None,
            },
        }
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        match self.values.get(name) {
            Some(v) => v.clone(),
            None => match &self.outer {
                Some(env) => return env.borrow().get(&name),
                None => None,
            },
        }
    }
}
