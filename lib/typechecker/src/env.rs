use crate::Type;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug)]
pub struct Env {
    parent: Option<Rc<RefCell<Env>>>,
    store: HashMap<String, Type>,
}

impl Env {
    pub fn new() -> Env {
        Env {
            parent: None,
            store: HashMap::new(),
        }
    }

    pub fn with_parent(env: Rc<RefCell<Env>>) -> Env {
        Env {
            parent: Some(env),
            store: HashMap::new(),
        }
    }

    fn get_store(&mut self) -> &mut HashMap<String, Type> {
        &mut self.store
    }

    pub fn set(&mut self, name: &str, value: Type) {
        let store = self.get_store();
        store.insert(name.into(), value);
    }

    pub fn get(&mut self, name: &str) -> Option<Type> {
        match self.get_store().get(name) {
            Some(ttype) => Some(ttype.clone()),
            None => match &self.parent {
                Some(env) => env.borrow_mut().get(name),
                None => None,
            },
        }
    }
}
