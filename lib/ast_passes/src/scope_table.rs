use std::collections::HashSet;

pub struct ScopeTable {
    scopes: Vec<HashSet<String>>
}

impl ScopeTable {
    pub fn new() -> Self {
        Self { scopes: vec![] }
    }

    pub fn with_global_scope() -> Self {
        Self { scopes: vec![HashSet::new()] }
    }

    pub fn define(&mut self, value: String) -> bool {
        match self.scopes.last_mut() {
            Some(scope) => scope.insert(value),
            None => unreachable!(),
        }
    }

    pub fn resolve(&mut self, value: &str) -> Option<usize> {
        for (i, scope) in self.scopes.iter().rev().enumerate() {
            if scope.contains(value) {
                return Some(i)
            }
        }
        None
    }

    pub fn add_scope(&mut self) {
        self.scopes.push(HashSet::new());
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop();
    }

}

