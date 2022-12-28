use parser::ast;
use parser::visit::*;

use crate::symbol_table::SymbolTable;


#[derive(Debug, PartialEq)]
enum Scope {
    TopLevel,
    Loop,
    Function,
    Method,
}

pub struct SemanticAnalyzer {
    errors: Vec<String>,
    scopes: Vec<Scope>,
    symbol_table: SymbolTable,
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        let mut symbol_table = SymbolTable::new();
        symbol_table.add_scope(vec![]); // add toplevel scope

        SemanticAnalyzer { 
            errors: vec![], 
            scopes: vec![Scope::TopLevel],
            symbol_table,
        }
    }

    pub fn run(&mut self, exprs: &mut ast::Program) {
        for e in exprs {
            e.accept(self)
        }
    }

    fn add_error(&mut self, error: &str) {
        self.errors.push(error.into())
    }

    pub fn errors(&self) -> Vec<String> {
        self.errors.clone()
    } 

    pub fn symbol_table(&self) -> SymbolTable {
        self.symbol_table.clone()
    }

    fn is_in_function_scope(&self, scope: Vec<Scope>) -> bool {
        for s in self.scopes.iter().rev() {
            if scope.contains(&s) {
                return true
            }
        }
        false
    }

    fn is_in_loop_scope(&self) -> bool {
        match self.scopes.len() {
            0 => false,
            n => self.scopes[n - 1] == Scope::Loop
        }
    }
}

impl Visitor for SemanticAnalyzer {
    fn visit_function(&mut self, e: &ast::Function) {
        self.scopes.push(Scope::Function);
        let symbols: Vec<String> = e.params.iter()
            .map(|i|  i.value.to_string())
            .collect();
        self.symbol_table.add_scope(symbols);
        walk_function(self, e);
        self.symbol_table.pop_scope();
        self.scopes.pop();
    }

    fn visit_block(&mut self, e: &ast::Block) {
        self.symbol_table.add_scope(vec![]);
        walk_block(self, e);
        self.symbol_table.pop_scope();        
    }

    fn visit_let(&mut self, e: &ast::LetExpr) {
       self.symbol_table.define(e.name.value.to_string());
       walk_let(self, e);
    }

    fn visit_loop(&mut self, e: &ast::LoopExpr) {
        self.scopes.push(Scope::Loop);
        walk_loop(self, e);
        self.scopes.pop();
    }

    fn visit_break(&mut self, _e: &ast::BreakExpr) {
        if !self.is_in_loop_scope() {
            self.add_error("Cannot use break and continue outside loop expression")
        }
    }

    fn visit_continue(&mut self, _e: &ast::ContinueExpr) {
        if !self.is_in_loop_scope() {
            self.add_error("Cannot use break and continue outside loop expression")
        }
    }

    fn visit_return(&mut self, e: &ast::ReturnExpr) {
        if !self.is_in_function_scope(vec![Scope::Function, Scope::Method]) {
            self.add_error("Cannot use return in top level");
        }
        walk_return(self, e);
    }
}


#[cfg(test)]
mod analyzer_test {
    use super::SemanticAnalyzer;

    fn analyze(src: &str, expected: Vec<&str>) -> SemanticAnalyzer {
        let mut ast = parser::parse_panic(src);
        let mut analyzer = SemanticAnalyzer::new();
        analyzer.run(&mut ast);

        let errors = analyzer.errors();

        if errors.len() != expected.len() {
            println!("{:#?}", errors);
            panic!(
                "analyzer emitted {} errors, expected errors has {} errors",
                errors.len(),
                expected.len()
            );
        }

        for (expected_error, error) in errors.iter().zip(expected) {
            assert_eq!(error, *expected_error)
        }

        analyzer
    }

    #[test]
    fn check_return() {
        analyze("
        return
        let x = fn {
            return
        }
        ", vec![
            "Cannot use return in top level"
        ]);
    }

    #[test]
    fn check_continue() {
        analyze("
            continue
            loop {
                continue
                fn {
                    continue
                }
            }
            
        ", vec![
            "Cannot use break and continue outside loop expression",
            "Cannot use break and continue outside loop expression"
        ]);
    }

    #[test]
    fn check_break() {
        analyze("
        break
        loop {
            continue
            fn {
                break
            }
        }
        
    ", vec![
        "Cannot use break and continue outside loop expression",
        "Cannot use break and continue outside loop expression"
    ]);    
    }

    #[test]
    fn check_name_resolution() {
        let analyzer = analyze("
        let x = 1
        let main = fn {
            let y = 2
        }
        ", vec![]);

        println!("{:#?}", analyzer.symbol_table());
    }
}