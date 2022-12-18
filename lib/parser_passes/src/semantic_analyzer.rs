use parser::ast;
use parser::visit::{Visitor, Visitable};

#[derive(Debug, PartialEq)]
enum Scope {
    TopLevel,
    Loop,
    Function,
    Method,
}

pub struct SemanticAnalyzer {
    errors: Vec<String>,
    scopes: Vec<Scope>
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        SemanticAnalyzer { 
            errors: vec![], 
            scopes: vec![Scope::TopLevel] 
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
        self.visit_expr(&e.body);
        self.scopes.pop();
    }

    fn visit_loop(&mut self, e: &ast::LoopExpr) {
        self.scopes.push(Scope::Loop);
        self.visit_expr(&e.condition);
        self.visit_expr(&e.body);
        if let Some(i) = &e.iterator {
            self.visit_expr(i)
        }
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

        if let Some(e) = &*e.value {
            self.visit_expr(e)
        }
    }
}


#[cfg(test)]
mod analyzer_test {
    use super::SemanticAnalyzer;

    fn analyze(src: &str, expected: Vec<&str>) {
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
        ])
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
        ])
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
    ])    
    }

}