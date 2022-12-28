use parser::ast;
use parser::visit::*;

use crate::errors::PassesError;


#[derive(Debug, PartialEq)]
enum Scope {
    TopLevel,
    Loop,
    Function,
    Method,
}

pub struct SemanticAnalyzer {
    errors: Vec<PassesError>,
    scopes: Vec<Scope>,
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        SemanticAnalyzer { 
            errors: vec![], 
            scopes: vec![Scope::TopLevel],
        }
    }

    pub fn run(&mut self, exprs: &mut ast::Program) {
        for e in exprs {
            e.accept(self)
        }
    }

    fn add_error(&mut self, error: PassesError) {
        self.errors.push(error)
    }

    pub fn errors(&self) -> Vec<PassesError> {
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
        walk_function(self, e);
        self.scopes.pop();
    }

    fn visit_loop(&mut self, e: &ast::LoopExpr) {
        self.scopes.push(Scope::Loop);
        walk_loop(self, e);
        self.scopes.pop();
    }

    fn visit_break(&mut self, e: &ast::BreakExpr) {
        if !self.is_in_loop_scope() {
            self.add_error(PassesError::NoBreakOutsideLoop(e.span.clone()))
        }
    }

    fn visit_continue(&mut self, e: &ast::ContinueExpr) {
        if !self.is_in_loop_scope() {
            self.add_error(PassesError::NoContinueOutsideLoop(e.span.clone()))
        }
    }

    fn visit_return(&mut self, e: &ast::ReturnExpr) {
        if !self.is_in_function_scope(vec![Scope::Function, Scope::Method]) {
            self.add_error(PassesError::NoTopLevelReturn(e.span.clone()));
        }
        walk_return(self, e);
    }
}


#[cfg(test)]
mod analyzer_test {
    use span_util::Span;

    use crate::errors::PassesError;

    use super::SemanticAnalyzer;

    fn analyze(src: &str, expected: Vec<PassesError>) -> SemanticAnalyzer {
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
            PassesError::NoTopLevelReturn(Span::fake())
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
            PassesError::NoContinueOutsideLoop(Span::fake()),
            PassesError::NoContinueOutsideLoop(Span::fake()),
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
        PassesError::NoBreakOutsideLoop(Span::fake()),
        PassesError::NoBreakOutsideLoop(Span::fake()),
    ]);    
    }
}