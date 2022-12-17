use parser::ast::{self, Expression, Program};

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
}


impl SemanticAnalyzer {

    pub fn new() -> Self {
        SemanticAnalyzer { 
            errors: vec![],
            scopes: vec![],
        }
    }

    pub fn run(&mut self, ast: &Program) {
        for node in ast {
            self.analyze(node)
        }
    }
    
    fn add_error(&mut self, error: &str) {
        self.errors.push(error.into())
    }

    pub fn errors(&self) -> Vec<String> {
        self.errors.clone()
    } 

    fn analyze(&mut self, expr: &Expression) {
        match expr {
            Expression::Block(expr) => {
                self.run(&expr.exprs)
            }
            Expression::If(expr) => {
                self.analyze(&expr.condition);
                self.analyze(&expr.then);
                if let Some(e) = &expr.not_then {
                    self.analyze(e);
                }
            }
            Expression::Function(expr) => {
                self.scopes.push(Scope::Function);
                self.analyze(&expr.body);
                self.scopes.pop();
            }
            Expression::Let(expr) => {
                if let Some(e) = &expr.value {
                    self.analyze(e);
                }
            }
            Expression::Grouping(expr) => {
                self.analyze(&expr.expr);
            }
            Expression::Return(expr) => {
                if let Some(e) = &*expr.value {
                    self.analyze(e);
                }

                if !self.is_in_function_scope(vec![Scope::Function, Scope::Method]) {
                    self.add_error("Cannot use return in top level")
                } 
            }
            Expression::ContinueExpr(_) |
            Expression::BreakExpr(_) => {
                if !self.is_in_loop_scope(vec![Scope::Loop]) {
                    self.add_error("Cannot use break and continue outside loop expression");
                }
            }
            Expression::LoopExpr(expr) => {
                self.analyze(&expr.condition);
                if let Some(e) = &expr.iterator {
                    self.analyze(e);
                }
                self.scopes.push(Scope::Loop);
                self.analyze(&expr.body);
                self.scopes.pop();
            }
            _ => {}
        }
    }

    fn is_in_function_scope(&self, scope: Vec<Scope>) -> bool {
        for s in self.scopes.iter().rev() {
            if scope.contains(&s) {
                return true
            }
        }
        false
    }

    fn is_in_loop_scope(&self, scope: Vec<Scope>) -> bool {
        match self.scopes.len() {
            0 => false,
            n => {
                scope.contains(&self.scopes[n - 1])
            }
        }
    }
}

#[cfg(test)]
mod analyzer_test {
    use super::SemanticAnalyzer;


    fn run(src: &str) -> SemanticAnalyzer {
        let ast = parser::parse_panic(src);
        let mut analyzer = SemanticAnalyzer::new();
        analyzer.run(&ast);

        analyzer
    }

    fn analyze(src: &str, expected: Vec<&str>) {
        let analyzer = run(src);
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