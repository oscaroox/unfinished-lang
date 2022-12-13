use crate::errors::AnalyzerError;
use crate::ast::{Program, Expression};

#[derive(Debug, PartialEq)]
enum FunctionScope {
    Function,
    Method,
    TopLevel,
}

#[derive(Debug, PartialEq)]
enum LoopScope {
    TopLevel,
    Loop,
}

pub struct SemanticAnalyzer {
    errors: Vec<AnalyzerError>,
    function_scopes: Vec<FunctionScope>,
    loop_scopes: Vec<LoopScope>,
}

type AnalyzerResult = Result<(), AnalyzerError>;

impl SemanticAnalyzer {
    pub fn new() -> SemanticAnalyzer {
        SemanticAnalyzer {
            errors: vec![],
            function_scopes: vec![FunctionScope::TopLevel],
            loop_scopes: vec![LoopScope::TopLevel],
        }
    }

    pub fn analyze(&mut self, program: &Program) -> Vec<AnalyzerError> {
        let mut errors = vec![];
        for expr in program {
            match self.expression(expr) {
                Err(err) => errors.push(err),
                _ => {}
            }
        }
        self.errors.clone()
    }

    fn expression(&mut self, expression: &Expression) -> AnalyzerResult {
        match expression {
            Expression::Let(expr) => {
                if let Some(e) = &expr.value {
                    self.expression(e)?;
                }
            }
            Expression::Function(expr) => {
                self.function_scopes.push(FunctionScope::Function);
                self.expression(&expr.body)?;
                self.function_scopes.pop();
            }
            Expression::DataStruct(expr) => {
                for method in &expr.methods {
                    self.function_scopes.push(FunctionScope::Method);
                    self.expression(method)?;
                    self.function_scopes.pop();
                }
            }
            Expression::Block(expr) => {
                for e in &expr.exprs {
                    self.expression(e)?
                }
            }
            Expression::Return(expr) => {
                if !self.is_in_function() {
                    if !self.is_in_method() {
                        self.error(AnalyzerError::NoTopLevelReturn(expr.span.clone()));
                    }
                }
                if let Some(e) = &*expr.value {
                    self.expression(e)?
                }
            }
            Expression::SelfExpr(expr) => {
                if !self.is_in_method() {
                    self.error(AnalyzerError::NoSelfOutsideMethod(expr.span.clone()));
                }
            }
            Expression::LoopExpr(expr) => {
                self.loop_scopes.push(LoopScope::Loop);
                self.expression(&expr.condition)?;
                self.expression(&expr.body)?;
                self.loop_scopes.pop();
            }
            Expression::Call(expr) => {
                let is_named_call = match &expr.arguments.get(0) {
                    Some(arg) => arg.is_named(),
                    None => false,
                };

                for arg in &expr.arguments {
                    if is_named_call && !arg.is_named() || !is_named_call && arg.is_named() {
                        self.error(AnalyzerError::NoUsePositionalWithNamedArgs(arg.1.get_span()))
                    }
                }
            }
            Expression::BreakExpr(expr) => {
                if !self.is_in_loop() {
                    self.error(AnalyzerError::NoBreakOutsideLoop(expr.span.clone()))
                }
            }
            Expression::ContinueExpr(expr) => {
                if !self.is_in_loop() {
                    self.error(AnalyzerError::NoContinueOutsideLoop(expr.span.clone()))
                }
            }
            Expression::GetProperty(expr) => {
                self.expression(&*expr.object)?;
            }
            Expression::If(expr) => {
                self.expression(&expr.condition)?;
                self.expression(&expr.then)?;
                if let Some(e) = &expr.not_then {
                    self.expression(e)?;
                }
            }
            _ => {}
        }
        Ok(())
    }

    fn error(&mut self, error: AnalyzerError) {
        self.errors.push(error);
    }

    fn is_in_function(&self) -> bool {
        self.function_scopes.contains(&FunctionScope::Function)
    }

    fn is_in_loop(&self) -> bool {
        self.loop_scopes.contains(&LoopScope::Loop)
    }

    fn is_in_method(&self) -> bool {
        self.function_scopes.contains(&FunctionScope::Method)
    }
}

#[cfg(test)]
mod tests {
    use crate::{scanner::Scanner, Parser};
    use span_util::Span;

    use super::{SemanticAnalyzer, AnalyzerError};
    

    fn analyze(source: &str) -> Vec<AnalyzerError> {
        let scanner = Scanner::new(source.to_string());
        let mut parser = Parser::new(scanner);
        let (program, parser_errors) = parser.parse();
        if !parser_errors.is_empty() {
            panic!(
                "Expected parser errors to be empty got {:#?}",
                parser_errors
            );
        };

        let mut analyzer = SemanticAnalyzer::new();
        analyzer.analyze(&program)
    }

    fn analyze_success(source: &str) {
        let errors = analyze(source);

        if errors.len() > 0 {
            panic!("analyzer emitted errors: {:#?}", errors);
        }
    }

    fn analyze_error(source: &str, expected_errors: Vec<AnalyzerError>) {
        let analyzer_errors = analyze(source);

        if analyzer_errors.is_empty() {
            panic!("analyzer did not emit errors");
        };

        if analyzer_errors.len() != expected_errors.len() {
            println!("{:#?}", analyzer_errors);
            panic!(
                "analyzer emitted {} errors, expected errors has {} errors",
                analyzer_errors.len(),
                expected_errors.len()
            );
        }
        for (err, expected) in analyzer_errors.iter().zip(expected_errors) {
            assert_eq!(*err, expected);
        }
    }

    #[test]
    fn analyze_return_expr() {
        analyze_error(
            "return;",
            vec![AnalyzerError::NoTopLevelReturn(Span::fake())],
        );
        analyze_error(
            "return; return;",
            vec![
                AnalyzerError::NoTopLevelReturn(Span::fake()),
                AnalyzerError::NoTopLevelReturn(Span::fake()),
            ],
        );
        analyze_error(
            "{return;};",
            vec![AnalyzerError::NoTopLevelReturn(Span::fake())],
        );
        analyze_success(
            "
        fn() {
            return;
        };
        ",
        );

        analyze_success(
            "
            data Person() {
                fn new {
                    return;
                }

                fn test(self) {
                    return;
                }
                fn nested() {
                    let main = fn() {
                        return;
                    };
                }
            };
        ",
        );
    }

    #[test]
    fn analyze_continue_break_expr() {
        analyze_error(
            "break;",
            vec![AnalyzerError::NoBreakOutsideLoop(Span::fake())],
        );
        analyze_error(
            "break;continue;",
            vec![
                AnalyzerError::NoBreakOutsideLoop(Span::fake()),
                AnalyzerError::NoContinueOutsideLoop(Span::fake()),
            ],
        );
        analyze_error(
            "continue;",
            vec![AnalyzerError::NoContinueOutsideLoop(Span::fake())],
        );
        analyze_error(
            "continue;break;",
            vec![
                AnalyzerError::NoContinueOutsideLoop(Span::fake()),
                AnalyzerError::NoBreakOutsideLoop(Span::fake()),
            ],
        );

        analyze_success(
            "
            loop {
                break;
            };
            loop {
                continue;
            };
        ",
        );
    }

    #[test]
    fn analyze_self_expr() {
        analyze_error(
            "self;",
            vec![AnalyzerError::NoSelfOutsideMethod(Span::fake())],
        );
        analyze_error(
            "
        self;
        let main = fn() {
            self;
        };
        ",
            vec![
                AnalyzerError::NoSelfOutsideMethod(Span::fake()),
                AnalyzerError::NoSelfOutsideMethod(Span::fake()),
            ],
        );

        analyze_success(
            "
        data Person() {
            fn new {
                self;
            }

            fn test(self) {
                self;
                let main = fn() {
                    self;
                };
            }
        };
        ",
        );
    }

    #[test]
    fn analyze_call_expr() {
        analyze_error("
            run(commit=true, 1);
            run(1, commit=true);
            run(1,3,4);
            run(commit = true, id =1);
            run(1, commit=true);
        ", vec![
            AnalyzerError::NoUsePositionalWithNamedArgs(Span::fake()),
            AnalyzerError::NoUsePositionalWithNamedArgs(Span::fake()),
            AnalyzerError::NoUsePositionalWithNamedArgs(Span::fake()),
        ])
    }
}