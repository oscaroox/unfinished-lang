use ariadne::{Label, Report, ReportKind};
use ast::{Expression, Program};
use span_util::Span;
use thiserror::Error;

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

pub struct Analyzer {
    errors: Vec<AnalyzerError>,
    function_scopes: Vec<FunctionScope>,
    loop_scopes: Vec<LoopScope>,
}

#[derive(Debug, Error, Clone, PartialEq)]
pub enum AnalyzerError {
    #[error("{0}")]
    Error(String),
    #[error("Cannot use return in top-level code")]
    NoTopLevelReturn(Span),
    #[error("Cannot use break outside loop expression")]
    NoBreakOutsideLoop,
    #[error("Cannot use continue outside loop expression")]
    NoContinueOutsideLoop,
    #[error("Cannot use self outside methods")]
    NoSelfOutsideMethod,
}

impl AnalyzerError {
    pub fn into_report(&mut self) -> Report {
        let msg = self.to_string();

        match self {
            Self::NoTopLevelReturn(span) => {
                let label = Label::new(span.to_range());
                Report::build(ReportKind::Error, (), 99)
                    .with_message("Parser Error")
                    .with_label(label.with_message(msg))
                    .finish()
            }
            _ => Report::build(ReportKind::Error, (), 99)
                .with_message(format!("Parser Error: {}", msg))
                .finish(),
        }
    }
}

type AnalyzerResult = Result<(), AnalyzerError>;

impl Analyzer {
    pub fn new() -> Analyzer {
        Analyzer {
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
            Expression::BinOp(_) => todo!(),
            Expression::Literal(lit) => match lit.0 {
                ast::Literal::Int(_)
                | ast::Literal::Float(_)
                | ast::Literal::Bool(_)
                | ast::Literal::String(_)
                | ast::Literal::Array(_)
                | ast::Literal::Null => {}
            },
            Expression::Assign(_) => todo!(),
            Expression::Index(_) => todo!(),
            Expression::SetIndex(_) => todo!(),
            Expression::GetProperty(_) => todo!(),
            Expression::SetProperty(_) => todo!(),
            Expression::Let(expr) => {
                if let Some(e) = &expr.0.value {
                    self.expression(e)?;
                }
            }
            Expression::LetRef(_) => todo!(),
            Expression::UnaryOp(_) => todo!(),
            Expression::Grouping(_) => todo!(),
            Expression::Logic(_) => todo!(),
            Expression::Call(_) => todo!(),
            Expression::Function(expr) => {
                self.function_scopes.push(FunctionScope::Function);
                self.expression(&expr.body)?;
                self.function_scopes.pop();
            }
            Expression::DataClass(expr) => {
                for method in &expr.methods {
                    self.function_scopes.push(FunctionScope::Method);
                    self.expression(method)?;
                    self.function_scopes.pop();
                }
            }
            Expression::DataClassInstance(_) => todo!(),
            Expression::Block(expr) => {
                for e in &expr.exprs {
                    self.expression(e)?
                }
            }
            Expression::If(_) => todo!(),
            Expression::ImplicitReturn(_) => todo!(),
            Expression::Return(expr) => {
                if !self.is_in_function() {
                    if !self.is_in_method() {
                        self.error(AnalyzerError::NoTopLevelReturn(expr.1.clone()));
                    }
                }
                if let Some(e) = &*expr.0.value {
                    self.expression(e)?
                }
            }
            Expression::SelfExpr(_) => {
                if !self.is_in_method() {
                    self.error(AnalyzerError::NoSelfOutsideMethod);
                }
            }
            Expression::LoopExpr(expr) => {
                self.loop_scopes.push(LoopScope::Loop);
                self.expression(&expr.condition)?;
                self.expression(&expr.body)?;
                self.loop_scopes.pop();
            }
            Expression::BreakExpr(_) => {
                if !self.is_in_loop() {
                    self.error(AnalyzerError::NoBreakOutsideLoop)
                }
            }
            Expression::ContinueExpr(_) => {
                if !self.is_in_loop() {
                    self.error(AnalyzerError::NoContinueOutsideLoop)
                }
            }
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
    use scanner::Scanner;
    use span_util::Span;

    use crate::{Analyzer, AnalyzerError, Parser};

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

        let mut analyzer = Analyzer::new();
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
                "analyzer emmitted {} errors, expected errors has {} errors",
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
        fun() {
            return;
        };
        ",
        );

        analyze_success(
            "
            data Person {

            } :: {
                fun new {
                    return;
                }

                fun test(self) {
                    return;
                }
                fun nested() {
                    let main = fun() {
                        return;
                    };
                }
            };
        ",
        );
    }

    #[test]
    fn analyze_continue_break_expr() {
        analyze_error("break;", vec![AnalyzerError::NoBreakOutsideLoop]);
        analyze_error(
            "break;continue;",
            vec![
                AnalyzerError::NoBreakOutsideLoop,
                AnalyzerError::NoContinueOutsideLoop,
            ],
        );
        analyze_error("continue;", vec![AnalyzerError::NoContinueOutsideLoop]);
        analyze_error(
            "continue;break;",
            vec![
                AnalyzerError::NoContinueOutsideLoop,
                AnalyzerError::NoBreakOutsideLoop,
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
        analyze_error("self;", vec![AnalyzerError::NoSelfOutsideMethod]);
        analyze_error(
            "
        self;
        let main = fun() {
            self;
        };
        ",
            vec![
                AnalyzerError::NoSelfOutsideMethod,
                AnalyzerError::NoSelfOutsideMethod,
            ],
        );

        analyze_success(
            "
        data Person {

        } :: {
            fun new {
                self;
            }

            fun test(self) {
                self;
                let main = fun() {
                    self;
                };
            }
        };
        ",
        );
    }
}
