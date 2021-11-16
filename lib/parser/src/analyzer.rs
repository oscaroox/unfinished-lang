use crate::ParserError;
use ast::{Expression, Program};
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
    function_scopes: Vec<FunctionScope>,
    loop_scopes: Vec<LoopScope>,
}

#[derive(Debug, Error, Clone, PartialEq)]
pub enum AnalyzerError {
    #[error("{0}")]
    Error(String),
}

type AnalyzerResult = Result<(), AnalyzerError>;

impl Analyzer {
    pub fn new() -> Analyzer {
        Analyzer {
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
        errors
    }

    fn expression(&mut self, expression: &Expression) -> AnalyzerResult {
        match expression {
            Expression::BinOp(_) => todo!(),
            Expression::Literal(_) => todo!(),
            Expression::Assign(_) => todo!(),
            Expression::Index(_) => todo!(),
            Expression::SetIndex(_) => todo!(),
            Expression::GetProperty(_) => todo!(),
            Expression::SetProperty(_) => todo!(),
            Expression::Let(_) => todo!(),
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
            Expression::DataClass(_) => todo!(),
            Expression::DataClassInstance(_) => todo!(),
            Expression::Block(expr) => {
                for e in &expr.exprs {
                    self.expression(e)?
                }
            }
            Expression::If(_) => todo!(),
            Expression::ImplicitReturn(_) => todo!(),
            Expression::Return(expr) => {
                if !self.is_in_function() || !self.is_in_method() {
                    return Err(AnalyzerError::Error(
                        "Cannot use return in top-level code".to_string(),
                    ));
                }
                if let Some(e) = &*expr.value {
                    self.expression(e)?
                }
            }
            Expression::SelfExpr(_) => todo!(),
            Expression::LoopExpr(_) => todo!(),
            Expression::BreakExpr(_) => todo!(),
            Expression::ContinueExpr(_) => todo!(),
        }
        Ok(())
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
    use ast::Program;
    use scanner::Scanner;
    use spanner::SpanManager;

    use crate::{Analyzer, AnalyzerError, Parser, ParserError};

    fn analyze(source: &str, expected_error: AnalyzerError) {
        let mut manager = SpanManager::default();
        let mut maker = manager.add_source(source.to_string());

        let scanner = Scanner::new(source.to_string(), &mut maker);
        let mut parser = Parser::new(scanner);
        let (program, parser_errors) = parser.parse();
        if !parser_errors.is_empty() {
            println!("errors {:#?}", parser_errors);
            panic!("Expected parser errors to be empty");
        };

        let mut analyzer = Analyzer::new();
        let analyzer_errors = analyzer.analyze(&program);

        if analyzer_errors.is_empty() {
            panic!("analyzer did not emit errors");
        };

        if analyzer_errors.len() > 1 {
            panic!("analyzer has more than one error");
        };

        assert_eq!(analyzer_errors[0], expected_error);
    }

    #[test]
    fn analyze_return_expr() {
        analyze(
            "return;",
            AnalyzerError::Error("Cannot use return in top-level code".to_string()),
        )
    }
}
