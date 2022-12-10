mod passes;
mod parser;
mod parser_error;
pub mod ast;

// TODO make scanner private, only allow parser
pub mod scanner;

pub use crate::parser::*;
use ast::Program;
pub use passes::*;
pub use parser_error::*;
pub use analyzer::Analyzer;
use passes::analyzer::AnalyzerError;


pub enum Error {
    ParserError(Vec<ParserError>),
    AnalyzerError(Vec<AnalyzerError>),
}

/**
 * Parse the input string and returns a tuple with errors and AST
 * AST might be incomplete if there are errors
 */
pub fn parse(source: &str) -> (Vec<Error>, Program) {
    let mut errors = vec![];

    let scanner = scanner::Scanner::new(source.to_string());
    let mut parser = parser::Parser::new(scanner);
    let mut analyzer = Analyzer::new();

    let (ast, parser_errors) = parser.parse();

    let analyzer_errors = analyzer.analyze(&ast);

    if !parser_errors.is_empty() {
        errors.push(Error::ParserError(parser_errors));
    }

    if !analyzer_errors.is_empty() {
        errors.push(Error::AnalyzerError(analyzer_errors));
    }

    (errors, ast)
}

/**
 * Parse the input and returns a complete AST
 * Function will panic on error
 */
pub fn parse_panic(source: &str) -> Program {
    let (errors, ast) = parse(source);
    
    if errors.is_empty() {
        return ast
    }

    println!("Errors found while parsing the source into AST, found {} errors", errors.len());

    for error in errors {
        match error {
            Error::ParserError(e) => println!("{e:#?}"),
            Error::AnalyzerError(e) => println!("{e:#?}"),
        }
    };

    panic!("parsed with errors");
}
