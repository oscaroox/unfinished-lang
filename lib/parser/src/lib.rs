mod scanner;
mod parser;
mod parser_error;


#[cfg(feature = "test-utils")]
pub mod test_utils;

pub use crate::parser::*;

use ast::Program;
pub use parser_error::*;


/**
 * Parse the input string and returns a tuple with errors and AST
 * The errors consist of parser and semantic errors.
 * AST may be incomplete when there are parser errors
 */
pub fn parse(source: &str) -> (Program, Vec<ParserError>) {

    let scanner = scanner::Scanner::new(source.to_string());
    let mut parser = parser::Parser::new(scanner);
    parser.parse()
}

/**
 * Parse the input and returns a complete AST
 * Function will panic if there are any parser and semantic errors
 */
pub fn parse_panic(source: &str) -> Program {
    let (ast, errors) = parse(source);
    
    if errors.is_empty() {
        return ast
    }

    println!("Errors found while parsing the source into AST, found {} errors", errors.len());

    for e in errors {
        println!("{e:#?}");
    };

    panic!("parsed with errors");
}
