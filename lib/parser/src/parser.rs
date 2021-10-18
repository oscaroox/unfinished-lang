use ast::{Expression, Literal, Program, Statement};
use scanner::{Scanner, Token, TokenType};
use spanner::Span;

use crate::ParserError;

#[derive(Debug)]
pub struct Parser<'a> {
    scanner: Scanner<'a>,
    prev_token: Token,
    curr_token: Token,
    peek_token: Token,
}

impl<'a> Parser<'a> {
    pub fn new(mut scanner: Scanner) -> Parser {
        let curr_token = scanner.next_token();
        let peek_token = scanner.next_token();
        Parser {
            scanner,
            prev_token: Token::eof(Span::dummy()),
            curr_token,
            peek_token,
        }
    }

    pub fn parse(&mut self) -> (Program, Vec<ParserError>) {
        let mut program = Vec::new();
        let mut errors = Vec::new();
        while !self.is_end() {
            match self.statement() {
                Ok(stmt) => program.push(stmt),
                Err(err) => {
                    self.sync();
                    errors.push(err);
                }
            }
        }
        (program, errors)
    }

    fn sync(&mut self) {
        self.advance();

        while !self.is_end() {
            if self.prev_token.token_type == TokenType::SemiColon {
                return;
            }

            match self.curr_token.token_type {
                TokenType::Let | TokenType::Fun => return,
                _ => {}
            }
            self.advance();
        }
    }

    fn matches(&mut self, types: Vec<TokenType>) -> bool {
        for token_type in types.iter() {
            if *token_type == self.curr_token.token_type {
                self.advance();
                return true;
            }
        }
        false
    }

    fn advance(&mut self) -> Token {
        if !self.is_end() {
            self.prev_token = self.curr_token.clone();
            self.curr_token = self.peek_token.clone();
            self.peek_token = self.scanner.next_token();
        }

        self.prev_token.clone()
    }

    fn is_end(&self) -> bool {
        self.peek_token.is_eof()
    }

    fn eat(&mut self, token_type: TokenType, msg: &str) -> Result<Token, ParserError> {
        if self.curr_token.token_type == token_type {
            return Ok(self.advance());
        }
        Err(ParserError::ExpectedToken(
            msg.to_string(),
            self.curr_token.clone(),
        ))
    }

    fn statement(&mut self) -> Result<Statement, ParserError> {
        if self.matches(vec![TokenType::Let]) {
            return self.let_statement();
        };
        return self.expression_statement();
    }

    fn let_statement(&mut self) -> Result<Statement, ParserError> {
        let identifier = self.eat(TokenType::Identifier, "Expected identifier")?;

        let mut init = None;
        if self.matches(vec![TokenType::Assign]) {
            let expr = match self.expression() {
                Ok(expr) => expr,
                Err(err) => {
                    if self.curr_token.is_assign() && self.peek_token.is_eof()
                        || self.curr_token.is_semi_colon()
                    {
                        return Err(ParserError::ExpectedExpression(self.curr_token.clone()));
                    }
                    return Err(err);
                }
            };
            init = Some(expr);
        }
        self.eat(TokenType::SemiColon, "Expected ';' after let statement")?;
        Ok(Statement::create_let(identifier.value, init))
    }

    fn expression_statement(&mut self) -> Result<Statement, ParserError> {
        let expr = self.expression()?;
        self.eat(TokenType::SemiColon, "Expected ';' after expression")?;
        Ok(Statement::create_expression(expr))
    }

    fn expression(&mut self) -> Result<Expression, ParserError> {
        self.primary()
    }

    fn primary(&mut self) -> Result<Expression, ParserError> {
        if self.matches(vec![TokenType::IntConst]) {
            let token = self.prev_token.clone();
            let val: u64 = match token.value.parse() {
                Ok(v) => v,
                _ => panic!("not a integer"),
            };
            return Ok(Expression::create_literal(Literal::Int(val)));
        }

        Err(ParserError::UnexpectedToken(self.curr_token.clone()))
    }
}

#[cfg(test)]
mod test {

    use crate::ParserError;

    use super::Parser;
    use ast::{Expression, Literal, Program, Statement};
    use scanner::Scanner;
    use spanner::SpanManager;

    fn run_parser(source: &str) -> (Program, Vec<ParserError>) {
        let mut manager = SpanManager::default();
        let mut maker = manager.add_source(source.to_string());

        let scanner = Scanner::new(source.to_string(), &mut maker);
        let mut parser = Parser::new(scanner);
        parser.parse()
    }

    fn parse(source: &str, expected: Program) {
        let (res, _) = run_parser(source);
        assert_eq!(res, expected);
    }

    fn parse_error(source: &str, expected: Vec<ParserError>) {
        let (res, errors) = run_parser(source);

        let errs: Vec<String> = errors.clone().into_iter().map(|e| e.to_string()).collect();
        println!("{:?}", errs);
        println!("{:?}", res);
        assert_eq!(errors, expected)
    }

    #[test]
    fn let_stmt() {
        parse(
            "let num; let _num = 1;",
            vec![
                Statement::create_let(String::from("num"), None),
                Statement::create_let(
                    String::from("_num"),
                    Some(Expression::create_literal(Literal::Int(1))),
                ),
            ],
        );
        // parse_error(
        //     "let = 1;",
        //     vec![
        //         ParserError::ExpectedToken("Expected identifier".to_string()),
        //         ParserError::ExpectedToken("Expected ';' after let statement".to_string()),
        //     ],
        // );
    }
}
