use ast::{BinOp, Expression, Literal, Program, Statement};
use scanner::{Scanner, Token, TokenType};

#[derive(Debug)]
pub struct Parser {
    scanner: Scanner,
    prev_token: Token,
    curr_token: Token,
    peek_token: Token,
}

impl Parser {
    pub fn new(mut scanner: Scanner) -> Parser {
        let curr_token = scanner.next_token();
        let peek_token = scanner.next_token();
        Parser {
            scanner,
            prev_token: Token::eof((0, 0)),
            curr_token,
            peek_token,
        }
    }

    pub fn parse(&mut self) -> Program {
        let mut program = Vec::new();
        while !self.is_end() {
            program.push(self.statement())
        }
        program
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

    fn eat(&mut self, token_type: TokenType, msg: &str) -> Token {
        if self.curr_token.token_type == token_type {
            return self.advance();
        }
        panic!("{}", msg.to_string())
    }

    fn statement(&mut self) -> Statement {
        if self.matches(vec![TokenType::Let]) {
            return self.let_statement();
        };
        return self.expression_statement();
    }

    fn let_statement(&mut self) -> Statement {
        let identifier = self.eat(TokenType::Identifier, "Expected identifier");

        let mut init = None;
        if self.matches(vec![TokenType::Assign]) {
            init = Some(self.expression());
        }
        self.eat(TokenType::SemiColon, "Expected ';' after let statement");
        Statement::create_let(identifier.value, init)
    }

    fn expression_statement(&mut self) -> Statement {
        let expr = self.expression();
        self.eat(TokenType::SemiColon, "Expected ';' after expression");
        Statement::create_expression(expr)
    }

    fn expression(&mut self) -> Expression {
        self.primary()
    }

    fn primary(&mut self) -> Expression {
        if self.matches(vec![TokenType::IntConst]) {
            let token = self.prev_token.clone();
            let val: u64 = match token.value.parse() {
                Ok(v) => v,
                _ => panic!("not a integer"),
            };
            return Expression::create_literal(Literal::Int(val));
        }

        panic!("wrong token")
    }
}

#[cfg(test)]
mod test {

    use super::Parser;
    use ast::{Expression, Literal, Program, Statement};
    use scanner::Scanner;

    fn parse(source: &str, expected: Program) {
        let scanner = Scanner::new(source.to_string());
        let mut parser = Parser::new(scanner);
        let res = parser.parse();
        assert_eq!(res, expected);
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
        )
    }
}
