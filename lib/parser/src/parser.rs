use ast::{self, BinOp, Expression, Program, Statement};
use scanner::{Scanner, Token, TokenType};

pub struct Parser {
    scanner: Scanner,
    curr_token: Token,
    peek_token: Token,
}

impl Parser {
    pub fn new(mut scanner: Scanner) -> Parser {
        let curr_token = scanner.next_token();
        let peek_token = scanner.next_token();
        Parser {
            scanner,
            curr_token,
            peek_token,
        }
    }

    pub fn parse(&mut self) -> Program {
        let mut program = Vec::new();
        while !self.curr_token.is_eof() {
            program.push(self.statement())
        }
        program
    }

    fn matches(&mut self, token_type: TokenType) -> bool {
        self.curr_token.token_type == token_type
    }

    fn eat(&mut self, token_type: TokenType, msg: &str) -> Token {
        if self.curr_token.token_type == token_type {
            let token = self.curr_token.clone();
            self.curr_token = self.peek_token.clone();
            self.peek_token = self.scanner.next_token();
            return token;
        }
        panic!("{}", msg.to_string())
    }

    fn statement(&mut self) -> ast::Statement {
        if self.matches(TokenType::Let) {
            return self.let_statement();
        };
        return self.expression_statement();
    }

    fn let_statement(&mut self) -> ast::Statement {
        let let_token = self.eat(TokenType::Let, "Expected let keyword");
        let identifier = self.eat(TokenType::Identifier, "Expected identifier");

        let mut init = None;
        if self.matches(TokenType::Assign) {
            init = Some(self.expression());
        }
        self.eat(TokenType::SemiColon, "Expected ';' after let statement");
        Statement::create_let(identifier.value, init)
    }

    fn expression_statement(&mut self) -> ast::Statement {
        let expr = self.expression();
        self.eat(TokenType::SemiColon, "Expected ';' after expression");
        Statement::create_expression(expr)
    }

    fn expression(&mut self) -> ast::Expression {
        Expression::create_binop()
    }
}

#[cfg(test)]
mod test {

    use super::Parser;
    use ast::{Program, Statement};
    use scanner::Scanner;

    fn parse(source: &str, expected: Program) {
        let scanner = Scanner::new(source.to_string());
        let mut parser = Parser::new(scanner);
        let res = parser.parse();
        println!("{:#?}", res);
        assert_eq!(res, expected);
    }

    #[test]
    fn let_stmt() {
        parse(
            "let _num;",
            vec![Statement::create_let(String::from("_num"), None)],
        )
    }
}
