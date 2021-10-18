use ast::{
    BinaryOperation, Expression, Identifier, Literal, LogicOperation, Program, Statement,
    UnaryOperation,
};
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
        self.prev_token = self.curr_token.clone();
        self.curr_token = self.peek_token.clone();
        self.peek_token = self.scanner.next_token();

        self.prev_token.clone()
    }

    fn is_end(&self) -> bool {
        self.curr_token.is_eof()
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

    fn eat_optional(&mut self, token_type: TokenType) -> Option<Token> {
        if self.curr_token.token_type == token_type {
            return Some(self.advance());
        }
        None
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
        Ok(Statement::create_expr(expr))
    }

    fn expression(&mut self) -> Result<Expression, ParserError> {
        if self.matches(vec![TokenType::Fun]) {
            return self.fun_expression();
        } else if self.matches(vec![TokenType::If]) {
            return self.if_expression();
        }
        self.assignment()
    }

    fn fun_expression(&mut self) -> Result<Expression, ParserError> {
        let mut params = vec![];

        let left_param = self.eat_optional(TokenType::LeftParen);

        match left_param {
            Some(_) => {
                if self.curr_token.token_type != TokenType::RightParen {
                    loop {
                        let ident = self.eat(TokenType::Identifier, "Expected parameter name")?;
                        params.push(Identifier::new(ident.value));
                        if !self.matches(vec![TokenType::Comma]) {
                            break;
                        }
                    }
                }
                self.eat(TokenType::RightParen, "Expcted ')' after parameters")?;
            }
            _ => {}
        }

        self.eat(TokenType::LeftBrace, "Expected '{'")?;
        let block = self.block_expression()?;

        Ok(Expression::create_function(params, block))
    }

    fn block_expression(&mut self) -> Result<Vec<Statement>, ParserError> {
        let mut stmts = vec![];
        while self.curr_token.token_type != TokenType::RightBrace && !self.is_end() {
            stmts.push(self.statement()?);
        }
        self.eat(TokenType::RightBrace, "Expected '}'")?;
        Ok(stmts)
    }

    fn if_expression(&mut self) -> Result<Expression, ParserError> {
        let left_paren = self.eat_optional(TokenType::LeftParen);
        let expr = self.expression()?;

        if left_paren.is_some() {
            self.eat(TokenType::RightParen, "Expected ')'")?;
        };

        self.eat(TokenType::LeftBrace, "Expected '{' after if condition")?;

        let then = self.block_expression()?;

        let mut not_then = None;
        if self.matches(vec![TokenType::Else]) {
            self.eat(TokenType::LeftBrace, "Expcted  '{' after else")?;
            not_then = Some(self.block_expression()?);
        }

        Ok(Expression::create_if(expr, then, not_then))
    }

    fn assignment(&mut self) -> Result<Expression, ParserError> {
        let expr = self.or()?;
        if self.matches(vec![TokenType::Assign]) {
            let curr_token = self.curr_token.clone();
            let rhs = self.assignment()?;
            match expr {
                Expression::LetRef(let_ref) => {
                    return Ok(Expression::create_assign(let_ref.name, rhs))
                }
                _ => {
                    return Err(ParserError::ExpectedToken(
                        "Invalid assignment target".to_string(),
                        curr_token,
                    ))
                }
            }
        }
        Ok(expr)
    }

    fn or(&mut self) -> Result<Expression, ParserError> {
        let mut expr = self.and()?;
        while self.matches(vec![TokenType::Or]) {
            let rhs = self.and()?;
            expr = Expression::create_logic(expr, LogicOperation::Or, rhs);
        }

        Ok(expr)
    }

    fn and(&mut self) -> Result<Expression, ParserError> {
        let mut expr = self.equality()?;
        while self.matches(vec![TokenType::And]) {
            let rhs = self.equality()?;
            expr = Expression::create_logic(expr, LogicOperation::And, rhs);
        }

        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expression, ParserError> {
        let mut expr = self.comparison()?;
        while self.matches(vec![TokenType::EqualEqual, TokenType::NotEqual]) {
            let tok = self.prev_token.clone();
            let rhs = self.comparison()?;
            let op = match tok.token_type {
                TokenType::EqualEqual => LogicOperation::Equal,
                _ => LogicOperation::NotEqual,
            };
            expr = Expression::create_logic(expr, op, rhs);
        }
        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expression, ParserError> {
        let mut expr = self.term()?;
        while self.matches(vec![
            TokenType::LessThan,
            TokenType::LessThanEqual,
            TokenType::GreaterThan,
            TokenType::GreaterThanEqual,
        ]) {
            let tok = self.prev_token.clone();
            let rhs = self.term()?;
            let op = match tok.token_type {
                TokenType::LessThan => LogicOperation::LessThan,
                TokenType::LessThanEqual => LogicOperation::LessThanEqual,
                TokenType::GreaterThan => LogicOperation::GreaterThan,
                _ => LogicOperation::GreaterThanEqual,
            };
            expr = Expression::create_logic(expr, op, rhs);
        }
        Ok(expr)
    }

    fn term(&mut self) -> Result<Expression, ParserError> {
        let mut res = self.factor()?;

        while self.matches(vec![TokenType::Plus, TokenType::Minus]) {
            let tok = self.prev_token.clone();
            let rhs = self.factor()?;
            let op = match tok.token_type {
                TokenType::Plus => BinaryOperation::Add,
                _ => BinaryOperation::Substract,
            };
            res = Expression::create_binop(res, op, rhs)
        }

        Ok(res)
    }
    fn factor(&mut self) -> Result<Expression, ParserError> {
        let mut res = self.unary()?;
        while self.matches(vec![TokenType::Star, TokenType::Slash]) {
            let tok = self.prev_token.clone();
            let rhs = self.unary()?;
            let op = match tok.token_type {
                TokenType::Star => BinaryOperation::Multiply,
                _ => BinaryOperation::Divide,
            };
            res = Expression::create_binop(res, op, rhs)
        }
        Ok(res)
    }

    fn unary(&mut self) -> Result<Expression, ParserError> {
        if self.matches(vec![TokenType::Plus, TokenType::Minus, TokenType::Bang]) {
            let tok = self.prev_token.clone();
            let rhs = self.unary()?;
            let op = match tok.token_type {
                TokenType::Plus => UnaryOperation::Plus,
                TokenType::Minus => UnaryOperation::Minus,
                TokenType::Bang => UnaryOperation::Not,
                _ => return Err(ParserError::UnexpectedToken(tok.clone())),
            };
            return Ok(Expression::create_unaryop(op, rhs));
        }
        self.call()
    }

    fn call(&mut self) -> Result<Expression, ParserError> {
        let mut expr = self.primary()?;

        if self.matches(vec![TokenType::LeftParen]) {
            let mut args = vec![];
            if self.curr_token.token_type != TokenType::RightParen {
                loop {
                    args.push(self.expression()?);

                    if !self.matches(vec![TokenType::Comma]) {
                        break;
                    }
                }
            }

            self.eat(TokenType::RightParen, "Unterminated function call")?;

            expr = Expression::create_call(expr, args);
        }

        Ok(expr)
    }

    fn primary(&mut self) -> Result<Expression, ParserError> {
        let token = self.curr_token.clone();
        if self.matches(vec![TokenType::IntConst]) {
            let val: i64 = match token.value.parse() {
                Ok(v) => v,
                Err(msg) => panic!("not a integer {}", msg),
            };
            return Ok(Expression::create_literal(Literal::Int(val)));
        }

        if self.matches(vec![TokenType::True, TokenType::False]) {
            let bool = match token.token_type {
                TokenType::True => true,
                _ => false,
            };

            return Ok(Expression::create_literal(Literal::Bool(bool)));
        }

        if self.matches(vec![TokenType::FloatConst]) {
            let val: f64 = match token.value.parse() {
                Ok(v) => v,
                Err(msg) => panic!("not a float {}", msg),
            };
            return Ok(Expression::create_literal(Literal::Float(val)));
        }

        if self.matches(vec![TokenType::Null]) {
            return Ok(Expression::create_literal(Literal::Null));
        }

        if self.matches(vec![TokenType::Identifier]) {
            return Ok(Expression::create_let_ref(Identifier::new(
                token.value.to_string(),
            )));
        }

        if self.matches(vec![TokenType::LeftParen]) {
            let expr = self.expression()?;
            self.eat(TokenType::RightParen, "Unterminated grouping expression")?;
            return Ok(Expression::create_grouping(expr));
        }

        Err(ParserError::UnexpectedToken(self.curr_token.clone()))
    }
}

#[cfg(test)]
mod test {

    use crate::ParserError;

    use super::Parser;
    use ast::{
        BinaryOperation, Expression, Identifier, Literal, LogicOperation, Program, Statement,
        UnaryOperation,
    };
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
        let (res, err) = run_parser(source);
        if err.len() > 0 {
            err.into_iter().for_each(|e| println!("{}", e));
            panic!("parsed with errors");
        }
        assert_eq!(res, expected);
    }

    // TODO check errors
    fn parse_error(source: &str, expected: Vec<ParserError>) {
        let (res, errors) = run_parser(source);

        let errs: Vec<String> = errors.clone().into_iter().map(|e| e.to_string()).collect();
        // let err: Vec<> = errors.clone().into_iter().map(|e| e.into_spanned_error());
        println!("{:?}", errs);
        println!("{:?}", res);
        assert_eq!(errors, expected)
    }

    fn int(val: i64) -> Expression {
        Expression::Literal(Literal::Int(val))
    }

    fn float(val: f64) -> Expression {
        Expression::Literal(Literal::Float(val))
    }

    fn bool_lit(val: bool) -> Expression {
        Expression::create_literal(Literal::Bool(val))
    }

    fn let_ref(val: &str) -> Expression {
        Expression::create_let_ref(Identifier::new(val.to_string()))
    }

    #[test]
    fn number_literal() {
        parse(
            "1;
        2;
        39;
        2347682384;
        34.6;
        22.732487656;
        2.;
        0.;",
            vec![
                Statement::create_expr(int(1)),
                Statement::create_expr(int(2)),
                Statement::create_expr(int(39)),
                Statement::create_expr(int(2347682384)),
                Statement::create_expr(float(34.6)),
                Statement::create_expr(float(22.732487656)),
                Statement::create_expr(float(2.0)),
                Statement::create_expr(float(0.)),
            ],
        )
    }

    #[test]
    fn bool_literal() {
        parse(
            "true; false;",
            vec![
                Statement::create_expr(Expression::Literal(Literal::Bool(true))),
                Statement::create_expr(Expression::Literal(Literal::Bool(false))),
            ],
        );
    }

    #[test]
    fn null_literal() {
        parse(
            "null;",
            vec![Statement::create_expr(Expression::create_literal(
                Literal::Null,
            ))],
        );
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
    }

    #[test]
    fn identifier() {
        parse(
            "num; num2;",
            vec![
                Statement::create_expr(Expression::create_let_ref(Identifier::new(
                    "num".to_string(),
                ))),
                Statement::create_expr(Expression::create_let_ref(Identifier::new(
                    "num2".to_string(),
                ))),
            ],
        );
    }

    #[test]
    fn assignment_expr() {
        parse(
            "num = 1;",
            vec![Statement::create_expr(Expression::create_assign(
                Identifier::new("num".to_string()),
                Expression::create_literal(Literal::Int(1)),
            ))],
        );
    }

    #[test]
    fn logic_expr() {
        parse(
            "1 && 2; 2 || 1;",
            vec![
                Statement::create_expr(Expression::create_logic(
                    int(1),
                    LogicOperation::And,
                    int(2),
                )),
                Statement::create_expr(Expression::create_logic(
                    int(2),
                    LogicOperation::Or,
                    int(1),
                )),
            ],
        );
    }

    #[test]
    fn equality_expr() {
        parse(
            "1 == 2; 2 != 1;",
            vec![
                Statement::create_expr(Expression::create_logic(
                    int(1),
                    LogicOperation::Equal,
                    int(2),
                )),
                Statement::create_expr(Expression::create_logic(
                    int(2),
                    LogicOperation::NotEqual,
                    int(1),
                )),
            ],
        );
    }

    #[test]
    fn comparison_expr() {
        parse(
            "1 < 2; 2 <= 1;3 > 1; 3 >= 1;",
            vec![
                Statement::create_expr(Expression::create_logic(
                    int(1),
                    LogicOperation::LessThan,
                    int(2),
                )),
                Statement::create_expr(Expression::create_logic(
                    int(2),
                    LogicOperation::LessThanEqual,
                    int(1),
                )),
                Statement::create_expr(Expression::create_logic(
                    int(3),
                    LogicOperation::GreaterThan,
                    int(1),
                )),
                Statement::create_expr(Expression::create_logic(
                    int(3),
                    LogicOperation::GreaterThanEqual,
                    int(1),
                )),
            ],
        );
    }

    #[test]
    fn binop_expr() {
        parse(
            "1 + 2 * 3 / 2;",
            vec![Statement::create_expr(Expression::create_binop(
                Expression::Literal(Literal::Int(1)),
                BinaryOperation::Add,
                Expression::create_binop(
                    Expression::create_binop(
                        Expression::Literal(Literal::Int(2)),
                        BinaryOperation::Multiply,
                        Expression::Literal(Literal::Int(3)),
                    ),
                    BinaryOperation::Divide,
                    Expression::Literal(Literal::Int(2)),
                ),
            ))],
        );
    }

    #[test]
    fn grouping_expr() {
        parse(
            "2 * (2 + 1);",
            vec![Statement::create_expr(Expression::create_binop(
                int(2),
                BinaryOperation::Multiply,
                Expression::create_grouping(Expression::create_binop(
                    int(2),
                    BinaryOperation::Add,
                    int(1),
                )),
            ))],
        );
    }

    #[test]
    fn unary_expr() {
        parse(
            "-1; 1 + -1; !1;",
            vec![
                Statement::create_expr(Expression::create_unaryop(UnaryOperation::Minus, int(1))),
                Statement::create_expr(Expression::create_binop(
                    int(1),
                    BinaryOperation::Add,
                    Expression::create_unaryop(UnaryOperation::Minus, int(1)),
                )),
                Statement::create_expr(Expression::create_unaryop(UnaryOperation::Not, int(1))),
            ],
        );
    }

    #[test]
    fn expression_stmt() {
        parse(
            "123; 23;",
            vec![
                Statement::create_expr(Expression::Literal(Literal::Int(123))),
                Statement::create_expr(Expression::Literal(Literal::Int(23))),
            ],
        );
    }

    #[test]
    fn function_call() {
        parse(
            "name(); this_is_a_func(); fnc(1, name, true);",
            vec![
                Statement::create_expr(Expression::create_call(let_ref("name"), vec![])),
                Statement::create_expr(Expression::create_call(let_ref("this_is_a_func"), vec![])),
                Statement::create_expr(Expression::create_call(
                    let_ref("fnc"),
                    vec![int(1), let_ref("name"), bool_lit(true)],
                )),
            ],
        );
    }

    #[test]
    fn function_expr() {
        parse(
            "
        fun {};
        fun () {};
        fun (x, y) {};
        fun (y, z) {
            let name = 2;
            123;
        };
        ",
            vec![
                Statement::create_expr(Expression::create_function(vec![], vec![])),
                Statement::create_expr(Expression::create_function(vec![], vec![])),
                Statement::create_expr(Expression::create_function(
                    vec![
                        Identifier::new("x".to_string()),
                        Identifier::new("y".to_string()),
                    ],
                    vec![],
                )),
                Statement::create_expr(Expression::create_function(
                    vec![
                        Identifier::new("y".to_string()),
                        Identifier::new("z".to_string()),
                    ],
                    vec![
                        Statement::create_let("name".to_string(), Some(int(2))),
                        Statement::create_expr(int(123)),
                    ],
                )),
            ],
        )
    }

    #[test]
    fn if_expr() {
        parse(
            "
            if (true) {};
            if true {};
            if false {};
            if (false) {} else {};
            if 123 {
                let x = 1;
            } else {
                let y = 2;
            };
            ",
            vec![
                Statement::create_expr(Expression::create_if(bool_lit(true), vec![], None)),
                Statement::create_expr(Expression::create_if(bool_lit(true), vec![], None)),
                Statement::create_expr(Expression::create_if(bool_lit(false), vec![], None)),
                Statement::create_expr(Expression::create_if(
                    bool_lit(false),
                    vec![],
                    Some(vec![]),
                )),
                Statement::create_expr(Expression::create_if(
                    int(123),
                    vec![Statement::create_let("x".to_string(), Some(int(1)))],
                    Some(vec![Statement::create_let("y".to_string(), Some(int(2)))]),
                )),
            ],
        );
    }
}
