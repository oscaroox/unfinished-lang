use ast::{
    BinaryOperation, DataClassInstanceField, Expression, Identifier, Literal, LogicOperation,
    Program, Statement, UnaryOperation,
};
use scanner::{Scanner, Token, TokenType};
use spanner::Span;

use crate::ParserError;

const RECOVER_SET: [TokenType; 5] = [
    TokenType::Let,
    TokenType::Fun,
    TokenType::Data,
    TokenType::Loop,
    TokenType::If,
];

#[derive(Debug, PartialEq)]
pub enum FunctionKind {
    None,
    Function,
    Method,
}

#[derive(Debug, PartialEq)]
pub enum LoopKind {
    None,
    Loop,
}

#[derive(Debug)]
pub struct Parser<'a> {
    scanner: Scanner<'a>,
    prev_token: Token,
    curr_token: Token,
    peek_token: Token,

    current_function: FunctionKind,
    current_loop: LoopKind,
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
            current_function: FunctionKind::None,
            current_loop: LoopKind::None,
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

            if RECOVER_SET.contains(&self.curr_token.token_type) {
                return;
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

    fn check(&mut self, token_type: TokenType) -> bool {
        self.curr_token.token_type == token_type
    }

    fn check_peek(&mut self, token_type: TokenType) -> bool {
        self.peek_token.token_type == token_type
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

        let tok = match self.curr_token.is_eof() {
            true => self.prev_token.clone(),
            false => self.curr_token.clone(),
        };

        Err(ParserError::ExpectedToken(msg.to_string(), tok))
    }

    fn error(&mut self, msg: String, token: Token) -> ParserError {
        ParserError::ExpectedToken(msg, token)
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
                Ok(expr) => match expr {
                    Expression::Function(fun) => Expression::create_function(
                        Some(identifier.value.to_string()),
                        fun.params,
                        fun.is_static,
                        fun.body,
                    ),
                    _ => expr,
                },
                Err(err) => match err {
                    ParserError::UnexpectedToken(_) => {
                        return Err(ParserError::ExpectedToken(
                            "Expected expression".to_string(),
                            self.curr_token.clone(),
                        ));
                    }
                    e => return Err(e),
                },
            };
            init = Some(expr);
        } else if !self.curr_token.is_semi_colon() {
            return Err(ParserError::ExpectedToken(
                "Expected '='".to_string(),
                self.curr_token.clone(),
            ));
        }

        self.eat(TokenType::SemiColon, "Expected ';' after let statement")?;
        Ok(Statement::create_let(identifier.value, init))
    }

    fn expression_statement(&mut self) -> Result<Statement, ParserError> {
        let expr = self.expression()?;
        match self.eat(TokenType::SemiColon, "Expected ';' after expression") {
            Ok(_) => Ok(Statement::create_expr(expr)),
            Err(err) => match err {
                ParserError::ExpectedToken(msg, _) => Err(self.error(msg, self.prev_token.clone())),
                _ => Err(err),
            },
        }
    }

    fn expression(&mut self) -> Result<Expression, ParserError> {
        if self.matches(vec![TokenType::If]) {
            return self.if_expression();
        } else if self.matches(vec![TokenType::LeftBrace]) {
            return Ok(Expression::create_block(self.block_expression()?));
        } else if self.matches(vec![TokenType::Return]) {
            return self.return_expression();
        } else if self.matches(vec![TokenType::Data]) {
            return self.data_class_expression();
        } else if self.matches(vec![TokenType::Loop]) {
            self.current_loop = LoopKind::Loop;
            let expr = self.loop_expression();
            self.current_loop = LoopKind::None;
            return expr;
        } else if self.matches(vec![TokenType::Break, TokenType::Continue]) {
            return self.continue_break_expression();
        } else if self.check(TokenType::Identifier) && self.check_peek(TokenType::LeftBrace) {
            return self.data_class_instantiate();
        }
        self.assignment()
    }

    fn continue_break_expression(&mut self) -> Result<Expression, ParserError> {
        if self.current_loop == LoopKind::None {
            let msg = if self.prev_token.token_type == TokenType::Break {
                String::from("Cannot use break outside loops")
            } else {
                String::from("Cannot use continue outside loops")
            };
            return Err(ParserError::Error(msg, self.prev_token.clone()));
        }
        Ok(match self.prev_token.token_type {
            TokenType::Break => Expression::create_break(),
            TokenType::Continue => Expression::create_continue(),
            _ => unreachable!(),
        })
    }

    fn loop_expression(&mut self) -> Result<Expression, ParserError> {
        let condition = if !self.check(TokenType::LeftBrace) {
            self.expression()?
        } else {
            Expression::create_literal(Literal::Bool(true))
        };

        // check if for loop i=0;
        if matches!(condition, Expression::Assign(_)) && self.check(TokenType::SemiColon) {
            self.eat(TokenType::SemiColon, "Expected ';'")?;
            let cond = self.expression()?;
            self.eat(TokenType::SemiColon, "Expected ';'")?;
            let incr = self.expression()?;
            self.eat(TokenType::SemiColon, "Expected ';'")?;

            self.eat(TokenType::LeftBrace, "Expected '{'")?;
            let body = self.block_expression()?;
            let loop_expr = Expression::create_loop(cond, body, Some(incr));
            let let_decl = condition.clone();
            let assign = let_decl.to_assign();

            return Ok(Expression::create_block(vec![
                Statement::create_let(assign.name.value, None),
                Statement::create_expr(let_decl),
                Statement::create_expr(loop_expr),
            ]));
        }

        self.eat(TokenType::LeftBrace, "Expected '{'")?;

        let body = self.block_expression()?;

        Ok(Expression::create_loop(condition, body, None))
    }

    fn data_class_instantiate(&mut self) -> Result<Expression, ParserError> {
        let ident = self.eat(TokenType::Identifier, "Expected identifier")?;
        self.eat(TokenType::LeftBrace, "Expected '{'")?;

        let mut fields = vec![];

        if self.curr_token.token_type != TokenType::RightBrace {
            while !self.curr_token.is_eof() {
                let id = self.eat(TokenType::Identifier, "Expected identifier")?;

                if self.matches(vec![TokenType::Comma])
                    || self.curr_token.token_type == TokenType::RightBrace
                {
                    let ident = Identifier::new(id.value.to_string());
                    fields.push(DataClassInstanceField::new(
                        ident.clone(),
                        Expression::create_let_ref(ident),
                    ));
                    if self.curr_token.token_type == TokenType::RightBrace {
                        break;
                    }
                    continue;
                }

                self.eat(TokenType::Colon, "Expected ':'")?;
                let expr = self.expression()?;
                let field =
                    DataClassInstanceField::new(Identifier::new(id.value.to_string()), expr);
                fields.push(field);

                if !self.matches(vec![TokenType::Comma])
                    || self.curr_token.token_type == TokenType::RightBrace
                {
                    break;
                }
            }
        }

        self.eat(TokenType::RightBrace, "Expected '}'")?;
        Ok(Expression::create_data_class_instance(
            Identifier::new(ident.value.to_string()),
            fields,
        ))
    }

    fn data_class_expression(&mut self) -> Result<Expression, ParserError> {
        let ident = self.eat(TokenType::Identifier, "Expected identifier")?;
        self.eat(TokenType::LeftBrace, "Expected '{'")?;

        let mut fields = vec![];
        let mut methods = vec![];

        if self.curr_token.token_type != TokenType::RightBrace {
            while !self.curr_token.is_eof() {
                let id = self.eat(TokenType::Identifier, "Expected identifier")?;
                fields.push(Identifier::new(id.value.to_string()));

                if !self.matches(vec![TokenType::Comma])
                    || self.curr_token.token_type == TokenType::RightBrace
                {
                    break;
                }
            }
        }

        self.eat(TokenType::RightBrace, "Expected '}'")?;

        let double_colon = self.eat_optional(TokenType::ColonColon);

        if double_colon.is_some() {
            self.eat(TokenType::LeftBrace, "Expected '{'")?;

            while !self.check(TokenType::RightBrace) && !self.is_end() {
                self.eat(TokenType::Fun, "Expected 'fun'")?;
                self.current_function = FunctionKind::Method;
                let expr = self.fun_expression(FunctionKind::Method);
                self.current_function = FunctionKind::None;
                methods.push(expr?);
            }

            self.eat(TokenType::RightBrace, "Expected '}'")?;
        }

        Ok(Expression::create_data_class(
            Identifier::new(ident.value.to_string()),
            fields,
            methods,
        ))
    }

    fn return_expression(&mut self) -> Result<Expression, ParserError> {
        if self.current_function == FunctionKind::None {
            return Err(ParserError::Error(
                "Cannot use return in top level code".to_string(),
                self.prev_token.clone(),
            ));
        }
        let mut value = None;

        if !self.curr_token.is_semi_colon() {
            value = Some(self.expression()?);
        }

        Ok(Expression::create_return(value))
    }

    fn fun_expression(&mut self, kind: FunctionKind) -> Result<Expression, ParserError> {
        let mut name = None;
        let mut params = vec![];
        let mut is_static = true;

        if kind == FunctionKind::Method {
            let ident = self.eat(TokenType::Identifier, "Expected method identifier")?;
            name = Some(ident.value);
        }

        let left_param = self.eat_optional(TokenType::LeftParen);

        if let Some(_) = left_param {
            if kind == FunctionKind::Method && self.check(TokenType::SELF) {
                let token = self.eat(TokenType::SELF, "Expected 'self'")?;
                self.eat_optional(TokenType::Comma);
                is_static = false;
                params.push(Identifier::with_token_type(token.value, token.token_type));
            } else if kind == FunctionKind::Function && self.check(TokenType::SELF) {
                return Err(ParserError::Error(
                    "Keyword 'self' can only be used in methods not functions".to_string(),
                    self.curr_token.clone(),
                ));
            }
            if self.curr_token.token_type != TokenType::RightParen {
                loop {
                    if self.check(TokenType::SELF) {
                        return Err(ParserError::Error(
                            "Expected 'self' to be the first parameter in a method".to_string(),
                            self.curr_token.clone(),
                        ));
                    }

                    let ident = self.eat(TokenType::Identifier, "Expected 'identifier'")?;
                    params.push(Identifier::new(ident.value));

                    if !self.matches(vec![TokenType::Comma])
                        || self.curr_token.token_type == TokenType::RightParen
                    {
                        break;
                    }
                }
            }
            self.eat(TokenType::RightParen, "Expected ')' after parameters")?;
        }

        if self.matches(vec![TokenType::Arrow]) {
            let expression = self.expression()?;
            return Ok(Expression::create_function(
                name,
                params,
                is_static,
                vec![Statement::create_expr(expression)],
            ));
        }

        self.eat(TokenType::LeftBrace, "Expected '{'")?;
        let block = self.block_expression()?;

        Ok(Expression::create_function(name, params, is_static, block))
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
            self.eat(TokenType::LeftBrace, "Expected  '{' after else")?;
            not_then = Some(self.block_expression()?);
        }

        Ok(Expression::create_if(expr, then, not_then))
    }

    fn assignment(&mut self) -> Result<Expression, ParserError> {
        let expr = self.or()?;
        if self.matches(vec![
            TokenType::Assign,
            TokenType::AssignPlus,
            TokenType::AssignMinus,
            TokenType::AssignSlash,
            TokenType::AssignStar,
        ]) {
            let curr_token = self.curr_token.clone();
            let assignment_tok = self.prev_token.clone();
            let rhs = self.assignment()?;

            match &expr {
                Expression::LetRef(let_ref) => match assignment_tok.token_type {
                    TokenType::Assign => {
                        return Ok(Expression::create_assign(let_ref.name.clone(), rhs))
                    }
                    _ => {
                        return Ok(Expression::create_assign(
                            let_ref.name.clone(),
                            Expression::create_binop(
                                Expression::create_let_ref(let_ref.name.clone()),
                                BinaryOperation::from_token(assignment_tok.clone()),
                                rhs,
                            ),
                        ))
                    }
                },
                Expression::Index(idx) => match assignment_tok.token_type {
                    TokenType::Assign => {
                        return Ok(Expression::create_set_index(
                            *idx.lhs.clone(),
                            *idx.index.clone(),
                            rhs,
                        ))
                    }
                    _ => {
                        return Ok(Expression::create_set_index(
                            *idx.lhs.clone(),
                            *idx.index.clone(),
                            Expression::create_binop(
                                expr,
                                BinaryOperation::from_token(assignment_tok.clone()),
                                rhs,
                            ),
                        ))
                    }
                },
                Expression::GetProperty(get) => match assignment_tok.token_type {
                    TokenType::Assign => {
                        return Ok(Expression::create_set_property(
                            *get.object.clone(),
                            get.name.clone(),
                            rhs,
                        ))
                    }
                    _ => {
                        return Ok(Expression::create_set_property(
                            *get.object.clone(),
                            get.name.clone(),
                            Expression::create_binop(
                                expr,
                                BinaryOperation::from_token(assignment_tok.clone()),
                                rhs,
                            ),
                        ))
                    }
                },
                _ => {
                    return Err(ParserError::Error(
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
                _ => return Err(ParserError::Error("".to_string(), tok.clone())),
            };
            return Ok(Expression::create_unaryop(op, rhs));
        }
        self.call()
    }

    fn call(&mut self) -> Result<Expression, ParserError> {
        let mut expr = self.primary()?;
        loop {
            if self.matches(vec![TokenType::LeftParen]) {
                let paren = self.prev_token.clone();

                let mut args = vec![];

                if self.curr_token.token_type != TokenType::RightParen && !self.is_end() {
                    loop {
                        args.push(self.expression()?);

                        if !self.matches(vec![TokenType::Comma]) {
                            break;
                        }
                    }
                }

                match self.eat_optional(TokenType::RightParen) {
                    Some(_) => {}
                    None => return Err(self.error("Unterminated function call".to_string(), paren)),
                }

                expr = Expression::create_call(expr, args);
            } else if self.matches(vec![TokenType::LeftBracket]) {
                let idx = self.expression()?;

                self.eat(TokenType::RightBracket, "Expected ']'")?;

                expr = Expression::create_index(expr, idx);
            } else if self.matches(vec![TokenType::Dot]) {
                let name = self.eat(TokenType::Identifier, "Expected identifier,")?;
                let is_callable = self.check(TokenType::LeftParen);
                expr = Expression::create_get_property(
                    expr,
                    Identifier::new(name.value.to_string()),
                    is_callable,
                )
            } else {
                break;
            }
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

        if self.matches(vec![TokenType::StringConst]) {
            return Ok(Expression::create_literal(Literal::String(token.value)));
        }

        if self.matches(vec![TokenType::Identifier]) {
            return Ok(Expression::create_let_ref(Identifier::new(
                token.value.to_string(),
            )));
        }

        if self.matches(vec![TokenType::Fun]) {
            self.current_function = FunctionKind::Function;
            let expr = self.fun_expression(FunctionKind::Function);
            self.current_function = FunctionKind::None;
            return expr;
        }

        if self.matches(vec![TokenType::SELF]) {
            if self.current_function == FunctionKind::None {
                return Err(ParserError::Error(
                    "Cannot use self outside methods".to_string(),
                    token.clone(),
                ));
            }
            return Ok(Expression::create_self(token.value));
        }

        if self.matches(vec![TokenType::LeftBracket]) {
            let mut exprs = vec![];
            if self.curr_token.token_type != TokenType::RightBracket {
                loop {
                    let expr = self.expression()?;
                    exprs.push(expr);

                    if !self.matches(vec![TokenType::Comma])
                        || self.curr_token.token_type == TokenType::RightBracket
                    {
                        break;
                    }
                }
            }

            self.eat(TokenType::RightBracket, "Expcted ']'")?;

            return Ok(Expression::create_literal(Literal::Array(exprs)));
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
        BinaryOperation, DataClassInstanceField, Expression, Identifier, Literal, LogicOperation,
        Program, Statement, UnaryOperation,
    };
    use scanner::{Scanner, TokenType};
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
    fn _parse_error(source: &str, expected: Vec<ParserError>) {
        let (res, errors) = run_parser(source);

        let errs: Vec<String> = errors.clone().into_iter().map(|e| e.to_string()).collect();
        // let err: Vec<> = errors.clone().into_iter().map(|e| e.into_spanned_error());
        println!("{:?}", errs);
        println!("{:?}", res);
        assert_eq!(errors, expected)
    }

    fn expr(val: Expression) -> Statement {
        Statement::create_expr(val)
    }

    fn int(val: i64) -> Expression {
        Expression::Literal(Literal::Int(val))
    }

    fn float(val: f64) -> Expression {
        Expression::Literal(Literal::Float(val))
    }

    fn array(val: Vec<Expression>) -> Expression {
        Expression::Literal(Literal::Array(val))
    }

    fn bool_lit(val: bool) -> Expression {
        Expression::create_literal(Literal::Bool(val))
    }

    fn ident(val: &str) -> Identifier {
        Identifier::new(val.to_string())
    }

    fn ident_self() -> Identifier {
        Identifier::with_token_type("self".to_string(), TokenType::SELF)
    }

    fn null() -> Expression {
        Expression::create_literal(Literal::Null)
    }

    fn let_ref(val: &str) -> Expression {
        Expression::create_let_ref(Identifier::new(val.to_string()))
    }

    fn let_stmt(name: &str, value: Option<Expression>) -> Statement {
        Statement::create_let(name.to_string(), value)
    }

    fn string_lit(val: &str) -> Expression {
        Expression::create_literal(Literal::String(val.to_string()))
    }

    fn data_class(name: &str, fields: Vec<&str>, methods: Vec<Expression>) -> Expression {
        Expression::create_data_class(
            Identifier::new(name.to_string()),
            fields
                .iter()
                .map(|v| Identifier::new(v.to_string()))
                .collect(),
            methods,
        )
    }

    fn data_class_instance(name: &str, fields: Vec<DataClassInstanceField>) -> Expression {
        Expression::create_data_class_instance(Identifier::new(name.to_string()), fields)
    }

    fn data_class_instance_field(name: &str, value: Expression) -> DataClassInstanceField {
        DataClassInstanceField::new(Identifier::new(name.to_string()), value)
    }

    fn fun_expr(
        name: Option<String>,
        params: Vec<Identifier>,
        is_static: bool,
        body: Vec<Statement>,
    ) -> Expression {
        Expression::create_function(name, params, is_static, body)
    }

    #[test]
    fn loop_for_expr() {
        parse(
            "
            loop i = 0; i < 10; i += 1; {};
            ",
            vec![expr(Expression::create_block(vec![
                let_stmt("i", None),
                expr(Expression::create_assign(ident("i"), int(0))),
                expr(Expression::create_loop(
                    Expression::create_logic(let_ref("i"), LogicOperation::LessThan, int(10)),
                    vec![],
                    Some(Expression::create_assign(
                        ident("i"),
                        Expression::create_binop(let_ref("i"), BinaryOperation::Add, int(1)),
                    )),
                )),
            ]))],
        );
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
    fn string_literal() {
        parse(
            r#""this is a string"; "string";"#,
            vec![
                Statement::create_expr(Expression::create_literal(Literal::String(
                    "this is a string".to_string(),
                ))),
                Statement::create_expr(Expression::create_literal(Literal::String(
                    "string".to_string(),
                ))),
            ],
        );
    }

    #[test]
    fn array_literal() {
        parse(
            r#"
            [1, 3, 4, true, false, "string", 2.0, null];
            [{}];
            [1,];
            "#,
            vec![
                Statement::create_expr(array(vec![
                    int(1),
                    int(3),
                    int(4),
                    bool_lit(true),
                    bool_lit(false),
                    string_lit("string"),
                    float(2.0),
                    null(),
                ])),
                Statement::create_expr(array(vec![Expression::create_block(vec![])])),
                Statement::create_expr(array(vec![int(1)])),
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
    fn self_keyword() {
        parse(
            "self;",
            vec![expr(Expression::create_self("self".to_string()))],
        );
    }

    #[test]
    fn parse_let_stmt() {
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
            "
            num = 1;
            num += 1;
            num -= 1;
            num *= 1;
            num /= 1;
 
            ",
            vec![
                Statement::create_expr(Expression::create_assign(
                    Identifier::new("num".to_string()),
                    Expression::create_literal(Literal::Int(1)),
                )),
                Statement::create_expr(Expression::create_assign(
                    Identifier::new("num".to_string()),
                    Expression::create_binop(let_ref("num"), BinaryOperation::Add, int(1)),
                )),
                Statement::create_expr(Expression::create_assign(
                    Identifier::new("num".to_string()),
                    Expression::create_binop(let_ref("num"), BinaryOperation::Substract, int(1)),
                )),
                Statement::create_expr(Expression::create_assign(
                    Identifier::new("num".to_string()),
                    Expression::create_binop(let_ref("num"), BinaryOperation::Multiply, int(1)),
                )),
                Statement::create_expr(Expression::create_assign(
                    Identifier::new("num".to_string()),
                    Expression::create_binop(let_ref("num"), BinaryOperation::Divide, int(1)),
                )),
            ],
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
    fn array_access() {
        parse(
            "array[0]; [1,2,3][2]; [[1], 2][0][0];",
            vec![
                Statement::create_expr(Expression::create_index(let_ref("array"), int(0))),
                Statement::create_expr(Expression::create_index(
                    array(vec![int(1), int(2), int(3)]),
                    int(2),
                )),
                Statement::create_expr(Expression::create_index(
                    Expression::create_index(array(vec![array(vec![int(1)]), int(2)]), int(0)),
                    int(0),
                )),
            ],
        )
    }

    #[test]
    fn array_index_assignment() {
        parse(
            "array[0] = 1;",
            vec![expr(Expression::create_set_index(
                let_ref("array"),
                int(0),
                int(1),
            ))],
        );

        parse(
            "
            array[0] += 1;
            array[0] -= 1;
            array[0] *= 1;
            array[0] /= 1;
            ",
            vec![
                expr(Expression::create_set_index(
                    let_ref("array"),
                    int(0),
                    Expression::create_binop(
                        Expression::create_index(let_ref("array"), int(0)),
                        BinaryOperation::Add,
                        int(1),
                    ),
                )),
                expr(Expression::create_set_index(
                    let_ref("array"),
                    int(0),
                    Expression::create_binop(
                        Expression::create_index(let_ref("array"), int(0)),
                        BinaryOperation::Substract,
                        int(1),
                    ),
                )),
                expr(Expression::create_set_index(
                    let_ref("array"),
                    int(0),
                    Expression::create_binop(
                        Expression::create_index(let_ref("array"), int(0)),
                        BinaryOperation::Multiply,
                        int(1),
                    ),
                )),
                expr(Expression::create_set_index(
                    let_ref("array"),
                    int(0),
                    Expression::create_binop(
                        Expression::create_index(let_ref("array"), int(0)),
                        BinaryOperation::Divide,
                        int(1),
                    ),
                )),
            ],
        );
    }

    #[test]
    fn function_call() {
        parse(
            "name(); this_is_a_func(); fnc(1, name, true);",
            vec![
                expr(Expression::create_call(let_ref("name"), vec![])),
                expr(Expression::create_call(let_ref("this_is_a_func"), vec![])),
                expr(Expression::create_call(
                    let_ref("fnc"),
                    vec![int(1), let_ref("name"), bool_lit(true)],
                )),
            ],
        );

        parse(
            "fun (){}();",
            vec![Statement::create_expr(Expression::create_call(
                Expression::create_function(None, vec![], true, vec![]),
                vec![],
            ))],
        );
    }

    #[test]
    fn return_expr() {
        parse(
            "return; return 1;",
            vec![
                expr(Expression::create_return(None)),
                expr(Expression::create_return(Some(int(1)))),
            ],
        )
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
                Statement::create_expr(Expression::create_function(None, vec![], true, vec![])),
                Statement::create_expr(Expression::create_function(None, vec![], true, vec![])),
                Statement::create_expr(Expression::create_function(
                    None,
                    vec![
                        Identifier::new("x".to_string()),
                        Identifier::new("y".to_string()),
                    ],
                    true,
                    vec![],
                )),
                Statement::create_expr(Expression::create_function(
                    None,
                    vec![
                        Identifier::new("y".to_string()),
                        Identifier::new("z".to_string()),
                    ],
                    true,
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

    #[test]
    fn block_expr() {
        parse(
            "
            {};
            let b = {
                let x = 1;
            };
            ",
            vec![
                Statement::create_expr(Expression::create_block(vec![])),
                Statement::create_let(
                    "b".to_string(),
                    Some(Expression::create_block(vec![Statement::create_let(
                        "x".to_string(),
                        Some(int(1)),
                    )])),
                ),
            ],
        );
    }

    #[test]
    fn data_class_expr() {
        parse(
            "
            data Person {};
        ",
            vec![expr(data_class("Person", vec![], vec![]))],
        );

        parse(
            "
            data Person {
                first_name,
            };
        ",
            vec![expr(data_class("Person", vec!["first_name"], vec![]))],
        );

        parse(
            "
            data Person {
                first_name,
                last_name,
                age
            };
        ",
            vec![expr(data_class(
                "Person",
                vec!["first_name", "last_name", "age"],
                vec![],
            ))],
        );

        parse(
            "
            data Person {
                first_name,
                last_name,
                age,
            };
        ",
            vec![expr(data_class(
                "Person",
                vec!["first_name", "last_name", "age"],
                vec![],
            ))],
        );
    }

    #[test]
    fn data_class_methods_expr() {
        parse(
            "
            data Person {
                first_name,
            } :: {
                fun new {
                }

                fun name(self) {

                }
            };
        ",
            vec![expr(data_class(
                "Person",
                vec!["first_name"],
                vec![
                    fun_expr(Some("new".to_string()), vec![], true, vec![]),
                    fun_expr(
                        Some("name".to_string()),
                        vec![Identifier::with_token_type(
                            "self".to_string(),
                            TokenType::SELF,
                        )],
                        false,
                        vec![],
                    ),
                ],
            ))],
        );
    }

    #[test]
    fn data_class_methods_self_expr() {
        parse(
            "
            data Person {
                first_name,
            } :: {
                fun name(self) {
                    self.first_name;
                }
                fun set_name(self, name) {
                    self.first_name = name;
                }
            };
        ",
            vec![expr(data_class(
                "Person",
                vec!["first_name"],
                vec![
                    fun_expr(
                        Some("name".to_string()),
                        vec![ident_self()],
                        false,
                        vec![expr(Expression::create_get_property(
                            Expression::create_self("self".to_string()),
                            Identifier::new("first_name".to_string()),
                            false,
                        ))],
                    ),
                    fun_expr(
                        Some("set_name".to_string()),
                        vec![ident_self(), ident("name")],
                        false,
                        vec![expr(Expression::create_set_property(
                            Expression::create_self("self".to_string()),
                            ident("first_name"),
                            let_ref("name"),
                        ))],
                    ),
                ],
            ))],
        );
    }

    #[test]
    fn data_class_instantiate_expr() {
        parse(
            "
            data Person {};
            Person {};
        ",
            vec![
                expr(data_class("Person", vec![], vec![])),
                expr(data_class_instance("Person", vec![])),
            ],
        );

        parse(
            r#"
            data Person {first_name, last_name, age};
            Person { first_name: "john", last_name: "doe", age: 23 };
        "#,
            vec![
                expr(data_class(
                    "Person",
                    vec!["first_name", "last_name", "age"],
                    vec![],
                )),
                expr(data_class_instance(
                    "Person",
                    vec![
                        data_class_instance_field("first_name", string_lit("john")),
                        data_class_instance_field("last_name", string_lit("doe")),
                        data_class_instance_field("age", int(23)),
                    ],
                )),
            ],
        );
    }

    #[test]
    fn data_class_instantiate_shorthand_expr() {
        parse(
            r#"
            data Person {
                id,
                first_name,
            };
            let id = 123;
            let first_name = "John";
            Person {
                id,
                first_name,
            };
            Person {
                id,
                first_name
            };
        "#,
            vec![
                expr(data_class("Person", vec!["id", "first_name"], vec![])),
                let_stmt("id", Some(int(123))),
                let_stmt("first_name", Some(string_lit("John"))),
                expr(data_class_instance(
                    "Person",
                    vec![
                        data_class_instance_field("id", let_ref("id")),
                        data_class_instance_field("first_name", let_ref("first_name")),
                    ],
                )),
                expr(data_class_instance(
                    "Person",
                    vec![
                        data_class_instance_field("id", let_ref("id")),
                        data_class_instance_field("first_name", let_ref("first_name")),
                    ],
                )),
            ],
        );
    }

    #[test]
    fn get_property_expr() {
        parse(
            "
            data Person {
                first_name,
                last_name,
                age,
            };
            let p = Person{};
            p.first_name;
        ",
            vec![
                expr(data_class(
                    "Person",
                    vec!["first_name", "last_name", "age"],
                    vec![],
                )),
                Statement::create_let("p".to_string(), Some(data_class_instance("Person", vec![]))),
                expr(Expression::create_get_property(
                    let_ref("p"),
                    Identifier::new("first_name".to_string()),
                    false,
                )),
            ],
        );
    }

    #[test]
    fn set_property_expr() {
        parse(
            r#"
            data Person {
                first_name,
                last_name,
                age,
            };
            let p = Person{};
            p.age = 1;
        "#,
            vec![
                expr(data_class(
                    "Person",
                    vec!["first_name", "last_name", "age"],
                    vec![],
                )),
                Statement::create_let("p".to_string(), Some(data_class_instance("Person", vec![]))),
                expr(Expression::create_set_property(
                    let_ref("p"),
                    Identifier::new("age".to_string()),
                    int(1),
                )),
            ],
        );
        parse(
            r#"
            data Person {
                first_name,
                last_name,
                age,
            };
            let p = Person{};
            p.age += 1;
            p.age -= 1;
            p.age *= 1;
            p.age /= 1;
        "#,
            vec![
                expr(data_class(
                    "Person",
                    vec!["first_name", "last_name", "age"],
                    vec![],
                )),
                Statement::create_let("p".to_string(), Some(data_class_instance("Person", vec![]))),
                expr(Expression::create_set_property(
                    let_ref("p"),
                    Identifier::new("age".to_string()),
                    Expression::create_binop(
                        Expression::create_get_property(
                            let_ref("p"),
                            Identifier::new("age".to_string()),
                            false,
                        ),
                        BinaryOperation::Add,
                        int(1),
                    ),
                )),
                expr(Expression::create_set_property(
                    let_ref("p"),
                    Identifier::new("age".to_string()),
                    Expression::create_binop(
                        Expression::create_get_property(
                            let_ref("p"),
                            Identifier::new("age".to_string()),
                            false,
                        ),
                        BinaryOperation::Substract,
                        int(1),
                    ),
                )),
                expr(Expression::create_set_property(
                    let_ref("p"),
                    Identifier::new("age".to_string()),
                    Expression::create_binop(
                        Expression::create_get_property(
                            let_ref("p"),
                            Identifier::new("age".to_string()),
                            false,
                        ),
                        BinaryOperation::Multiply,
                        int(1),
                    ),
                )),
                expr(Expression::create_set_property(
                    let_ref("p"),
                    Identifier::new("age".to_string()),
                    Expression::create_binop(
                        Expression::create_get_property(
                            let_ref("p"),
                            Identifier::new("age".to_string()),
                            false,
                        ),
                        BinaryOperation::Divide,
                        int(1),
                    ),
                )),
            ],
        );
    }

    #[test]
    fn loop_expr() {
        parse(
            "
            loop {}; 
            loop true {}; 
            loop 1 < 2 {};
            ",
            vec![
                expr(Expression::create_loop(bool_lit(true), vec![], None)),
                expr(Expression::create_loop(bool_lit(true), vec![], None)),
                expr(Expression::create_loop(
                    Expression::create_logic(int(1), LogicOperation::LessThan, int(2)),
                    vec![],
                    None,
                )),
            ],
        )
    }

    #[test]
    fn break_continue_expr() {
        parse(
            "
        loop { break; }; 
        loop { continue; }; 
        ",
            vec![
                expr(Expression::create_loop(
                    bool_lit(true),
                    vec![expr(Expression::create_break())],
                    None,
                )),
                expr(Expression::create_loop(
                    bool_lit(true),
                    vec![expr(Expression::create_continue())],
                    None,
                )),
            ],
        )
    }
}
