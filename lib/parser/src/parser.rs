use crate::ParserError;
use ast::{
    BinaryOperation, DataStructInstanceField, EType, Expression, Identifier, Literal,
    LogicOperation, Program, Type, UnaryOperation,
};
use scanner::{Scanner, ScannerMode, Token, TokenType, TokenWithSpan};
use span_util::Span;

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

type ParserResult = Result<Expression, ParserError>;

#[derive(Debug)]
pub struct Parser {
    scanner: Scanner,
    prev_token: TokenWithSpan,
    curr_token: TokenWithSpan,
    errors: Vec<ParserError>,
}

impl Parser {
    pub fn new(mut scanner: Scanner) -> Parser {
        let curr_token = scanner.next_token();
        Parser {
            scanner,
            prev_token: Token::eof(0..0),
            curr_token,
            errors: vec![],
        }
    }

    pub fn parse(&mut self) -> (Program, Vec<ParserError>) {
        let mut program = Vec::new();
        while !self.is_end() {
            match self.expression_statement(true) {
                Ok(stmt) => program.push(stmt),
                Err(err) => self.error_and_sync(err),
            }
        }
        (program, self.errors.clone())
    }

    fn error_and_sync(&mut self, err: ParserError) {
        self.sync();
        self.errors.push(err);
    }

    fn sync(&mut self) {
        self.advance();

        while !self.is_end() {
            if self.prev_token.0.token_type == TokenType::SemiColon {
                return;
            }

            if RECOVER_SET.contains(&self.curr_token.0.token_type) {
                return;
            }
            self.advance();
        }
    }

    fn matches(&mut self, types: Vec<TokenType>) -> bool {
        for token_type in types.iter() {
            if *token_type == self.curr_token.0.token_type {
                self.advance();
                return true;
            }
        }
        false
    }

    fn check(&mut self, token_type: TokenType) -> bool {
        self.curr_token.0.token_type == token_type
    }

    fn check_peek(&mut self, token_type: TokenType) -> bool {
        self.peek().token_type == token_type
    }

    fn peek(&mut self) -> Token {
        let token = self.scanner.next_token();
        self.scanner.backtrack();
        token.0
    }

    fn advance(&mut self) -> Token {
        self.prev_token = self.curr_token.clone();
        self.curr_token = self.scanner.next_token();
        self.prev_token.clone().0
    }

    fn is_end(&self) -> bool {
        self.curr_token.0.is_eof()
    }

    fn eat(&mut self, token_type: TokenType, msg: &str) -> Result<Token, ParserError> {
        if self.curr_token.0.token_type == token_type {
            return Ok(self.advance());
        }

        let tok = match self.curr_token.0.is_eof() {
            true => self.prev_token.clone(),
            false => self.curr_token.clone(),
        };

        let msg = format!("{}, found '{}'", msg.to_string(), tok.0);
        Err(ParserError::ExpectedToken(msg, tok))
    }

    fn eat_optional(&mut self, token_type: TokenType) -> Option<Token> {
        if self.curr_token.0.token_type == token_type {
            return Some(self.advance());
        }
        None
    }

    fn eat_semi(&mut self) -> Result<Token, ParserError> {
        self.eat(TokenType::SemiColon, "Expected ';' after expression")
    }

    fn expression_statement(&mut self, require_semicolon: bool) -> ParserResult {
        let expr = self.expression()?;
        if require_semicolon {
            self.eat_semi()?;
        }
        Ok(expr)
        // match self.eat(TokenType::SemiColon, "Expected ';' after expression") {
        //     Ok(_) => Ok(expr),
        //     Err(err) => match err {
        //         // TODO find out why i intercept the error message here
        //         // ParserError::ExpectedToken(msg, _) => Err(self.error(msg, self.prev_token.clone())),
        //         _ => Err(err),
        //     },
        // }
    }

    fn expression(&mut self) -> ParserResult {
        if self.matches(vec![TokenType::Let]) {
            return self.let_expression();
        } else if self.matches(vec![TokenType::If]) {
            return self.if_expression();
        } else if self.matches(vec![TokenType::LeftBrace]) {
            return self.block_expression();
        } else if self.matches(vec![TokenType::Return]) {
            return self.return_expression();
        } else if self.matches(vec![TokenType::Data]) {
            return self.data_struct_expression();
        } else if self.matches(vec![TokenType::Loop]) {
            return self.loop_expression();
        } else if self.matches(vec![TokenType::Break, TokenType::Continue]) {
            return self.continue_break_expression();
        }

        self.assignment()
    }

    fn parse_type(&mut self) -> Result<Type, ParserError> {
        let token = self.curr_token.clone();
        if self.matches(vec![
            TokenType::Int,
            TokenType::Float,
            TokenType::String,
            TokenType::Bool,
            TokenType::Identifier,
        ]) {
            let mut is_array = false;
            let etype = match &token.0.token_type {
                TokenType::Int => EType::Int,
                TokenType::Float => EType::Float,
                TokenType::String => EType::String,
                TokenType::Bool => EType::Bool,
                TokenType::Identifier => EType::User(token.0.value.to_string()),
                _ => unreachable!(),
            };

            if self.matches(vec![TokenType::LeftBracket]) {
                self.eat(TokenType::RightBracket, "Expected ']'")?;
                is_array = true;
            }

            return Ok(Type::new(etype, is_array));
        }

        if self.matches(vec![TokenType::Unit]) {
            return Err(ParserError::InvalidUseOfUnitType(token));
        }

        Err(ParserError::InvalidType(token))
    }

    fn let_expression(&mut self) -> ParserResult {
        let start_span: Span = self.prev_token.1.clone();
        let ident = self.curr_token.clone();
        let identifier = self.eat(TokenType::Identifier, "Expected identifier")?;

        let has_colon = self.eat_optional(TokenType::Colon);

        let itype = match has_colon {
            Some(_) => Some(self.parse_type()?),
            None => None,
        };

        let mut init = None;

        if self.matches(vec![TokenType::Assign]) {
            let expr = match self.expression() {
                Ok(expr) => match expr {
                    Expression::Function(fun) => Expression::create_function(
                        Some(identifier.value.to_string()),
                        fun.0.params,
                        fun.0.is_static,
                        *fun.0.body,
                        fun.1,
                    ),
                    _ => expr,
                },
                Err(err) => match err {
                    ParserError::UnexpectedToken(_) => {
                        return Err(ParserError::ExpectedToken(
                            format!(
                                "Expected expression here, found '{}'",
                                self.curr_token.0.clone()
                            ),
                            self.curr_token.clone(),
                        ));
                    }
                    e => return Err(e),
                },
            };
            init = Some(expr);
        }

        if itype.is_none() && init.is_none() {
            return Err(ParserError::TypeAnnotationNeeded(ident));
        }

        let span: Span = start_span.extend(self.curr_token.1.clone());

        Ok(Expression::create_let(
            Identifier::with_ident_type(identifier.value, itype),
            init,
            span,
        ))
    }

    fn continue_break_expression(&mut self) -> ParserResult {
        Ok(match self.prev_token.0.token_type {
            TokenType::Break => Expression::create_break(self.prev_token.1.clone()),
            TokenType::Continue => Expression::create_continue(self.prev_token.1.clone()),
            _ => unreachable!(),
        })
    }

    fn loop_expression(&mut self) -> ParserResult {
        let if_span = self.prev_token.1.clone();
        let token = self.curr_token.clone();
        let condition = if !self.check(TokenType::LeftBrace) {
            self.expression()?
        } else {
            Expression::create_literal(Literal::Bool(true), if_span)
        };

        let iterator = if self.matches(vec![TokenType::In]) {
            Some(self.expression()?)
        } else {
            None
        };

        self.eat(TokenType::LeftBrace, "Expected '{'")?;

        let body = self.block_expression()?;

        if let Some(iter) = iterator {
            return match &condition {
                Expression::LetRef(let_ref) => Ok(Expression::create_loop(
                    Expression::create_let(let_ref.0.name.clone(), None, let_ref.1.clone()),
                    body,
                    Some(iter.clone()),
                    let_ref.1.extend(iter.get_span()),
                )),
                _ => Err(ParserError::ExpectedToken(
                    "Expected variable".to_string(),
                    token,
                )),
            };
        }

        let span = condition.get_span();
        Ok(Expression::create_loop(condition, body, None, span))
    }

    fn data_struct_instantiate(&mut self) -> ParserResult {
        let identifier_span = self.curr_token.1.clone();
        let ident = self.eat(TokenType::Identifier, "Expected identifier")?;
        self.eat(TokenType::LeftBrace, "Expected '{'")?;

        let mut fields = vec![];

        if self.curr_token.0.token_type != TokenType::RightBrace {
            while !self.curr_token.0.is_eof() {
                let id_span = self.curr_token.1.clone();
                let id = self.eat(TokenType::Identifier, "Expected identifier")?;

                if self.matches(vec![TokenType::Comma])
                    || self.curr_token.0.token_type == TokenType::RightBrace
                {
                    let ident = Identifier::new(id.value.to_string());
                    fields.push(DataStructInstanceField::new(
                        ident.clone(),
                        Expression::create_let_ref(ident, id_span),
                    ));
                    if self.curr_token.0.token_type == TokenType::RightBrace {
                        break;
                    }
                    continue;
                }

                self.eat(TokenType::Colon, "Expected ':'")?;
                let expr = self.expression()?;
                let field =
                    DataStructInstanceField::new(Identifier::new(id.value.to_string()), expr);
                fields.push(field);

                if !self.matches(vec![TokenType::Comma])
                    || self.curr_token.0.token_type == TokenType::RightBrace
                {
                    break;
                }
            }
        }

        self.eat(TokenType::RightBrace, "Expected '}'")?;
        Ok(Expression::create_data_struct_instance(
            Identifier::new(ident.value.to_string()),
            fields,
            identifier_span,
        ))
    }

    fn data_struct_expression(&mut self) -> ParserResult {
        let ident_span = self.curr_token.1.clone();
        let ident = self.eat(TokenType::Identifier, "Expected identifier")?;
        self.eat(TokenType::LeftBrace, "Expected '{'")?;

        let mut fields = vec![];
        let mut methods = vec![];

        if self.curr_token.0.token_type != TokenType::RightBrace {
            while !self.curr_token.0.is_eof() {
                let id = self.eat(TokenType::Identifier, "Expected identifier")?;
                fields.push(Identifier::new(id.value.to_string()));

                if !self.matches(vec![TokenType::Comma])
                    || self.curr_token.0.token_type == TokenType::RightBrace
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
                methods.push(self.fun_expression(FunctionKind::Method)?);
            }

            self.eat(TokenType::RightBrace, "Expected '}'")?;
        }

        Ok(Expression::create_data_struct(
            Identifier::new(ident.value.to_string()),
            fields,
            methods,
            ident_span,
        ))
    }

    fn return_expression(&mut self) -> ParserResult {
        let mut span: Span = self.prev_token.1.clone();
        let mut value = None;

        if !self.curr_token.0.is_semi_colon() {
            let expr = self.expression()?;
            span = span.extend(expr.get_span());
            value = Some(expr);
        }

        let span = span.extend(self.curr_token.1.clone());
        Ok(Expression::create_return(value, span))
    }

    fn fun_expression(&mut self, kind: FunctionKind) -> ParserResult {
        let fun_span: Span = self.prev_token.1.clone();
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
            if self.curr_token.0.token_type != TokenType::RightParen {
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
                        || self.curr_token.0.token_type == TokenType::RightParen
                    {
                        break;
                    }
                }
            }
            self.eat(TokenType::RightParen, "Expected ')' after parameters")?;
        }

        if self.matches(vec![TokenType::Arrow]) {
            let arrow_span: Span = self.prev_token.1.clone();
            let expression = self.expression()?;
            let expr_span = expression.get_span();
            let full_span = fun_span.extend(arrow_span);
            return Ok(Expression::create_function(
                name,
                params,
                is_static,
                Expression::create_block(
                    vec![Expression::create_implicit_return(
                        expression,
                        expr_span.clone(),
                    )],
                    expr_span.clone(),
                ),
                full_span,
            ));
        }

        let span = fun_span.extend(self.prev_token.1.clone());
        self.eat(TokenType::LeftBrace, "Expected '{'")?;
        let block = self.block_expression()?;

        Ok(Expression::create_function(
            name, params, is_static, block, span,
        ))
    }

    fn block_expression(&mut self) -> ParserResult {
        let span_left_brace = self.prev_token.1.clone();
        let mut exprs = vec![];
        while !self.check(TokenType::RightBrace) && !self.is_end() {
            match self.expression_statement(false) {
                Ok(e) => {
                    exprs.push(e);
                    if !self.check(TokenType::RightBrace) {
                        self.eat_semi()?;
                    }
                }
                Err(err) => self.error_and_sync(err),
            }
        }

        if !exprs.is_empty() && !self.prev_token.0.is_semi_colon() {
            let expr = exprs.pop().unwrap();
            let span = expr.get_span();
            exprs.push(Expression::create_implicit_return(expr, span));
        }

        self.eat(TokenType::RightBrace, "Expected '}'")?;
        Ok(Expression::create_block(exprs, span_left_brace))
    }

    fn if_expression(&mut self) -> ParserResult {
        let left_paren = self.eat_optional(TokenType::LeftParen);
        let expr = self.expression()?;
        let expr_span = expr.get_span();
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

        Ok(Expression::create_if(expr, then, not_then, expr_span))
    }

    fn assignment(&mut self) -> ParserResult {
        let expr = self.or()?;
        if self.matches(vec![
            TokenType::Assign,
            TokenType::AssignPlus,
            TokenType::AssignMinus,
            TokenType::AssignSlash,
            TokenType::AssignStar,
        ]) {
            let assignment_tok = self.prev_token.clone();
            let curr_token = self.curr_token.clone();
            let rhs = self.assignment()?;

            let rhs_span = rhs.get_span();
            let full_span: Span = expr.get_span().extend(rhs_span.clone());

            match &expr {
                Expression::LetRef(let_ref) => match assignment_tok.0.token_type {
                    TokenType::Assign => {
                        return Ok(Expression::create_assign(
                            let_ref.0.name.clone(),
                            rhs,
                            full_span,
                        ))
                    }
                    _ => {
                        return Ok(Expression::create_assign(
                            let_ref.0.name.clone(),
                            Expression::create_binop(
                                Expression::create_let_ref(
                                    let_ref.0.name.clone(),
                                    rhs_span.clone(),
                                ),
                                BinaryOperation::from_token(assignment_tok.0.clone()),
                                rhs,
                                rhs_span,
                            ),
                            full_span,
                        ))
                    }
                },
                Expression::Index(idx) => match assignment_tok.0.token_type {
                    TokenType::Assign => {
                        return Ok(Expression::create_set_index(
                            *idx.0.lhs.clone(),
                            *idx.0.index.clone(),
                            rhs,
                            full_span,
                        ))
                    }
                    _ => {
                        return Ok(Expression::create_set_index(
                            *idx.0.lhs.clone(),
                            *idx.0.index.clone(),
                            Expression::create_binop(
                                expr,
                                BinaryOperation::from_token(assignment_tok.0.clone()),
                                rhs,
                                rhs_span,
                            ),
                            full_span,
                        ))
                    }
                },
                Expression::GetProperty(get) => match assignment_tok.0.token_type {
                    TokenType::Assign => {
                        return Ok(Expression::create_set_property(
                            *get.0.object.clone(),
                            get.0.name.clone(),
                            rhs,
                            full_span,
                        ))
                    }
                    _ => {
                        return Ok(Expression::create_set_property(
                            *get.0.object.clone(),
                            get.0.name.clone(),
                            Expression::create_binop(
                                expr,
                                BinaryOperation::from_token(assignment_tok.0.clone()),
                                rhs,
                                rhs_span,
                            ),
                            full_span,
                        ))
                    }
                },
                _ => return Err(ParserError::InvalidAssignmentTarget(curr_token)),
            }
        }
        Ok(expr)
    }

    fn or(&mut self) -> ParserResult {
        let mut expr = self.and()?;
        while self.matches(vec![TokenType::Or]) {
            let rhs = self.and()?;
            let span = expr.get_span().extend(rhs.get_span());
            expr = Expression::create_logic(expr, LogicOperation::Or, rhs, span);
        }

        Ok(expr)
    }

    fn and(&mut self) -> ParserResult {
        let mut expr = self.equality()?;
        while self.matches(vec![TokenType::And]) {
            let rhs = self.equality()?;
            let span = expr.get_span().extend(rhs.get_span());
            expr = Expression::create_logic(expr, LogicOperation::And, rhs, span);
        }

        Ok(expr)
    }

    fn equality(&mut self) -> ParserResult {
        let mut expr = self.comparison()?;
        while self.matches(vec![TokenType::EqualEqual, TokenType::NotEqual]) {
            let tok = self.prev_token.clone();
            let rhs = self.comparison()?;
            let span = expr.get_span().extend(rhs.get_span());
            let op = match tok.0.token_type {
                TokenType::EqualEqual => LogicOperation::Equal,
                _ => LogicOperation::NotEqual,
            };
            expr = Expression::create_logic(expr, op, rhs, span);
        }
        Ok(expr)
    }

    fn comparison(&mut self) -> ParserResult {
        let mut expr = self.term()?;
        while self.matches(vec![
            TokenType::LessThan,
            TokenType::LessThanEqual,
            TokenType::GreaterThan,
            TokenType::GreaterThanEqual,
        ]) {
            let tok = self.prev_token.clone();
            let rhs = self.term()?;
            let span = expr.get_span().extend(rhs.get_span());
            let op = match tok.0.token_type {
                TokenType::LessThan => LogicOperation::LessThan,
                TokenType::LessThanEqual => LogicOperation::LessThanEqual,
                TokenType::GreaterThan => LogicOperation::GreaterThan,
                _ => LogicOperation::GreaterThanEqual,
            };
            expr = Expression::create_logic(expr, op, rhs, span);
        }
        Ok(expr)
    }

    fn term(&mut self) -> ParserResult {
        let mut expr = self.factor()?;
        while self.matches(vec![TokenType::Plus, TokenType::Minus]) {
            let tok = self.prev_token.clone();
            let rhs = self.factor()?;
            let span = expr.get_span().extend(rhs.get_span());
            let op = match tok.0.token_type {
                TokenType::Plus => BinaryOperation::Add,
                _ => BinaryOperation::Substract,
            };
            expr = Expression::create_binop(expr, op, rhs, span)
        }

        Ok(expr)
    }

    fn factor(&mut self) -> ParserResult {
        let mut expr = self.unary()?;

        while self.matches(vec![TokenType::Star, TokenType::Slash]) {
            let tok = self.prev_token.clone();
            let rhs = self.unary()?;
            let span = expr.get_span().extend(rhs.get_span());
            let op = match tok.0.token_type {
                TokenType::Star => BinaryOperation::Multiply,
                _ => BinaryOperation::Divide,
            };
            expr = Expression::create_binop(expr, op, rhs, span)
        }
        Ok(expr)
    }

    fn unary(&mut self) -> ParserResult {
        if self.matches(vec![TokenType::Plus, TokenType::Minus, TokenType::Bang]) {
            let tok = self.prev_token.clone();
            let rhs = self.unary()?;
            let span: Span = tok.1.extend(rhs.get_span());
            let op = match tok.0.token_type {
                TokenType::Plus => UnaryOperation::Plus,
                TokenType::Minus => UnaryOperation::Minus,
                TokenType::Bang => UnaryOperation::Not,
                _ => return Err(ParserError::InvalidUnaryOperation(tok)),
            };
            return Ok(Expression::create_unaryop(op, rhs, span));
        }
        self.call()
    }

    fn call(&mut self) -> ParserResult {
        let mut expr = self.primary()?;
        let primary_span = expr.get_span();
        loop {
            if self.matches(vec![TokenType::LeftParen]) {
                let paren = self.prev_token.clone();

                let mut args = vec![];

                if self.curr_token.0.token_type != TokenType::RightParen && !self.is_end() {
                    loop {
                        args.push(self.expression()?);

                        if !self.matches(vec![TokenType::Comma]) {
                            break;
                        }
                    }
                }

                match self.eat_optional(TokenType::RightParen) {
                    Some(_) => {}
                    None => {
                        return Err(ParserError::UnterminatedFunctionCall(paren));
                    }
                }
                let span: Span = primary_span.extend(self.prev_token.1.clone());
                expr = Expression::create_call(expr, args, span);
            } else if self.matches(vec![TokenType::LeftBracket]) {
                let idx = self.expression()?;

                self.eat(TokenType::RightBracket, "Expected ']'")?;

                expr = Expression::create_index(
                    expr,
                    idx,
                    primary_span.extend(self.prev_token.1.clone()),
                );
            } else if self.matches(vec![TokenType::Dot]) {
                let ident_span = self.curr_token.1.clone();
                let name = self.eat(TokenType::Identifier, "Expected identifier")?;
                let is_callable = self.check(TokenType::LeftParen);
                expr = Expression::create_get_property(
                    expr,
                    Identifier::new(name.value.to_string()),
                    is_callable,
                    ident_span,
                )
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn primary(&mut self) -> ParserResult {
        let token = self.curr_token.clone();
        let span: Span = token.1.clone();
        if self.matches(vec![TokenType::IntConst]) {
            let val: i64 = match token.0.value.parse() {
                Ok(v) => v,
                Err(msg) => panic!("not a integer {}", msg),
            };
            return Ok(Expression::create_literal(Literal::Int(val), span));
        }

        if self.matches(vec![TokenType::True, TokenType::False]) {
            let bool = match token.0.token_type {
                TokenType::True => true,
                _ => false,
            };

            return Ok(Expression::create_literal(Literal::Bool(bool), span));
        }

        if self.matches(vec![TokenType::FloatConst]) {
            let val: f64 = match token.0.value.parse() {
                Ok(v) => v,
                Err(msg) => panic!("not a float {}", msg),
            };
            return Ok(Expression::create_literal(Literal::Float(val), span));
        }

        if self.matches(vec![TokenType::Null]) {
            return Ok(Expression::create_literal(Literal::Null, span));
        }

        if self.check(TokenType::DoubleQuote) {
            return self.parse_string();
        }

        if self.matches(vec![TokenType::StringConst]) {
            return Ok(Expression::create_literal(
                Literal::String(token.0.value),
                span,
            ));
        }

        if self.check(TokenType::Identifier)
            && self.check_peek(TokenType::LeftBrace)
            // TODO data struct instantiation has the same syntax
            //  When using the In and If expression
            //  e.g if value {}
            //        ^^^^^^^^ after the If the parser will try to parse the expression after if as a data struct
            //  possible solution is the one below checking if the the previous token is one of the keywords: If and In
            //  there should be a better solution: what if we implement pattern matching / destructering
            //  let Person { name } = person; // will be parsed as data struct instantiation
            //  match person {
            //      Person { name } => name,
            //  }
            //  In the match expression the parser would try to parse the first match arm as
            //  a data struct instantation
            //
            //  Possible solution is to use the :: token for instantiation
            //  e.g: Person :: { id: 1 };
            && self.prev_token.0.token_type != TokenType::In
            && self.prev_token.0.token_type != TokenType::If
            && self.prev_token.0.token_type != TokenType::Loop
        {
            return self.data_struct_instantiate();
        }

        if self.matches(vec![TokenType::Identifier]) {
            return Ok(Expression::create_let_ref(
                Identifier::new(token.0.value.to_string()),
                span,
            ));
        }

        if self.matches(vec![TokenType::Fun]) {
            return self.fun_expression(FunctionKind::Function);
        }

        if self.matches(vec![TokenType::SELF]) {
            return Ok(Expression::create_self(token.0.value, token.1));
        }

        if self.matches(vec![TokenType::LeftBracket]) {
            let mut exprs = vec![];
            if self.curr_token.0.token_type != TokenType::RightBracket {
                loop {
                    let expr = self.expression()?;
                    exprs.push(expr);

                    if !self.matches(vec![TokenType::Comma])
                        || self.curr_token.0.token_type == TokenType::RightBracket
                    {
                        break;
                    }
                }
            }

            self.eat(TokenType::RightBracket, "Expcted ']'")?;
            let span: Span = (token.1.start..self.curr_token.1.start).into();
            return Ok(Expression::create_literal(Literal::Array(exprs), span));
        }

        if self.matches(vec![TokenType::LeftParen]) {
            let expr = self.expression()?;
            self.eat(TokenType::RightParen, "Unterminated grouping expression")?;
            let span: Span = (token.1.start..self.prev_token.1.start).into();
            return Ok(Expression::create_grouping(expr, span));
        }

        Err(ParserError::UnexpectedToken(self.curr_token.clone()))
    }

    fn parse_string(&mut self) -> ParserResult {
        self.scanner.set_mode(ScannerMode::String);
        let quote = self.curr_token.clone();
        let mut out = vec![];

        loop {
            match self.peek().token_type {
                TokenType::DollarSign | TokenType::LeftParen => {
                    self.scanner.set_mode(ScannerMode::Default);
                    self.advance(); // advance to '$' token
                    let dollar_sign = self.curr_token.clone();
                    self.advance(); // advance to '(' token
                    self.advance(); // current token should be parsed as

                    // return empty string if interpolation is empty
                    if self.check(TokenType::RightParen) {
                        continue;
                    }

                    if self.curr_token.0.token_type == TokenType::DoubleQuote {
                        return Err(ParserError::UnterminatedInterpolation(dollar_sign));
                    }

                    out.push(self.expression()?);

                    if self.check(TokenType::RightParen) {
                        self.advance();
                    } else {
                        return Err(ParserError::UnterminatedInterpolation(dollar_sign));
                    };
                    self.scanner.backtrack();
                    self.scanner.set_mode(ScannerMode::String);
                    continue;
                }
                _ => {}
            }

            self.advance();
            match self.curr_token.0.token_type {
                TokenType::StringConst => {
                    out.push(Expression::create_literal(
                        Literal::String(self.curr_token.0.value.to_string()),
                        (0..0).into(),
                    ));
                }
                TokenType::DoubleQuote => {
                    break;
                }
                _ => {
                    self.scanner.set_mode(ScannerMode::Default);
                    return Err(ParserError::UnterminatedString(quote));
                }
            }
        }
        self.scanner.set_mode(ScannerMode::Default);
        self.advance(); // advance over '"'

        let out_len = out.len();
        let span: Span = (quote.1.start..self.prev_token.1.end).into();
        if out_len == 0 {
            // return a empty string if there is no output
            return Ok(Expression::create_literal(
                Literal::String("".to_string()),
                span,
            ));
        } else if out_len == 1 && out[0].is_string_lit() {
            // return the only elem in output as string lit
            // no reason to create a binop expression
            return Ok(out[0].clone());
        } else if out_len == 1 {
            // should be an expression other than string literal
            // append to empty string so it gets formatted as string
            return Ok(Expression::create_binop(
                Expression::create_literal(Literal::String("".to_string()), Span::fake()),
                BinaryOperation::ConcatInterpolation,
                out[0].clone(),
                span,
            ));
        }

        // from here on out there should be more than one expression in the output
        let mut expr = Expression::create_binop(
            out[0].clone(),
            BinaryOperation::ConcatInterpolation,
            out[1].clone(),
            span.clone(),
        );

        for e in out[2..].into_iter() {
            expr = Expression::create_binop(
                expr,
                BinaryOperation::ConcatInterpolation,
                e.clone(),
                span.clone(),
            )
        }

        Ok(expr)
    }
}

#[cfg(test)]
mod test {

    use crate::ParserError;

    use super::Parser;
    use ast::{
        BinaryOperation, DataStructInstanceField, EType, Expression, Identifier, Literal,
        LogicOperation, Program, Type, UnaryOperation,
    };
    use scanner::{Scanner, Token, TokenType};
    use span_util::{Span, WithSpan};

    fn run_parser(source: &str) -> (Program, Vec<ParserError>) {
        let scanner = Scanner::new(source.to_string());
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

    fn parse_error(source: &str, expected_errors: Vec<ParserError>) {
        let (_, errors) = run_parser(source);

        if errors.is_empty() {
            panic!("parser did not emit errors");
        };

        if errors.len() != expected_errors.len() {
            println!("{:#?}", errors);
            panic!(
                "parser emmitted {} errors, expected errors has {} errors",
                errors.len(),
                expected_errors.len()
            );
        }
        for (err, expected) in errors.iter().zip(expected_errors) {
            assert_eq!(*err, expected);
        }
    }

    fn create_token(token: Token) -> WithSpan<Token> {
        WithSpan(token, Span::fake())
    }

    fn create_let(id: &str, value: Option<Expression>) -> Expression {
        Expression::create_let(ident(id), value, Span::fake())
    }

    fn create_let_type(id: Identifier, value: Option<Expression>) -> Expression {
        Expression::create_let(id, value, Span::fake())
    }

    fn int(val: i64) -> Expression {
        Expression::create_literal(Literal::Int(val), Span::fake())
    }

    fn float(val: f64) -> Expression {
        Expression::create_literal(Literal::Float(val), Span::fake())
    }

    fn array(val: Vec<Expression>) -> Expression {
        Expression::create_literal(Literal::Array(val), Span::fake())
    }

    fn null() -> Expression {
        Expression::create_literal(Literal::Null, Span::fake())
    }

    fn bool_lit(val: bool) -> Expression {
        Expression::create_literal(Literal::Bool(val), Span::fake())
    }

    fn string_lit(val: &str) -> Expression {
        Expression::create_literal(Literal::String(val.into()), Span::fake())
    }

    fn ident(val: &str) -> Identifier {
        Identifier::new(val.to_string())
    }

    fn create_type(etype: EType, is_array: bool) -> Type {
        Type::new(etype, is_array)
    }

    fn ident_type(val: &str, value_type: Type) -> Identifier {
        Identifier::with_ident_type(val.into(), Some(value_type))
    }

    fn ident_self() -> Identifier {
        Identifier::with_token_type("self".to_string(), TokenType::SELF)
    }

    fn create_binop(left: Expression, op: BinaryOperation, right: Expression) -> Expression {
        Expression::create_binop(left, op, right, Span::fake())
    }

    fn create_let_ref(val: &str) -> Expression {
        Expression::create_let_ref(Identifier::new(val.to_string()), Span::fake())
    }

    fn create_block(exprs: Vec<Expression>) -> Expression {
        Expression::create_block(exprs, Span::fake())
    }

    fn create_return(val: Option<Expression>) -> Expression {
        Expression::create_return(val, Span::fake())
    }

    fn create_implicit_return(val: Expression) -> Expression {
        Expression::create_implicit_return(val, Span::fake())
    }

    fn create_data_struct(name: &str, fields: Vec<&str>, methods: Vec<Expression>) -> Expression {
        Expression::create_data_struct(
            Identifier::new(name.to_string()),
            fields
                .iter()
                .map(|v| Identifier::new(v.to_string()))
                .collect(),
            methods,
            Span::fake(),
        )
    }

    fn create_data_struct_instance(name: &str, fields: Vec<DataStructInstanceField>) -> Expression {
        Expression::create_data_struct_instance(
            Identifier::new(name.to_string()),
            fields,
            Span::fake(),
        )
    }

    fn data_struct_instance_field(name: &str, value: Expression) -> DataStructInstanceField {
        DataStructInstanceField::new(Identifier::new(name.to_string()), value)
    }

    fn create_assign(name: Identifier, rhs: Expression) -> Expression {
        Expression::create_assign(name, rhs, Span::fake())
    }

    fn create_index(lhs: Expression, index: Expression) -> Expression {
        Expression::create_index(lhs, index, Span::fake())
    }

    fn create_unaryop(op: UnaryOperation, rhs: Expression) -> Expression {
        Expression::create_unaryop(op, rhs, Span::fake())
    }

    fn create_grouping(expr: Expression) -> Expression {
        Expression::create_grouping(expr, Span::fake())
    }

    fn create_logic(lhs: Expression, op: LogicOperation, rhs: Expression) -> Expression {
        Expression::create_logic(lhs, op, rhs, Span::fake())
    }

    fn create_call(callee: Expression, args: Vec<Expression>) -> Expression {
        Expression::create_call(callee, args, Span::fake())
    }

    fn create_self(name: &str) -> Expression {
        Expression::create_self(name.into(), Span::fake())
    }

    fn create_break() -> Expression {
        Expression::create_break(Span::fake())
    }

    fn create_continue() -> Expression {
        Expression::create_continue(Span::fake())
    }

    fn create_loop(
        condition: Expression,
        body: Expression,
        iterator: Option<Expression>,
    ) -> Expression {
        Expression::create_loop(condition, body, iterator, Span::fake())
    }

    fn create_if(
        condition: Expression,
        then: Expression,
        not_then: Option<Expression>,
    ) -> Expression {
        Expression::create_if(condition, then, not_then, Span::fake())
    }

    fn create_set_index(lhs: Expression, index: Expression, value: Expression) -> Expression {
        Expression::create_set_index(lhs, index, value, Span::fake())
    }

    fn create_get_property(object: Expression, name: Identifier, is_callable: bool) -> Expression {
        Expression::create_get_property(object, name, is_callable, Span::fake())
    }

    fn create_set_property(object: Expression, name: Identifier, value: Expression) -> Expression {
        Expression::create_set_property(object, name, value, Span::fake())
    }

    fn create_function(
        name: Option<String>,
        params: Vec<Identifier>,
        is_static: bool,
        body: Expression,
    ) -> Expression {
        Expression::create_function(name, params, is_static, body, Span::fake())
    }

    #[test]
    fn parse_type_let_expr() {
        parse(
            r#"
        let cents: int = 2;
        let pi: float = 3.14;
        let name: string = "test";
        let exists: bool = true;
        "#,
            vec![
                create_let_type(
                    ident_type("cents", create_type(EType::Int, false)),
                    Some(int(2)),
                ),
                create_let_type(
                    ident_type("pi", create_type(EType::Float, false)),
                    Some(float(3.14)),
                ),
                create_let_type(
                    ident_type("name", create_type(EType::String, false)),
                    Some(string_lit("test")),
                ),
                create_let_type(
                    ident_type("exists", create_type(EType::Bool, false)),
                    Some(bool_lit(true)),
                ),
            ],
        );

        parse(
            r#"
        let cents: int[] = [1];
        let pi: float[] = [1.0];
        let name: string[] = ["test"];
        let exists: bool[] = [true];
        "#,
            vec![
                create_let_type(
                    ident_type("cents", create_type(EType::Int, true)),
                    Some(array(vec![int(1)])),
                ),
                create_let_type(
                    ident_type("pi", create_type(EType::Float, true)),
                    Some(array(vec![float(1.0)])),
                ),
                create_let_type(
                    ident_type("name", create_type(EType::String, true)),
                    Some(array(vec![string_lit("test")])),
                ),
                create_let_type(
                    ident_type("exists", create_type(EType::Bool, true)),
                    Some(array(vec![bool_lit(true)])),
                ),
            ],
        );

        parse(
            r#"
        let joe: Person = Person {};
        let family: Person[] = [Person{}];
        "#,
            vec![
                create_let_type(
                    ident_type("joe", create_type(EType::User("Person".into()), false)),
                    Some(create_data_struct_instance("Person".into(), vec![])),
                ),
                create_let_type(
                    ident_type("family", create_type(EType::User("Person".into()), true)),
                    Some(array(vec![create_data_struct_instance(
                        "Person".into(),
                        vec![],
                    )])),
                ),
            ],
        );
    }

    #[test]
    fn parse_type_error_let_expr() {
        parse_error(
            "
            let name;
            let name: +;
            let name: 123 = 2;
            let name: @23432 = 2;
            let name: unit;
            let name: int[ ;
            let name: int] ;
        ",
            vec![
                ParserError::TypeAnnotationNeeded(Token::identifier(
                    "name".into(),
                    Span::fake().into(),
                )),
                ParserError::InvalidType(Token::plus(Span::fake().into())),
                ParserError::InvalidType(Token::int_const("123".into(), Span::fake().into())),
                ParserError::InvalidType(Token::illegal("@".into(), Span::fake().into())),
                ParserError::InvalidUseOfUnitType(Token::unit(Span::fake().into())),
                ParserError::ExpectedToken(
                    "Expected ']', found ';'".into(),
                    Token::semi_colon(Span::fake().into()),
                ),
                ParserError::ExpectedToken(
                    "Expected ';' after expression, found ']'".into(),
                    Token::right_bracket(Span::fake().into()),
                ),
            ],
        )
    }

    #[test]
    // last expression in a block can implicitly return by not including the semicolon
    fn parse_last_expr_in_block_return_on_no_semi() {
        parse(
            "
        {
            let x = 2;
            let y = 3;
            x
        };
        {
            let x = 2;
            let y = 3;
            x;
        };
        { x };
        {x;};
        ",
            vec![
                create_block(vec![
                    create_let("x", Some(int(2))),
                    create_let("y", Some(int(3))),
                    create_implicit_return(create_let_ref("x")),
                ]),
                create_block(vec![
                    create_let("x", Some(int(2))),
                    create_let("y", Some(int(3))),
                    create_let_ref("x"),
                ]),
                create_block(vec![create_implicit_return(create_let_ref("x"))]),
                create_block(vec![create_let_ref("x")]),
            ],
        );
    }

    #[test]
    fn parse_string_interplation() {
        // TODO test parser errors for unterminated string and interpolation
        // "$() -> this returns a error "Expcted ';' after expression" should be unterminated string
        // "
        parse(
            r#"
        "";
        " ";
        "$()";
        "Hello";
        "Hello $(name), how was your day?";
        "$(name)";
        "$(name) Hello";
        "Hello $(if true { "world"; } else { "mars"; })";
        "Hello $(person.name)";
        "#,
            vec![
                string_lit(""),
                string_lit(" "),
                string_lit(""),
                string_lit("Hello"),
                create_binop(
                    create_binop(
                        string_lit("Hello "),
                        BinaryOperation::ConcatInterpolation,
                        create_let_ref("name"),
                    ),
                    BinaryOperation::ConcatInterpolation,
                    string_lit(", how was your day?"),
                ),
                create_binop(
                    string_lit(""),
                    BinaryOperation::ConcatInterpolation,
                    create_let_ref("name"),
                ),
                create_binop(
                    create_let_ref("name"),
                    BinaryOperation::ConcatInterpolation,
                    string_lit(" Hello"),
                ),
                create_binop(
                    string_lit("Hello "),
                    BinaryOperation::ConcatInterpolation,
                    create_if(
                        bool_lit(true),
                        create_block(vec![string_lit("world")]),
                        Some(create_block(vec![string_lit("mars")])),
                    ),
                ),
                create_binop(
                    string_lit("Hello "),
                    BinaryOperation::ConcatInterpolation,
                    create_get_property(create_let_ref("person"), ident("name"), false),
                ),
            ],
        );
    }

    #[test]
    fn parse_loop_for_expr() {
        parse(
            "
        loop i in name {};
        ",
            vec![create_loop(
                create_let("i", None),
                create_block(vec![]),
                Some(create_let_ref("name")),
            )],
        );

        parse(
            "
        loop i in (Person {}) {};
        ",
            vec![create_loop(
                create_let("i", None),
                create_block(vec![]),
                Some(create_grouping(create_data_struct_instance(
                    "Person",
                    vec![],
                ))),
            )],
        );
    }

    #[test]
    fn parse_number_literal() {
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
                int(1),
                int(2),
                int(39),
                int(2347682384),
                float(34.6),
                float(22.732487656),
                float(2.0),
                float(0.),
            ],
        )
    }

    #[test]
    fn parse_bool_literal() {
        parse("true; false;", vec![bool_lit(true), bool_lit(false)]);
    }

    #[test]
    fn parse_string_literal() {
        parse(
            r#""this is a string"; "string";"#,
            vec![string_lit("this is a string"), string_lit("string")],
        );
    }

    #[test]
    fn parse_array_literal() {
        parse(
            r#"
            [1, 3, 4, true, false, "string", 2.0, null];
            [{}];
            [1,];
            "#,
            vec![
                array(vec![
                    int(1),
                    int(3),
                    int(4),
                    bool_lit(true),
                    bool_lit(false),
                    string_lit("string"),
                    float(2.0),
                    null(),
                ]),
                array(vec![create_block(vec![])]),
                array(vec![int(1)]),
            ],
        );
    }

    #[test]
    fn parse_null_literal() {
        parse("null;", vec![null()]);
    }

    #[test]
    fn parse_self_keyword() {
        parse(
            "let main = fun { self; };",
            vec![create_let(
                "main",
                Some(create_function(
                    Some("main".to_string()),
                    vec![],
                    true,
                    create_block(vec![create_self("self")]),
                )),
            )],
        );
    }

    #[test]
    fn parse_let_stmt() {
        parse(
            "let num: int; let _num = 1;",
            vec![
                create_let_type(ident_type("num", Type::new(EType::Int, false)), None),
                create_let("_num", Some(int(1))),
            ],
        );
    }

    #[test]
    fn parse_identifier() {
        parse(
            "num; num2;",
            vec![create_let_ref("num"), create_let_ref("num2")],
        );
    }

    #[test]
    fn parse_assignment_expr() {
        parse(
            "
            num = 1;
            num += 1;
            num -= 1;
            num *= 1;
            num /= 1;
 
            ",
            vec![
                create_assign(ident("num"), int(1)),
                create_assign(
                    ident("num"),
                    create_binop(create_let_ref("num"), BinaryOperation::Add, int(1)),
                ),
                create_assign(
                    ident("num"),
                    create_binop(create_let_ref("num"), BinaryOperation::Substract, int(1)),
                ),
                create_assign(
                    ident("num"),
                    create_binop(create_let_ref("num"), BinaryOperation::Multiply, int(1)),
                ),
                create_assign(
                    ident("num"),
                    create_binop(create_let_ref("num"), BinaryOperation::Divide, int(1)),
                ),
            ],
        );
    }

    #[test]
    fn parse_logic_expr() {
        parse(
            "1 && 2; 2 || 1;",
            vec![
                create_logic(int(1), LogicOperation::And, int(2)),
                create_logic(int(2), LogicOperation::Or, int(1)),
            ],
        );
    }

    #[test]
    fn parse_equality_expr() {
        parse(
            "1 == 2; 2 != 1;",
            vec![
                create_logic(int(1), LogicOperation::Equal, int(2)),
                create_logic(int(2), LogicOperation::NotEqual, int(1)),
            ],
        );
    }

    #[test]
    fn parse_comparison_expr() {
        parse(
            "1 < 2; 2 <= 1;3 > 1; 3 >= 1;",
            vec![
                create_logic(int(1), LogicOperation::LessThan, int(2)),
                create_logic(int(2), LogicOperation::LessThanEqual, int(1)),
                create_logic(int(3), LogicOperation::GreaterThan, int(1)),
                create_logic(int(3), LogicOperation::GreaterThanEqual, int(1)),
            ],
        );
    }

    #[test]
    fn parse_binop_expr() {
        parse(
            "1 + 2 * 3 / 2;",
            vec![create_binop(
                int(1),
                BinaryOperation::Add,
                create_binop(
                    create_binop(int(2), BinaryOperation::Multiply, int(3)),
                    BinaryOperation::Divide,
                    int(2),
                ),
            )],
        );
    }

    #[test]
    fn parse_grouping_expr() {
        parse(
            "2 * (2 + 1);",
            vec![create_binop(
                int(2),
                BinaryOperation::Multiply,
                create_grouping(create_binop(int(2), BinaryOperation::Add, int(1))),
            )],
        );
    }

    #[test]
    fn parse_unary_expr() {
        parse(
            "-1; 1 + -1; !1;",
            vec![
                create_unaryop(UnaryOperation::Minus, int(1)),
                create_binop(
                    int(1),
                    BinaryOperation::Add,
                    create_unaryop(UnaryOperation::Minus, int(1)),
                ),
                create_unaryop(UnaryOperation::Not, int(1)),
            ],
        );
    }

    #[test]
    fn parse_expression_stmt() {
        parse("123; 23;", vec![int(123), int(23)]);
    }

    #[test]
    fn parse_array_access() {
        parse(
            "array[0]; [1,2,3][2]; [[1], 2][0][0];",
            vec![
                create_index(create_let_ref("array"), int(0)),
                create_index(array(vec![int(1), int(2), int(3)]), int(2)),
                create_index(
                    create_index(array(vec![array(vec![int(1)]), int(2)]), int(0)),
                    int(0),
                ),
            ],
        )
    }

    #[test]
    fn parse_array_index_assignment() {
        parse(
            "array[0] = 1;",
            vec![create_set_index(create_let_ref("array"), int(0), int(1))],
        );

        parse(
            "
            array[0] += 1;
            array[0] -= 1;
            array[0] *= 1;
            array[0] /= 1;
            ",
            vec![
                create_set_index(
                    create_let_ref("array"),
                    int(0),
                    create_binop(
                        create_index(create_let_ref("array"), int(0)),
                        BinaryOperation::Add,
                        int(1),
                    ),
                ),
                create_set_index(
                    create_let_ref("array"),
                    int(0),
                    create_binop(
                        create_index(create_let_ref("array"), int(0)),
                        BinaryOperation::Substract,
                        int(1),
                    ),
                ),
                create_set_index(
                    create_let_ref("array"),
                    int(0),
                    create_binop(
                        create_index(create_let_ref("array"), int(0)),
                        BinaryOperation::Multiply,
                        int(1),
                    ),
                ),
                create_set_index(
                    create_let_ref("array"),
                    int(0),
                    create_binop(
                        create_index(create_let_ref("array"), int(0)),
                        BinaryOperation::Divide,
                        int(1),
                    ),
                ),
            ],
        );
    }

    #[test]
    fn parse_function_call() {
        parse(
            "name(); this_is_a_func(); fnc(1, name, true);",
            vec![
                create_call(create_let_ref("name"), vec![]),
                create_call(create_let_ref("this_is_a_func"), vec![]),
                create_call(
                    create_let_ref("fnc"),
                    vec![int(1), create_let_ref("name"), bool_lit(true)],
                ),
            ],
        );

        parse(
            "fun (){}();",
            vec![create_call(
                create_function(None, vec![], true, create_block(vec![])),
                vec![],
            )],
        );
    }

    #[test]
    fn parse_return_expr() {
        parse(
            "
                let main = fun {
                    return; 
                    return 1;
                };
                let main = fun() => 1;
            ",
            vec![
                create_let(
                    "main",
                    Some(create_function(
                        Some("main".to_string()),
                        vec![],
                        true,
                        create_block(vec![create_return(None), create_return(Some(int(1)))]),
                    )),
                ),
                create_let(
                    "main",
                    Some(create_function(
                        Some("main".to_string()),
                        vec![],
                        true,
                        create_block(vec![create_implicit_return(int(1))]),
                    )),
                ),
            ],
        )
    }

    #[test]
    fn parse_function_expr() {
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
                create_function(None, vec![], true, create_block(vec![])),
                create_function(None, vec![], true, create_block(vec![])),
                create_function(
                    None,
                    vec![
                        Identifier::new("x".to_string()),
                        Identifier::new("y".to_string()),
                    ],
                    true,
                    create_block(vec![]),
                ),
                create_function(
                    None,
                    vec![
                        Identifier::new("y".to_string()),
                        Identifier::new("z".to_string()),
                    ],
                    true,
                    create_block(vec![create_let("name", Some(int(2))), int(123)]),
                ),
            ],
        )
    }

    #[test]
    fn parse_if_expr() {
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
            if false { x = 1 } else  { x = 2 };
            if val { 1 } else { 2 };
            ",
            vec![
                create_if(bool_lit(true), create_block(vec![]), None),
                create_if(bool_lit(true), create_block(vec![]), None),
                create_if(bool_lit(false), create_block(vec![]), None),
                create_if(
                    bool_lit(false),
                    create_block(vec![]),
                    Some(create_block(vec![])),
                ),
                create_if(
                    int(123),
                    create_block(vec![create_let("x", Some(int(1)))]),
                    Some(create_block(vec![create_let("y", Some(int(2)))])),
                ),
                create_if(
                    bool_lit(false),
                    create_block(vec![create_implicit_return(create_assign(
                        ident("x"),
                        int(1),
                    ))]),
                    Some(create_block(vec![create_implicit_return(create_assign(
                        ident("x"),
                        int(2),
                    ))])),
                ),
                create_if(
                    create_let_ref("val"),
                    create_block(vec![create_implicit_return(int(1))]),
                    Some(create_block(vec![create_implicit_return(int(2))])),
                ),
            ],
        );
    }

    #[test]
    fn parse_block_expr() {
        parse(
            "
            {};
            let b = {
                let x = 1;
            };
            let x = { 1; 2; 3; 4; 5; 3345 };
            ",
            vec![
                create_block(vec![]),
                create_let("b", Some(create_block(vec![create_let("x", Some(int(1)))]))),
                create_let(
                    "x",
                    Some(create_block(vec![
                        int(1),
                        int(2),
                        int(3),
                        int(4),
                        int(5),
                        create_implicit_return(int(3345)),
                    ])),
                ),
            ],
        );
    }

    #[test]
    fn parse_data_struct_expr() {
        parse(
            "
            data Person {};
        ",
            vec![create_data_struct("Person", vec![], vec![])],
        );

        parse(
            "
            data Person {
                first_name,
            };
        ",
            vec![create_data_struct("Person", vec!["first_name"], vec![])],
        );

        parse(
            "
            data Person {
                first_name,
                last_name,
                age
            };
        ",
            vec![create_data_struct(
                "Person",
                vec!["first_name", "last_name", "age"],
                vec![],
            )],
        );

        parse(
            "
            data Person {
                first_name,
                last_name,
                age,
            };
        ",
            vec![create_data_struct(
                "Person",
                vec!["first_name", "last_name", "age"],
                vec![],
            )],
        );
    }

    #[test]
    fn parse_data_struct_methods_expr() {
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
            vec![create_data_struct(
                "Person",
                vec!["first_name"],
                vec![
                    create_function(Some("new".to_string()), vec![], true, create_block(vec![])),
                    create_function(
                        Some("name".to_string()),
                        vec![Identifier::with_token_type(
                            "self".to_string(),
                            TokenType::SELF,
                        )],
                        false,
                        create_block(vec![]),
                    ),
                ],
            )],
        );
    }

    #[test]
    fn parse_data_struct_methods_self_expr() {
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
            vec![create_data_struct(
                "Person",
                vec!["first_name"],
                vec![
                    create_function(
                        Some("name".to_string()),
                        vec![ident_self()],
                        false,
                        create_block(vec![create_get_property(
                            create_self("self"),
                            Identifier::new("first_name".to_string()),
                            false,
                        )]),
                    ),
                    create_function(
                        Some("set_name".to_string()),
                        vec![ident_self(), ident("name")],
                        false,
                        create_block(vec![create_set_property(
                            create_self("self"),
                            ident("first_name"),
                            create_let_ref("name"),
                        )]),
                    ),
                ],
            )],
        );
    }

    #[test]
    fn parse_data_struct_instantiate_expr() {
        parse(
            "
            data Person {};
            Person {};
        ",
            vec![
                create_data_struct("Person", vec![], vec![]),
                create_data_struct_instance("Person", vec![]),
            ],
        );

        parse(
            r#"
            data Person {first_name, last_name, age};
            Person { first_name: "john", last_name: "doe", age: 23 };
        "#,
            vec![
                create_data_struct("Person", vec!["first_name", "last_name", "age"], vec![]),
                create_data_struct_instance(
                    "Person",
                    vec![
                        data_struct_instance_field("first_name", string_lit("john")),
                        data_struct_instance_field("last_name", string_lit("doe")),
                        data_struct_instance_field("age", int(23)),
                    ],
                ),
            ],
        );

        parse(
            "Person {id: 1}.id;",
            vec![create_get_property(
                create_data_struct_instance(
                    "Person",
                    vec![data_struct_instance_field("id", int(1))],
                ),
                ident("id"),
                false,
            )],
        );
    }

    #[test]
    fn parse_data_struct_instantiate_shorthand_expr() {
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
                create_data_struct("Person", vec!["id", "first_name"], vec![]),
                create_let("id", Some(int(123))),
                create_let("first_name", Some(string_lit("John"))),
                create_data_struct_instance(
                    "Person",
                    vec![
                        data_struct_instance_field("id", create_let_ref("id")),
                        data_struct_instance_field("first_name", create_let_ref("first_name")),
                    ],
                ),
                create_data_struct_instance(
                    "Person",
                    vec![
                        data_struct_instance_field("id", create_let_ref("id")),
                        data_struct_instance_field("first_name", create_let_ref("first_name")),
                    ],
                ),
            ],
        );
    }

    #[test]
    fn parse_get_property_expr() {
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
                create_data_struct("Person", vec!["first_name", "last_name", "age"], vec![]),
                create_let("p", Some(create_data_struct_instance("Person", vec![]))),
                create_get_property(
                    create_let_ref("p"),
                    Identifier::new("first_name".to_string()),
                    false,
                ),
            ],
        );
    }

    #[test]
    fn parse_set_property_expr() {
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
                create_data_struct("Person", vec!["first_name", "last_name", "age"], vec![]),
                create_let("p", Some(create_data_struct_instance("Person", vec![]))),
                create_set_property(create_let_ref("p"), ident("age"), int(1)),
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
                create_data_struct("Person", vec!["first_name", "last_name", "age"], vec![]),
                create_let("p", Some(create_data_struct_instance("Person", vec![]))),
                create_set_property(
                    create_let_ref("p"),
                    ident("age"),
                    create_binop(
                        create_get_property(create_let_ref("p"), ident("age"), false),
                        BinaryOperation::Add,
                        int(1),
                    ),
                ),
                create_set_property(
                    create_let_ref("p"),
                    ident("age"),
                    create_binop(
                        create_get_property(create_let_ref("p"), ident("age"), false),
                        BinaryOperation::Substract,
                        int(1),
                    ),
                ),
                create_set_property(
                    create_let_ref("p"),
                    ident("age"),
                    create_binop(
                        create_get_property(create_let_ref("p"), ident("age"), false),
                        BinaryOperation::Multiply,
                        int(1),
                    ),
                ),
                create_set_property(
                    create_let_ref("p"),
                    ident("age"),
                    create_binop(
                        create_get_property(create_let_ref("p"), ident("age"), false),
                        BinaryOperation::Divide,
                        int(1),
                    ),
                ),
            ],
        );
    }

    #[test]
    fn parse_loop_expr() {
        parse(
            "
            loop {}; 
            loop true {}; 
            loop 1 < 2 {};
            ",
            vec![
                create_loop(bool_lit(true), create_block(vec![]), None),
                create_loop(bool_lit(true), create_block(vec![]), None),
                create_loop(
                    create_logic(int(1), LogicOperation::LessThan, int(2)),
                    create_block(vec![]),
                    None,
                ),
            ],
        )
    }

    #[test]
    fn parse_break_continue_expr() {
        parse(
            "
        loop { break; }; 
        loop { continue; }; 
        ",
            vec![
                create_loop(bool_lit(true), create_block(vec![create_break()]), None),
                create_loop(bool_lit(true), create_block(vec![create_continue()]), None),
            ],
        )
    }
}
