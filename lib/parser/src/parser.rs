use crate::ParserError;
use ast::{
    BinaryOperation, DataStructInstanceField, Expression, Identifier, Literal, LogicOperation,
    Program, Type, UnaryOperation,
};
use scanner::{Scanner, ScannerMode, Token, TokenType};
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
    Method(String),
}

type ParserResult = Result<Expression, ParserError>;

#[derive(Debug)]
pub struct Parser {
    scanner: Scanner,
    prev_token: Token,
    curr_token: Token,
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
        self.peek().token_type == token_type
    }

    fn peek(&mut self) -> Token {
        let token = self.scanner.next_token();
        self.scanner.backtrack();
        token
    }

    fn advance(&mut self) -> Token {
        self.prev_token = self.curr_token.clone();
        self.curr_token = self.scanner.next_token();
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

        let msg = format!("{}, found '{}'", msg.to_string(), tok);
        Err(ParserError::ExpectedToken(msg, tok))
    }

    fn eat_optional(&mut self, token_type: TokenType) -> Option<Token> {
        if self.curr_token.token_type == token_type {
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

    fn parse_parameters(&mut self, token_end: TokenType) -> Result<Vec<Identifier>, ParserError> {
        let mut params = vec![];
        if self.curr_token.token_type != token_end {
            loop {
                if self.check(TokenType::SELF) {
                    return Err(ParserError::Error(
                        "Expected 'self' to be the first parameter in a method".to_string(),
                        self.curr_token.clone(),
                    ));
                }

                let ident = self.eat(TokenType::Identifier, "Expected 'identifier'")?;

                let ident = if self.eat_optional(TokenType::Colon).is_none() {
                    // return Err(ParserError::TypeAnnotationNeeded(ident));
                    Identifier::new(ident.value, ident.span)
                } else {
                    let ttype = self.parse_type(false)?;
                    Identifier::with_value_type(ident.value, Some(ttype), ident.span)
                };
                // let ttype = self.parse_type(false)?;

                params.push(ident);

                if !self.matches(vec![TokenType::Comma]) || self.curr_token.token_type == token_end
                {
                    break;
                }
            }
        }
        Ok(params)
    }

    fn parse_type(&mut self, allow_unit: bool) -> Result<Type, ParserError> {
        let token = self.curr_token.clone();

        let ttype = if self.matches(vec![TokenType::LeftParen]) {
            let ttype = self.parse_type(false)?;
            self.eat(TokenType::RightParen, "Expected ')'")?;
            Some(ttype)
        } else if self.matches(vec![
            TokenType::Int,
            TokenType::Float,
            TokenType::String,
            TokenType::Bool,
            TokenType::Identifier,
        ]) {
            let ttype = match &token.token_type {
                TokenType::Int => Type::int(),
                TokenType::Float => Type::float(),
                TokenType::String => Type::string(),
                TokenType::Bool => Type::bool(),
                TokenType::Identifier => Type::identifier(token.value.to_string()),
                _ => unreachable!(),
            };
            Some(ttype)
        } else if self.matches(vec![TokenType::FunType]) {
            self.eat(TokenType::LeftParen, "Expected '('")?;
            let params = self.parse_parameters(TokenType::RightParen)?;
            self.eat(TokenType::RightParen, "Expected ')'")?;
            let mut return_type = Type::unit();
            if self.eat_optional(TokenType::Colon).is_some() {
                return_type = self.parse_type(true)?;
            }
            Some(Type::fun(params, return_type))
        } else {
            None
        };

        if self.matches(vec![TokenType::Unit]) {
            // we can allow unit to be used in function return type
            // using unit in other context is forbidden,
            // there should be a Option type like in rust
            // maybe an built in enum type
            if allow_unit {
                return Ok(Type::unit());
            }
            return Err(ParserError::InvalidUseOfUnitType(token));
        }

        match ttype {
            Some(t) => {
                if self.matches(vec![TokenType::LeftBracket]) {
                    self.eat(TokenType::RightBracket, "Expected ']'")?;
                    return Ok(Type::array(t));
                }
                return Ok(t);
            }
            None => {}
        }

        Err(ParserError::InvalidType(token))
    }

    fn let_expression(&mut self) -> ParserResult {
        let start_span: Span = self.prev_token.span.clone();
        let ident = self.curr_token.clone();
        let identifier = self.eat(TokenType::Identifier, "Expected identifier")?;

        let has_colon = self.eat_optional(TokenType::Colon);

        let itype = match has_colon {
            Some(_) => Some(self.parse_type(false)?),
            None => None,
        };

        let mut init = None;

        if self.matches(vec![TokenType::Assign]) {
            let expr = match self.expression() {
                Ok(expr) => match expr {
                    Expression::Function(fun) => Expression::create_function(
                        Some(identifier.value.to_string()),
                        fun.params,
                        fun.return_type,
                        fun.is_static,
                        *fun.body,
                        fun.span,
                    ),
                    _ => expr,
                },
                Err(err) => match err {
                    ParserError::UnexpectedToken(_) => {
                        return Err(ParserError::ExpectedToken(
                            format!(
                                "Expected expression here, found '{}'",
                                self.curr_token.clone()
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

        let span: Span = start_span.extend(self.curr_token.span.clone());

        Ok(Expression::create_let(
            Identifier::with_value_type(identifier.value, itype, identifier.span),
            init,
            span,
        ))
    }

    fn continue_break_expression(&mut self) -> ParserResult {
        Ok(match self.prev_token.token_type {
            TokenType::Break => Expression::create_break(self.prev_token.span.clone()),
            TokenType::Continue => Expression::create_continue(self.prev_token.span.clone()),
            _ => unreachable!(),
        })
    }

    fn loop_expression(&mut self) -> ParserResult {
        let if_span = self.prev_token.span.clone();
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
        let identifier_span = self.curr_token.span.clone();
        let ident = self.eat(TokenType::Identifier, "Expected identifier")?;
        self.eat(TokenType::LeftBrace, "Expected '{'")?;

        let mut fields = vec![];

        if self.curr_token.token_type != TokenType::RightBrace {
            while !self.curr_token.is_eof() {
                let id_span = self.curr_token.span.clone();
                let id = self.eat(TokenType::Identifier, "Expected identifier")?;

                if self.matches(vec![TokenType::Comma])
                    || self.curr_token.token_type == TokenType::RightBrace
                {
                    let ident = Identifier::new(id.value.to_string(), id.span);
                    fields.push(DataStructInstanceField::new(
                        ident.clone(),
                        Expression::create_let_ref(ident, id_span),
                    ));
                    if self.curr_token.token_type == TokenType::RightBrace {
                        break;
                    }
                    continue;
                }

                self.eat(TokenType::Colon, "Expected ':'")?;
                let expr = self.expression()?;
                let field = DataStructInstanceField::new(
                    Identifier::new(id.value.to_string(), id.span),
                    expr,
                );
                fields.push(field);

                if !self.matches(vec![TokenType::Comma])
                    || self.curr_token.token_type == TokenType::RightBrace
                {
                    break;
                }
            }
        }

        self.eat(TokenType::RightBrace, "Expected '}'")?;
        Ok(Expression::create_data_struct_instance(
            Identifier::new(ident.value.to_string(), ident.span),
            fields,
            identifier_span,
        ))
    }

    fn data_struct_expression(&mut self) -> ParserResult {
        let ident_span = self.curr_token.span.clone();
        let ident = self.eat(TokenType::Identifier, "Expected identifier")?;
        self.eat(TokenType::LeftBrace, "Expected '{'")?;

        let fields = self.parse_parameters(TokenType::RightBrace)?;
        let mut methods = vec![];

        self.eat(TokenType::RightBrace, "Expected '}'")?;

        let double_colon = self.eat_optional(TokenType::ColonColon);

        if double_colon.is_some() {
            self.eat(TokenType::LeftBrace, "Expected '{'")?;

            while !self.check(TokenType::RightBrace) && !self.is_end() {
                self.eat(TokenType::Fun, "Expected 'fun'")?;
                methods.push(self.fun_expression(FunctionKind::Method(ident.value.clone()))?);
            }

            self.eat(TokenType::RightBrace, "Expected '}'")?;
        }

        Ok(Expression::create_data_struct(
            Identifier::new(ident.value.to_string(), ident.span),
            fields,
            methods,
            ident_span,
        ))
    }

    fn return_expression(&mut self) -> ParserResult {
        let mut span: Span = self.prev_token.span.clone();
        let mut value = None;

        if !self.curr_token.is_semi_colon() {
            let expr = self.expression()?;
            span = span.extend(expr.get_span());
            value = Some(expr);
        }

        let span = span.extend(self.curr_token.span.clone());
        Ok(Expression::create_return(value, span))
    }

    fn fun_expression(&mut self, kind: FunctionKind) -> ParserResult {
        let fun_span: Span = self.prev_token.span.clone();
        let mut name = None;
        let mut params = vec![];
        let mut is_static = true;

        if let FunctionKind::Method(_) = kind {
            let ident = self.eat(TokenType::Identifier, "Expected method identifier")?;
            name = Some(ident.value);
        }

        let left_param = self.eat_optional(TokenType::LeftParen);

        if let Some(_) = left_param {
            match (kind, self.check(TokenType::SELF)) {
                (FunctionKind::Method(data_struct_indentifier), true) => {
                    let token = self.eat(TokenType::SELF, "Expected 'self'")?;
                    self.eat_optional(TokenType::Comma);
                    is_static = false;
                    params.push(Identifier::with_all(
                        token.value,
                        token.token_type,
                        Type::Identifier(data_struct_indentifier),
                        token.span,
                    ));
                }
                (FunctionKind::Function, true) => {
                    return Err(ParserError::Error(
                        "Keyword 'self' can only be used in methods not functions".to_string(),
                        self.curr_token.clone(),
                    ))
                }
                _ => {}
            }

            params.append(&mut self.parse_parameters(TokenType::RightParen)?);

            self.eat(TokenType::RightParen, "Expected ')' after parameters")?;
        }

        let return_type = match self.eat_optional(TokenType::Colon) {
            Some(_) => self.parse_type(true)?,
            None => Type::unit(),
        };

        if self.matches(vec![TokenType::Arrow]) {
            let arrow_span: Span = self.prev_token.span.clone();
            let expression = self.expression()?;
            let expr_span = expression.get_span();
            let full_span = fun_span.extend(arrow_span);
            return Ok(Expression::create_function(
                name,
                params,
                return_type,
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

        let span = fun_span.extend(self.prev_token.span.clone());

        self.eat(TokenType::LeftBrace, "Expected '{'")?;
        let block = self.block_expression()?;

        Ok(Expression::create_function(
            name,
            params,
            return_type,
            is_static,
            block,
            span,
        ))
    }

    fn block_expression(&mut self) -> ParserResult {
        let span_left_brace = self.prev_token.span.clone();
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

        if !exprs.is_empty() && !self.prev_token.is_semi_colon() {
            let expr = exprs.pop().unwrap();
            let span = expr.get_span();
            exprs.push(Expression::create_implicit_return(expr, span));
        }

        self.eat(TokenType::RightBrace, "Expected '}'")?;
        Ok(Expression::create_block(exprs, span_left_brace))
    }

    fn if_expression(&mut self) -> ParserResult {
        let if_token = self.prev_token.clone();
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

        Ok(Expression::create_if(
            expr,
            then,
            not_then,
            expr_span,
            if_token.span,
        ))
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
                Expression::LetRef(let_ref) => match assignment_tok.token_type {
                    TokenType::Assign => {
                        return Ok(Expression::create_assign(
                            // TODO pass full let ref expression to lhs of assign
                            // TODO make Identifier an expression in ast
                            let_ref.0.name.clone(),
                            rhs,
                            full_span,
                        ));
                    }
                    _ => {
                        return Ok(Expression::create_assign(
                            let_ref.0.name.clone(),
                            Expression::create_binop(
                                Expression::create_let_ref(
                                    let_ref.0.name.clone(),
                                    rhs_span.clone(),
                                ),
                                BinaryOperation::from_token(assignment_tok.clone()),
                                rhs,
                                rhs_span,
                            ),
                            full_span,
                        ))
                    }
                },
                Expression::GetIndex(idx) => match assignment_tok.token_type {
                    TokenType::Assign => {
                        return Ok(Expression::create_set_index(
                            *idx.lhs.clone(),
                            *idx.index.clone(),
                            rhs,
                            full_span,
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
                                rhs_span,
                            ),
                            full_span,
                        ))
                    }
                },
                Expression::GetProperty(get) => match assignment_tok.token_type {
                    TokenType::Assign => {
                        return Ok(Expression::create_set_property(
                            *get.object.clone(),
                            get.name.clone(),
                            rhs,
                            full_span,
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
            let op = match tok.token_type {
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
            let op = match tok.token_type {
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

            let op = BinaryOperation::from_token(tok);
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
            let op = BinaryOperation::from_token(tok);
            expr = Expression::create_binop(expr, op, rhs, span)
        }
        Ok(expr)
    }

    fn unary(&mut self) -> ParserResult {
        if self.matches(vec![TokenType::Plus, TokenType::Minus, TokenType::Bang]) {
            let tok = self.prev_token.clone();
            let rhs = self.unary()?;
            let span: Span = tok.span.extend(rhs.get_span());
            let op = match tok.token_type {
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
                    None => {
                        return Err(ParserError::UnterminatedFunctionCall(paren));
                    }
                }
                let span: Span = primary_span.extend(self.prev_token.span.clone());
                expr = Expression::create_call(expr, args, span);
            } else if self.matches(vec![TokenType::LeftBracket]) {
                let idx = self.expression()?;

                self.eat(TokenType::RightBracket, "Expected ']'")?;

                expr = Expression::create_index(
                    expr,
                    idx,
                    primary_span.extend(self.prev_token.span.clone()),
                );
            } else if self.matches(vec![TokenType::Dot]) {
                let ident_span = self.curr_token.span.clone();
                let name = self.eat(TokenType::Identifier, "Expected identifier")?;
                let is_callable = self.check(TokenType::LeftParen);
                expr = Expression::create_get_property(
                    expr,
                    Identifier::new(name.value.to_string(), ident_span.clone()),
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
        let span: Span = token.span.clone();
        if self.matches(vec![TokenType::IntConst]) {
            let val: i64 = match token.value.parse() {
                Ok(v) => v,
                Err(msg) => panic!("not a integer {}", msg),
            };
            return Ok(Expression::create_literal(Literal::Int(val), span));
        }

        if self.matches(vec![TokenType::True, TokenType::False]) {
            let bool = match token.token_type {
                TokenType::True => true,
                _ => false,
            };

            return Ok(Expression::create_literal(Literal::Bool(bool), span));
        }

        if self.matches(vec![TokenType::FloatConst]) {
            let val: f64 = match token.value.parse() {
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
                Literal::String(token.value),
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
            //  there should be a better solution: what if we implement pattern matching / destructuring
            //  let Person { name } = person; // will be parsed as data struct instantiation
            //  match person {
            //      Person { name } => name,
            //  }
            //  In the match expression the parser would try to parse the first match arm as
            //  a data struct instantiation
            //
            //  Possible solution is to use the :: token for instantiation
            //  e.g: Person :: { id: 1 };
            && self.prev_token.token_type != TokenType::In
            && self.prev_token.token_type != TokenType::If
            && self.prev_token.token_type != TokenType::Loop
        {
            return self.data_struct_instantiate();
        }

        if self.matches(vec![TokenType::Identifier]) {
            return Ok(Expression::create_let_ref(
                Identifier::new(token.value.to_string(), token.span),
                span,
            ));
        }

        if self.matches(vec![TokenType::Fun]) {
            return self.fun_expression(FunctionKind::Function);
        }

        if self.matches(vec![TokenType::SELF]) {
            return Ok(Expression::create_self(token.value, token.span));
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
            let span: Span = (token.span.start..self.curr_token.span.start).into();
            return Ok(Expression::create_literal(Literal::Array(exprs), span));
        }

        if self.matches(vec![TokenType::LeftParen]) {
            let expr = self.expression()?;
            self.eat(TokenType::RightParen, "Unterminated grouping expression")?;
            let span: Span = (token.span.start..self.prev_token.span.start).into();
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

                    if self.curr_token.token_type == TokenType::DoubleQuote {
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
            match self.curr_token.token_type {
                TokenType::StringConst => {
                    out.push(Expression::create_literal(
                        Literal::String(self.curr_token.value.to_string()),
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
        let span: Span = (quote.span.start..self.prev_token.span.end).into();
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
        BinaryOperation, DataStructInstanceField, Expression, Identifier, Literal, LogicOperation,
        Program, Type, UnaryOperation,
    };
    use scanner::{Scanner, Token, TokenType};
    use span_util::Span;

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
        Identifier::new(val.to_string(), Span::fake())
    }

    fn ident_type(val: &str, value_type: Type) -> Identifier {
        Identifier::with_value_type(val.into(), Some(value_type), Span::fake())
    }

    fn ident_self(r#type: &str) -> Identifier {
        Identifier {
            value: "self".to_string(),
            token_type: Some(TokenType::SELF),
            value_type: Some(Type::Identifier(r#type.into())),
            span: Span::fake(),
        }
    }

    fn create_binop(left: Expression, op: BinaryOperation, right: Expression) -> Expression {
        Expression::create_binop(left, op, right, Span::fake())
    }

    fn create_let_ref(val: &str) -> Expression {
        Expression::create_let_ref(Identifier::new(val.to_string(), Span::fake()), Span::fake())
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

    fn create_data_struct(
        name: &str,
        fields: Vec<(&str, Type)>,
        methods: Vec<Expression>,
    ) -> Expression {
        Expression::create_data_struct(
            Identifier::new(name.to_string(), Span::fake()),
            fields
                .iter()
                .map(|v| ident_type(v.0, v.1.clone()))
                .collect(),
            methods,
            Span::fake(),
        )
    }

    fn create_data_struct_instance(name: &str, fields: Vec<DataStructInstanceField>) -> Expression {
        Expression::create_data_struct_instance(
            Identifier::new(name.to_string(), Span::fake()),
            fields,
            Span::fake(),
        )
    }

    fn data_struct_instance_field(name: &str, value: Expression) -> DataStructInstanceField {
        DataStructInstanceField::new(Identifier::new(name.to_string(), Span::fake()), value)
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
        Expression::create_if(condition, then, not_then, Span::fake(), Span::fake())
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
        return_type: Type,
        is_static: bool,
        body: Expression,
    ) -> Expression {
        Expression::create_function(name, params, return_type, is_static, body, Span::fake())
    }

    fn binary_add() -> BinaryOperation {
        BinaryOperation::Add(Span::fake())
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
                create_let_type(ident_type("cents", Type::int()), Some(int(2))),
                create_let_type(ident_type("pi", Type::float()), Some(float(3.14))),
                create_let_type(ident_type("name", Type::string()), Some(string_lit("test"))),
                create_let_type(ident_type("exists", Type::Bool), Some(bool_lit(true))),
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
                    ident_type("cents", Type::array(Type::int())),
                    Some(array(vec![int(1)])),
                ),
                create_let_type(
                    ident_type("pi", Type::array(Type::float())),
                    Some(array(vec![float(1.0)])),
                ),
                create_let_type(
                    ident_type("name", Type::array(Type::string())),
                    Some(array(vec![string_lit("test")])),
                ),
                create_let_type(
                    ident_type("exists", Type::array(Type::bool())),
                    Some(array(vec![bool_lit(true)])),
                ),
            ],
        );
    }

    #[test]
    fn parse_type_fun() {
        parse(
            "
            let x: Fun(a: string);
            let x: Fun();
            let x: Fun(): unit;
            let x: Fun(): string;
            let x: Fun(a: string): int;
            let x: Fun(a: string,): int[];
            let x: (Fun(b: int): int)[];
            let x: Fun(b: int)[];
        ",
            vec![
                create_let_type(
                    ident_type(
                        "x",
                        Type::fun(vec![ident_type("a".into(), Type::string())], Type::unit()),
                    ),
                    None,
                ),
                create_let_type(ident_type("x", Type::fun(vec![], Type::unit())), None),
                create_let_type(ident_type("x", Type::fun(vec![], Type::unit())), None),
                create_let_type(ident_type("x", Type::fun(vec![], Type::string())), None),
                create_let_type(
                    ident_type(
                        "x",
                        Type::fun(vec![ident_type("a".into(), Type::string())], Type::int()),
                    ),
                    None,
                ),
                create_let_type(
                    ident_type(
                        "x",
                        Type::fun(
                            vec![ident_type("a".into(), Type::string())],
                            Type::array(Type::int()),
                        ),
                    ),
                    None,
                ),
                create_let_type(
                    ident_type(
                        "x",
                        Type::array(Type::fun(
                            vec![ident_type("b".into(), Type::int())],
                            Type::int(),
                        )),
                    ),
                    None,
                ),
                create_let_type(
                    ident_type(
                        "x",
                        Type::array(Type::fun(
                            vec![ident_type("b".into(), Type::int())],
                            Type::unit(),
                        )),
                    ),
                    None,
                ),
            ],
        );
    }

    #[test]
    fn parse_type_user_type() {
        parse(
            r#"
        let joe: Person = Person {};
        let family: Person[] = [Person{}];
        let x: Fun(x: Person);
        "#,
            vec![
                create_let_type(
                    ident_type("joe", Type::identifier("Person")),
                    Some(create_data_struct_instance("Person".into(), vec![])),
                ),
                create_let_type(
                    ident_type("family", Type::array(Type::Identifier("Person".into()))),
                    Some(array(vec![create_data_struct_instance(
                        "Person".into(),
                        vec![],
                    )])),
                ),
                create_let_type(
                    ident_type(
                        "x",
                        Type::fun(
                            vec![ident_type("x".into(), Type::identifier("Person"))],
                            Type::unit(),
                        ),
                    ),
                    None,
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
    fn parse_type_error_fun() {
        parse_error(
            "
        let x: Fun;
        let x: Fun(;
        let x: Fun): int = 2;
        let x: Fun(): = 2;
        let x: Fun(a: unit): int = 2;
        ",
            vec![
                ParserError::ExpectedToken(
                    "Expected '(', found ';'".into(),
                    Token::semi_colon(Span::fake().into()),
                ),
                ParserError::ExpectedToken(
                    "Expected 'identifier', found ';'".into(),
                    Token::semi_colon(Span::fake().into()),
                ),
                ParserError::ExpectedToken(
                    "Expected '(', found ')'".into(),
                    Token::right_paren(Span::fake().into()),
                ),
                ParserError::InvalidType(Token::assign(Span::fake().into())),
                ParserError::InvalidUseOfUnitType(Token::unit(Span::fake().into())),
            ],
        );
    }

    #[test]
    fn parse_function_error() {
        parse_error(
            "
        fun(a): int {};
        fun(a: 123) {};
        fun(a: unit) {};
        ",
            vec![
                ParserError::InvalidType(Token::int_const("123".into(), Span::fake().into())),
                ParserError::InvalidUseOfUnitType(Token::unit(Span::fake().into())),
            ],
        );
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
                    Type::unit(),
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
                create_let_type(ident_type("num", Type::int()), None),
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
                    create_binop(
                        create_let_ref("num"),
                        BinaryOperation::Add(Span::fake()),
                        int(1),
                    ),
                ),
                create_assign(
                    ident("num"),
                    create_binop(
                        create_let_ref("num"),
                        BinaryOperation::Substract(Span::fake()),
                        int(1),
                    ),
                ),
                create_assign(
                    ident("num"),
                    create_binop(
                        create_let_ref("num"),
                        BinaryOperation::Multiply(Span::fake()),
                        int(1),
                    ),
                ),
                create_assign(
                    ident("num"),
                    create_binop(
                        create_let_ref("num"),
                        BinaryOperation::Divide(Span::fake()),
                        int(1),
                    ),
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
                BinaryOperation::Add(Span::fake()),
                create_binop(
                    create_binop(int(2), BinaryOperation::Multiply(Span::fake()), int(3)),
                    BinaryOperation::Divide(Span::fake()),
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
                BinaryOperation::Multiply(Span::fake()),
                create_grouping(create_binop(
                    int(2),
                    BinaryOperation::Add(Span::fake()),
                    int(1),
                )),
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
                    BinaryOperation::Add(Span::fake()),
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
                        BinaryOperation::Add(Span::fake()),
                        int(1),
                    ),
                ),
                create_set_index(
                    create_let_ref("array"),
                    int(0),
                    create_binop(
                        create_index(create_let_ref("array"), int(0)),
                        BinaryOperation::Substract(Span::fake()),
                        int(1),
                    ),
                ),
                create_set_index(
                    create_let_ref("array"),
                    int(0),
                    create_binop(
                        create_index(create_let_ref("array"), int(0)),
                        BinaryOperation::Multiply(Span::fake()),
                        int(1),
                    ),
                ),
                create_set_index(
                    create_let_ref("array"),
                    int(0),
                    create_binop(
                        create_index(create_let_ref("array"), int(0)),
                        BinaryOperation::Divide(Span::fake()),
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
                create_function(None, vec![], Type::unit(), true, create_block(vec![])),
                vec![],
            )],
        );
    }

    #[test]
    fn parse_return_expr() {
        parse(
            "
                let main = fun(): int {
                    return; 
                    return 1;
                };
                let main = fun(): int => 1;
            ",
            vec![
                create_let(
                    "main",
                    Some(create_function(
                        Some("main".to_string()),
                        vec![],
                        Type::int(),
                        true,
                        create_block(vec![create_return(None), create_return(Some(int(1)))]),
                    )),
                ),
                create_let(
                    "main",
                    Some(create_function(
                        Some("main".to_string()),
                        vec![],
                        Type::int(),
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
        fun:string {};
        fun (x: int, y: int) {};
        fun (y: int, z: int) {
            let name = 2;
            123;
        };
        ",
            vec![
                create_function(None, vec![], Type::unit(), true, create_block(vec![])),
                create_function(None, vec![], Type::unit(), true, create_block(vec![])),
                create_function(None, vec![], Type::string(), true, create_block(vec![])),
                create_function(
                    None,
                    vec![ident_type("x", Type::int()), ident_type("y", Type::int())],
                    Type::unit(),
                    true,
                    create_block(vec![]),
                ),
                create_function(
                    None,
                    vec![ident_type("y", Type::int()), ident_type("z", Type::int())],
                    Type::unit(),
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
                first_name: string,
            };
        ",
            vec![create_data_struct(
                "Person",
                vec![("first_name", Type::string())],
                vec![],
            )],
        );

        parse(
            "
            data Person {
                first_name: string,
                last_name: string,
                age: int
            };
        ",
            vec![create_data_struct(
                "Person",
                vec![
                    ("first_name", Type::string()),
                    ("last_name", Type::string()),
                    ("age", Type::int()),
                ],
                vec![],
            )],
        );

        parse(
            "
            data Person {
                first_name: string,
                last_name: string,
                age: int,
            };
        ",
            vec![create_data_struct(
                "Person",
                vec![
                    ("first_name", Type::string()),
                    ("last_name", Type::string()),
                    ("age", Type::int()),
                ],
                vec![],
            )],
        );
    }

    #[test]
    fn parse_data_struct_methods_expr() {
        parse(
            "
            data Person {
                first_name: string,
            } :: {
                fun new {
                }

                fun name(self) {

                }
            };
        ",
            vec![create_data_struct(
                "Person",
                vec![("first_name", Type::string())],
                vec![
                    create_function(
                        Some("new".to_string()),
                        vec![],
                        Type::unit(),
                        true,
                        create_block(vec![]),
                    ),
                    create_function(
                        Some("name".to_string()),
                        vec![ident_self("Person")],
                        Type::unit(),
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
                first_name: string,
            } :: {
                fun name(self) {
                    self.first_name;
                }
                fun set_name(self, name: string) {
                    self.first_name = name;
                }
            };
        ",
            vec![create_data_struct(
                "Person",
                vec![("first_name", Type::string())],
                vec![
                    create_function(
                        Some("name".to_string()),
                        vec![ident_self("Person")],
                        Type::unit(),
                        false,
                        create_block(vec![create_get_property(
                            create_self("self"),
                            ident("first_name"),
                            false,
                        )]),
                    ),
                    create_function(
                        Some("set_name".to_string()),
                        vec![ident_self("Person"), ident_type("name", Type::string())],
                        Type::unit(),
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
            data Person { first_name: string, last_name: string, age: int };
            Person { first_name: "john", last_name: "doe", age: 23 };
        "#,
            vec![
                create_data_struct(
                    "Person",
                    vec![
                        ("first_name", Type::string()),
                        ("last_name", Type::string()),
                        ("age", Type::int()),
                    ],
                    vec![],
                ),
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
                id: int,
                first_name: string,
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
                create_data_struct(
                    "Person",
                    vec![("id", Type::int()), ("first_name", Type::string())],
                    vec![],
                ),
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
                first_name: string,
                last_name: string,
                age: int,
            };
            let p = Person{};
            p.first_name;
        ",
            vec![
                create_data_struct(
                    "Person",
                    vec![
                        ("first_name", Type::string()),
                        ("last_name", Type::string()),
                        ("age", Type::int()),
                    ],
                    vec![],
                ),
                create_let("p", Some(create_data_struct_instance("Person", vec![]))),
                create_get_property(create_let_ref("p"), ident("first_name"), false),
            ],
        );
    }

    #[test]
    fn parse_set_property_expr() {
        parse(
            r#"
            data Person {
                first_name: string,
                last_name: string,
                age: int,
            };
            let p = Person{};
            p.age = 1;
        "#,
            vec![
                create_data_struct(
                    "Person",
                    vec![
                        ("first_name", Type::string()),
                        ("last_name", Type::string()),
                        ("age", Type::int()),
                    ],
                    vec![],
                ),
                create_let("p", Some(create_data_struct_instance("Person", vec![]))),
                create_set_property(create_let_ref("p"), ident("age"), int(1)),
            ],
        );
        parse(
            r#"
            data Person {
                first_name: string,
                last_name: string,
                age: int,
            };
            let p = Person{};
            p.age += 1;
            p.age -= 1;
            p.age *= 1;
            p.age /= 1;
        "#,
            vec![
                create_data_struct(
                    "Person",
                    vec![
                        ("first_name", Type::string()),
                        ("last_name", Type::string()),
                        ("age", Type::int()),
                    ],
                    vec![],
                ),
                create_let("p", Some(create_data_struct_instance("Person", vec![]))),
                create_set_property(
                    create_let_ref("p"),
                    ident("age"),
                    create_binop(
                        create_get_property(create_let_ref("p"), ident("age"), false),
                        BinaryOperation::Add(Span::fake()),
                        int(1),
                    ),
                ),
                create_set_property(
                    create_let_ref("p"),
                    ident("age"),
                    create_binop(
                        create_get_property(create_let_ref("p"), ident("age"), false),
                        BinaryOperation::Substract(Span::fake()),
                        int(1),
                    ),
                ),
                create_set_property(
                    create_let_ref("p"),
                    ident("age"),
                    create_binop(
                        create_get_property(create_let_ref("p"), ident("age"), false),
                        BinaryOperation::Multiply(Span::fake()),
                        int(1),
                    ),
                ),
                create_set_property(
                    create_let_ref("p"),
                    ident("age"),
                    create_binop(
                        create_get_property(create_let_ref("p"), ident("age"), false),
                        BinaryOperation::Divide(Span::fake()),
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
