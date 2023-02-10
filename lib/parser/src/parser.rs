use crate::scanner::{Scanner, ScannerMode, Token, TokenType};
use crate::ParserError;
use ast::{
    BinaryOperation, CallArgs, DataStructInstanceField, Expression, Identifier, LiteralValue,
    Program,
};
use span_util::Span;
use type_core::{FunctionParam, Type};

const RECOVER_SET: [TokenType; 6] = [
    TokenType::Let,
    TokenType::Fn,
    TokenType::Data,
    TokenType::Loop,
    TokenType::If,
    // TokenType::Identifier,
    TokenType::EOF,
];

#[derive(Debug, PartialEq, Clone)]
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

    fn add_error(&mut self, err: ParserError) {
        self.errors.push(err);
    }

    fn error_and_sync(&mut self, err: ParserError) {
        self.add_error(err);
        self.sync();
    }

    fn sync(&mut self) {
        while !self.is_end() {
            // if self.prev_token.token_type == TokenType::SemiColon
            //     && RECOVER_SET.contains(&self.curr_token.token_type)
            // {
            //     return;
            // }

            // if self.curr_token.token_type == TokenType::SemiColon {
            //     return;
            // }

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
            if let Err(e) = self.eat_semi() {
                self.error_and_sync(e);
            };
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
        } else if self.matches(vec![TokenType::For]) {
            return self.for_expression();
        } else if self.matches(vec![TokenType::While]) {
            return self.while_expression();
        } else if self.matches(vec![TokenType::Break, TokenType::Continue]) {
            return self.continue_break_expression();
        }

        self.assignment()
    }

    fn parse_parameters(&mut self, token_end: TokenType) -> Result<Vec<Identifier>, ParserError> {
        let mut params = vec![];

        if self.curr_token.token_type != token_end {
            while !self.is_end() {
                if self.check(TokenType::SELF) {
                    return Err(ParserError::Error(
                        "Expected 'self' to be the first parameter in a method".to_string(),
                        self.curr_token.clone(),
                    ));
                }

                let ident = self.eat(TokenType::Identifier, "Expected 'identifier'")?;

                let ident = if self.eat_optional(TokenType::Colon).is_none() {
                    Identifier::new(ident.value, ident.span)
                } else {
                    let ttype = self.parse_type(false)?;
                    Identifier::with_value_type(ident.value, Some(ttype), ident.span)
                };

                params.push(ident);

                if !self.matches(vec![TokenType::Comma, TokenType::SemiColon])
                    || self.curr_token.token_type == token_end
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
        } else if self.matches(vec![TokenType::Fn]) {
            self.eat(TokenType::LeftParen, "Expected '('")?;
            let params = self.parse_parameters(TokenType::RightParen)?;
            self.eat(TokenType::RightParen, "Expected ')'")?;
            let mut return_type = Type::unit();
            if self.eat_optional(TokenType::Colon).is_some() {
                return_type = self.parse_type(true)?;
            }

            let p: Vec<FunctionParam> = params
                .iter()
                .map(|i| FunctionParam {
                    name: i.value.to_string(),
                    ttype: i.value_type.as_ref().unwrap().clone(),
                })
                .collect();

            Some(Type::function(p, return_type))
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
        let let_token = self.prev_token.clone();
        let _ident = self.curr_token.clone();
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

                Err(err) => {
                    return Err(match err {
                        ParserError::UnexpectedToken(_) => ParserError::ExpectedToken(
                            format!(
                                "Expected expression here, found '{}'",
                                self.curr_token.clone()
                            ),
                            self.curr_token.clone(),
                        ),
                        e => e,
                    })
                }
            };
            init = Some(expr);
        }

        let span: Span = let_token.span.extend(self.curr_token.span.clone());

        Ok(Expression::create_let(
            Identifier::with_value_type(identifier.value, itype, identifier.span),
            init,
            span,
            let_token.span,
        ))
    }

    fn continue_break_expression(&mut self) -> ParserResult {
        Ok(match self.prev_token.token_type {
            TokenType::Break => Expression::create_break(self.prev_token.span.clone()),
            TokenType::Continue => Expression::create_continue(self.prev_token.span.clone()),
            _ => unreachable!(),
        })
    }

    fn while_expression(&mut self) -> ParserResult {
        let while_token = self.prev_token.clone();
        let left_paren = self.eat_optional(TokenType::LeftParen);
        let condition = self.expression()?;

        if let Some(_) = left_paren {
            self.eat(TokenType::RightParen, "Expected ')'")?;
        }

        let body = self.expression()?;

        Ok(Expression::create_while(condition, body, while_token.span))
    }

    fn for_expression(&mut self) -> ParserResult {
        let for_token = self.prev_token.clone();
        let left_paren = self.eat_optional(TokenType::LeftParen);
        let ident = self.eat(TokenType::Identifier, "Expected 'identifier'")?;

        self.eat(TokenType::In, "Expected 'in'")?;

        let iterator = self.expression()?;

        if let Some(_) = left_paren {
            self.eat(TokenType::RightParen, "Expected ')'")?;
        }

        let body = self.expression()?;

        Ok(Expression::create_for(
            Identifier::new(ident.value, ident.span),
            iterator,
            body,
            for_token.span.clone(),
            for_token.span,
        ))
    }

    fn loop_expression(&mut self) -> ParserResult {
        let loop_token = self.prev_token.clone();

        self.eat(TokenType::LeftBrace, "Expected '{'")?;

        let body = self.block_expression()?;

        Ok(Expression::create_loop(body, loop_token.span))
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
                        Expression::create_let_ref(ident, id_span, None),
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
        let ident_token = self.eat(TokenType::Identifier, "Expected identifier")?;
        let identifier = Identifier::new(ident_token.value.to_string(), ident_token.span);

        if !self.check(TokenType::LeftParen) {
            return Ok(Expression::create_data_struct(
                identifier,
                Vec::new(),
                Vec::new(),
                ident_span,
            ));
        }

        self.eat(TokenType::LeftParen, "Expected '(")?;

        let fields = self.parse_parameters(TokenType::RightParen)?;
        let mut methods = vec![];

        self.eat(TokenType::RightParen, "Expected ')'")?;

        let left_brace = self.eat_optional(TokenType::LeftBrace);

        if left_brace.is_some() {
            while !self.check(TokenType::RightBrace) && !self.is_end() {
                self.eat(TokenType::Fn, "Expected 'fun'")?;
                methods.push(self.fun_expression(FunctionKind::Method(ident_token.value.clone()))?);
                self.eat_optional(TokenType::SemiColon); // remove auto inserted semi colon
            }

            self.eat(TokenType::RightBrace, "Expected '}'")?;
        }

        Ok(Expression::create_data_struct(
            identifier, fields, methods, ident_span,
        ))
    }

    fn return_expression(&mut self) -> ParserResult {
        let return_token = self.prev_token.clone();
        let mut span: Span = return_token.span.clone();
        let mut value = None;

        if !self.curr_token.is_semi_colon() {
            let expr = self.expression()?;
            span = span.extend(expr.get_span());
            value = Some(expr);
        }

        let span = span.extend(self.curr_token.span.clone());
        Ok(Expression::create_return(value, span, return_token.span))
    }

    fn fun_expression(&mut self, kind: FunctionKind) -> ParserResult {
        let fun_span: Span = self.prev_token.span.clone();
        let mut name = None;
        let mut params = vec![];
        let mut is_static = true;
        let mut return_type = Type::unit();

        if let FunctionKind::Method(_) = kind {
            let ident = self.eat(TokenType::Identifier, "Expected method identifier")?;
            name = Some(ident.value);
        }

        let left_paren = self.eat_optional(TokenType::LeftParen);

        if let Some(_) = left_paren {
            match (kind.clone(), self.check(TokenType::SELF)) {
                (FunctionKind::Method(data_struct_identifier), true) => {
                    let token = self.eat(TokenType::SELF, "Expected 'self'")?;
                    self.eat_optional(TokenType::Comma);
                    is_static = false;
                    params.push(Identifier::with_all(
                        token.value,
                        // token.token_type,
                        Type::Identifier(data_struct_identifier),
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
        } else if self.check(TokenType::Identifier) && matches!(kind, FunctionKind::Function) {
            // Allow functions without parens to have one identifier e.g
            // fn x => 1
            // fn y { 2 }
            // multiple identifiers require parens
            let param = self.eat(TokenType::Identifier, "Expected 'identifier'")?;
            params.push(Identifier::new(param.value, param.span))
        }

        // do not check for return type if parsing shorthand lambda function e.g
        // fn x:int => 1; is invalid code
        // if the function is declared as a method fn new:int => 1; is valid.
        // since 'new' is used as the function name and 'int' as its return type. in the context of a method
        if self.prev_token.token_type != TokenType::Identifier
            || !matches!(kind, FunctionKind::Function)
        {
            return_type = match self.eat_optional(TokenType::Colon) {
                Some(_) => self.parse_type(true)?,
                None => Type::unit(),
            };
        }

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
                            let_ref.name.clone(),
                            rhs,
                            full_span,
                            None,
                        ));
                    }
                    _ => {
                        return Ok(Expression::create_assign(
                            let_ref.name.clone(),
                            Expression::create_binop(
                                Expression::create_let_ref(
                                    let_ref.name.clone(),
                                    rhs_span.clone(),
                                    None,
                                ),
                                assignment_tok.to_binary_operator(),
                                rhs,
                                rhs_span,
                            ),
                            full_span,
                            None,
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
                                assignment_tok.to_binary_operator(),
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
                                assignment_tok.to_binary_operator(),
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
            let op = self.prev_token.clone();
            let rhs = self.and()?;
            let span = expr.get_span().extend(rhs.get_span());
            expr = Expression::create_logic(expr, op.to_logic_operator(), rhs, span);
        }

        Ok(expr)
    }

    fn and(&mut self) -> ParserResult {
        let mut expr = self.equality()?;
        while self.matches(vec![TokenType::And]) {
            let op = self.prev_token.clone();
            let rhs = self.equality()?;
            let span = expr.get_span().extend(rhs.get_span());
            expr = Expression::create_logic(expr, op.to_logic_operator(), rhs, span);
        }

        Ok(expr)
    }

    fn equality(&mut self) -> ParserResult {
        let mut expr = self.comparison()?;
        while self.matches(vec![TokenType::EqualEqual, TokenType::NotEqual]) {
            let op = self.prev_token.clone();
            let rhs = self.comparison()?;
            let span = expr.get_span().extend(rhs.get_span());

            expr = Expression::create_logic(expr, op.to_logic_operator(), rhs, span);
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
            let op = self.prev_token.clone();
            let rhs = self.term()?;
            let span = expr.get_span().extend(rhs.get_span());

            expr = Expression::create_logic(expr, op.to_logic_operator(), rhs, span);
        }
        Ok(expr)
    }

    fn term(&mut self) -> ParserResult {
        let mut expr = self.factor()?;
        while self.matches(vec![TokenType::Plus, TokenType::Minus]) {
            let tok = self.prev_token.clone();
            let rhs = self.factor()?;
            let span = expr.get_span().extend(rhs.get_span());

            let op = tok.to_binary_operator();
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
            let op = tok.to_binary_operator();
            expr = Expression::create_binop(expr, op, rhs, span)
        }
        Ok(expr)
    }

    fn unary(&mut self) -> ParserResult {
        if self.matches(vec![TokenType::Plus, TokenType::Minus, TokenType::Bang]) {
            let tok = self.prev_token.clone();
            let rhs = self.unary()?;
            let span: Span = tok.span.extend(rhs.get_span());
            let op = tok.to_unary_operator();

            return Ok(Expression::create_unaryop(op, rhs, span));
        }
        self.call()
    }

    fn call(&mut self) -> ParserResult {
        let mut expr = self.primary()?;
        let primary_span = expr.get_span();
        loop {
            if self.matches(vec![TokenType::LeftParen]) {
                let mut args = vec![];

                if self.curr_token.token_type != TokenType::RightParen && !self.is_end() {
                    loop {
                        let mut ident: Option<Identifier> = None;
                        if self.check(TokenType::Identifier) && self.check_peek(TokenType::Assign) {
                            let token = self.eat(TokenType::Identifier, "Expected 'identifier'")?;
                            self.eat(TokenType::Assign, "Expected '='")?;
                            ident = Some(Identifier::new(token.value, token.span))
                        }

                        args.push(CallArgs(ident, self.expression()?));

                        if !self.matches(vec![TokenType::Comma])
                            || self.curr_token.token_type == TokenType::RightParen
                        {
                            break;
                        }
                    }
                }

                self.eat(
                    TokenType::RightParen,
                    "unterminated function call expected either ',' or ')'",
                )?;

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
            return Ok(Expression::create_literal(LiteralValue::Int(val), span));
        }

        if self.matches(vec![TokenType::True, TokenType::False]) {
            let bool = match token.token_type {
                TokenType::True => true,
                _ => false,
            };

            return Ok(Expression::create_literal(LiteralValue::Bool(bool), span));
        }

        if self.matches(vec![TokenType::FloatConst]) {
            let val: f64 = match token.value.parse() {
                Ok(v) => v,
                Err(msg) => panic!("not a float {}", msg),
            };
            return Ok(Expression::create_literal(LiteralValue::Float(val), span));
        }

        if self.matches(vec![TokenType::Null]) {
            return Ok(Expression::create_literal(LiteralValue::Null, span));
        }

        if self.check(TokenType::DoubleQuote) {
            return self.parse_string();
        }

        if self.matches(vec![TokenType::StringConst]) {
            return Ok(Expression::create_literal(
                LiteralValue::String(token.value),
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
                None,
            ));
        }

        if self.matches(vec![TokenType::Fn]) {
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

            self.eat(TokenType::RightBracket, "Expected ']'")?;
            let span: Span = (token.span.start..self.curr_token.span.start).into();
            return Ok(Expression::create_literal(LiteralValue::Array(exprs), span));
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
        let quote_start = self.curr_token.clone();
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
                        LiteralValue::String(self.curr_token.value.to_string()),
                        (0..0).into(),
                    ));
                }
                TokenType::DoubleQuote => {
                    break;
                }
                _ => {
                    self.scanner.set_mode(ScannerMode::Default);
                    return Err(ParserError::UnterminatedString(quote_start));
                }
            }
        }
        self.scanner.set_mode(ScannerMode::Default);
        self.advance(); // advance over '"'
        let quote_end = self.prev_token.clone();
        let out_len = out.len();
        let span: Span = quote_start.span.extend(quote_end.span.clone());

        if out_len == 0 {
            // return a empty string if there is no output
            return Ok(Expression::create_literal(
                LiteralValue::String("".to_string()),
                span,
            ));
        } else if out_len == 1 && out[0].is_string_lit() {
            // return the only elem in output as string lit
            // no reason to create a binop expression
            match &out[0] {
                Expression::Literal(lit) => match lit.value {
                    LiteralValue::String(_) => {
                        return Ok(Expression::create_literal(lit.value.clone(), span))
                    }
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            }
        } else if out_len == 1 {
            // should be an expression other than string literal
            // append to empty string so it gets formatted as string
            return Ok(Expression::create_binop(
                Expression::create_literal(LiteralValue::String("".to_string()), span.clone()),
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
pub mod parser_tests {

    use super::Parser;
    use crate::scanner::{Scanner, Token};
    use crate::test_utils::*;
    use crate::ParserError;
    use ast::{BinaryOperation, LogicOperation, Program, UnaryOperation};

    use span_util::Span;
    use type_core::{FunctionParam, Type};

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

    fn parse_recover(source: &str, expected: Program) {
        let (res, err) = run_parser(source);

        println!("errors:{} --------------", err.len());
        for e in err {
            println!("{}", e.to_string())
        }
        println!("errors:--------------");

        assert_eq!(res, expected)
    }

    fn parse_error(source: &str, expected_errors: Vec<ParserError>) {
        let (_, errors) = run_parser(source);

        if errors.is_empty() {
            panic!("parser did not emit errors");
        };

        if errors.len() != expected_errors.len() {
            println!("{:#?}", errors);
            panic!(
                "parser emitted {} errors, expected errors has {} errors",
                errors.len(),
                expected_errors.len()
            );
        }
        for (err, expected) in errors.iter().zip(expected_errors) {
            assert_eq!(*err, expected);
        }
    }

    #[test]
    fn recover_missing_semi_expr() {
        parse_recover(
            r#"
            let x = 1
            let y = 2
            let n = 33;
            let bb = {}
        "#,
            vec![
                create_let("x", Some(int(1))),
                create_let("y", Some(int(2))),
                create_let("n", Some(int(33))),
                create_let("bb", Some(create_block(vec![]))),
            ],
        )
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
                create_let_type(ident_type("exists", Type::bool()), Some(bool_lit(true))),
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
            let x: fn(a: string);
            let x: fn();
            let x: fn(): unit;
            let x: fn(): string;
            let x: fn(a: string): int;
            let x: fn(a: string,): int[];
            let x: (fn(b: int): int)[];
            let x: fn(b: int)[];
        ",
            vec![
                create_let_type(
                    ident_type(
                        "x",
                        Type::function(vec![create_param_type("a", Type::string())], Type::unit()),
                    ),
                    None,
                ),
                create_let_type(ident_type("x", Type::function(vec![], Type::unit())), None),
                create_let_type(ident_type("x", Type::function(vec![], Type::unit())), None),
                create_let_type(
                    ident_type("x", Type::function(vec![], Type::string())),
                    None,
                ),
                create_let_type(
                    ident_type(
                        "x",
                        Type::function(vec![create_param_type("a", Type::string())], Type::int()),
                    ),
                    None,
                ),
                create_let_type(
                    ident_type(
                        "x",
                        Type::function(
                            vec![create_param_type("a", Type::string())],
                            Type::array(Type::int()),
                        ),
                    ),
                    None,
                ),
                create_let_type(
                    ident_type(
                        "x",
                        Type::array(Type::function(
                            vec![create_param_type("b", Type::int())],
                            Type::int(),
                        )),
                    ),
                    None,
                ),
                create_let_type(
                    ident_type(
                        "x",
                        Type::array(Type::function(
                            vec![create_param_type("b", Type::int())],
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
        let joe: Person = Person();
        let family: Person[] = [Person()];
        let x: fn(x: Person);
        "#,
            vec![
                create_let_type(
                    ident_type("joe", Type::identifier("Person".to_string())),
                    Some(create_call(create_let_ref("Person"), vec![])),
                ),
                create_let_type(
                    ident_type("family", Type::array(Type::Identifier("Person".into()))),
                    Some(array(vec![create_call(create_let_ref("Person"), vec![])])),
                ),
                create_let_type(
                    ident_type(
                        "x",
                        Type::function(
                            vec![FunctionParam {
                                name: "x".to_string(),
                                ttype: Type::identifier("Person".to_string()),
                            }],
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
            let name: +;
            let name: 123 = 2;
            let name: @23432 = 2;
            let name: unit;
            let name: int[ ;
            let name: int] ;
        ",
            vec![
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
        let x: fn;
        let x: fn(;
        let x: fn): int = 2;
        let x: fn(): = 2;
        let x: fn(a: unit): int = 2;
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
        fn(a): int {};
        fn(a: 123) {};
        fn(a: unit) {};
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
    fn parse_string_interpolation() {
        // TODO test parser errors for unterminated string and interpolation
        // "$() -> this returns a error "Expected ';' after expression" should be unterminated string
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
    fn parse_for_loop_expr() {
        parse(
            "
        for i in name {};
        for (i in name) {};
        ",
            vec![
                create_for(ident("i"), create_let_ref("name"), create_block(vec![])),
                create_for(ident("i"), create_let_ref("name"), create_block(vec![])),
            ],
        );

        parse(
            "
        for i in (Person()) {};
        ",
            vec![create_for(
                ident("i"),
                create_grouping(create_call(create_let_ref("Person"), vec![])),
                create_block(vec![]),
            )],
        );
    }

    #[test]
    fn parse_while_loop_expr() {
        parse(
            "
        while true {};
        while (true) {};
        ",
            vec![
                create_while(bool_lit(true), create_block(vec![])),
                create_while(bool_lit(true), create_block(vec![])),
            ],
        );

        parse(
            "
        while (Person()) {};
        ",
            vec![create_while(
                create_call(create_let_ref("Person"), vec![]),
                create_block(vec![]),
            )],
        );
    }

    #[test]
    fn parse_break_continue_expr() {
        parse(
            "
        loop { break; }; 
        loop { continue; }; 
        ",
            vec![
                create_loop(create_block(vec![create_break()])),
                create_loop(create_block(vec![create_continue()])),
            ],
        )
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
            "let main = fn { self; };",
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
                create_assign("num", int(1)),
                create_assign(
                    "num",
                    create_binop(
                        create_let_ref("num"),
                        BinaryOperation::Add(Span::fake()),
                        int(1),
                    ),
                ),
                create_assign(
                    "num",
                    create_binop(
                        create_let_ref("num"),
                        BinaryOperation::Subtract(Span::fake()),
                        int(1),
                    ),
                ),
                create_assign(
                    "num",
                    create_binop(
                        create_let_ref("num"),
                        BinaryOperation::Multiply(Span::fake()),
                        int(1),
                    ),
                ),
                create_assign(
                    "num",
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
                create_logic(int(1), LogicOperation::And(Span::fake()), int(2)),
                create_logic(int(2), LogicOperation::Or(Span::fake()), int(1)),
            ],
        );
    }

    #[test]
    fn parse_equality_expr() {
        parse(
            "1 == 2; 2 != 1;",
            vec![
                create_logic(int(1), LogicOperation::Equal(Span::fake()), int(2)),
                create_logic(int(2), LogicOperation::NotEqual(Span::fake()), int(1)),
            ],
        );
    }

    #[test]
    fn parse_comparison_expr() {
        parse(
            "1 < 2; 2 <= 1;3 > 1; 3 >= 1;",
            vec![
                create_logic(int(1), LogicOperation::LessThan(Span::fake()), int(2)),
                create_logic(int(2), LogicOperation::LessThanEqual(Span::fake()), int(1)),
                create_logic(int(3), LogicOperation::GreaterThan(Span::fake()), int(1)),
                create_logic(
                    int(3),
                    LogicOperation::GreaterThanEqual(Span::fake()),
                    int(1),
                ),
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
                create_unaryop(UnaryOperation::Minus(Span::fake()), int(1)),
                create_binop(
                    int(1),
                    BinaryOperation::Add(Span::fake()),
                    create_unaryop(UnaryOperation::Minus(Span::fake()), int(1)),
                ),
                create_unaryop(UnaryOperation::Not(Span::fake()), int(1)),
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
                        BinaryOperation::Subtract(Span::fake()),
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
                    vec![
                        arg(int(1)),
                        arg(create_let_ref("name")),
                        arg(bool_lit(true)),
                    ],
                ),
            ],
        );

        parse(
            "fn (){}();",
            vec![create_call(
                create_function(None, vec![], Type::unit(), true, create_block(vec![])),
                vec![],
            )],
        );
    }

    #[test]
    fn parse_function_call_named_args() {
        parse(
            r#"
        update_user(id=1, name = "John", "Doe");
        save(1, drop=false);
        "#,
            vec![
                create_call(
                    create_let_ref("update_user"),
                    vec![
                        named_arg(ident("id"), int(1)),
                        named_arg(ident("name"), string_lit("John")),
                        arg(string_lit("Doe")),
                    ],
                ),
                create_call(
                    create_let_ref("save"),
                    vec![arg(int(1)), named_arg(ident("drop"), bool_lit(false))],
                ),
            ],
        )
    }

    #[test]
    fn parse_return_expr() {
        parse(
            "
                let main = fn(): int {
                    return; 
                    return 1;
                };
                let main = fn(): int => 1;
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
        fn {};
        fn () {};
        fn:string {};
        fn (x: int, y: int) {};
        fn (y: int, z: int) {
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
    fn parse_shorthand_lambda_expr() {
        parse(
            "
            fn x => 1
        ",
            vec![create_function(
                None,
                vec![ident("x")],
                Type::unit(),
                true,
                create_block(vec![create_implicit_return(int(1))]),
            )],
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
                    create_block(vec![create_implicit_return(create_assign("x", int(1)))]),
                    Some(create_block(vec![create_implicit_return(create_assign(
                        "x",
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
            data Person;
        ",
            vec![create_data_struct("Person", vec![], vec![])],
        );

        parse(
            "
            data Person(
                first_name: string,
            );
        ",
            vec![create_data_struct(
                "Person",
                vec![("first_name", Type::string())],
                vec![],
            )],
        );

        parse(
            "
            data Person(
                first_name: string,
                last_name: string,
                age: int
            );
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
            data Person(
                first_name: string,
                last_name: string,
                age: int,
            );
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
            data Person (
                first_name: string,
            ) {
                fn new {
                }

                fn name(self) {

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
            data Person (
                first_name: string,
            ) {
                fn name(self) {
                    self.first_name;
                }
                fn set_name(self, name: string) {
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
            data Person;
            Person();
        ",
            vec![
                create_data_struct("Person", vec![], vec![]),
                create_call(create_let_ref("Person"), vec![]),
            ],
        );

        parse(
            r#"
            data Person(first_name: string, last_name: string, age: int);
            Person(first_name = "john", last_name = "doe", age = 23);
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
                create_call(
                    create_let_ref("Person"),
                    vec![
                        named_arg(ident("first_name"), string_lit("john")),
                        named_arg(ident("last_name"), string_lit("doe")),
                        named_arg(ident("age"), int(23)),
                    ],
                ),
            ],
        );

        parse(
            "Person(id = 1).id;",
            vec![create_get_property(
                create_call(
                    create_let_ref("Person"),
                    vec![named_arg(ident("id"), int(1))],
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
            data Person(
                id: int,
                first_name: string,
            );
            let id = 123;
            let first_name = "John";
            Person(
                id,
                first_name,
            );
            Person(
                id,
                first_name
            );
        "#,
            vec![
                create_data_struct(
                    "Person",
                    vec![("id", Type::int()), ("first_name", Type::string())],
                    vec![],
                ),
                create_let("id", Some(int(123))),
                create_let("first_name", Some(string_lit("John"))),
                create_call(
                    create_let_ref("Person"),
                    vec![arg(create_let_ref("id")), arg(create_let_ref("first_name"))],
                ),
                create_call(
                    create_let_ref("Person"),
                    vec![arg(create_let_ref("id")), arg(create_let_ref("first_name"))],
                ),
            ],
        );
    }

    #[test]
    fn parse_get_property_expr() {
        parse(
            "
            data Person(
                first_name: string,
                last_name: string,
                age: int,
            );
            let p = Person();
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
                create_let("p", Some(create_call(create_let_ref("Person"), vec![]))),
                create_get_property(create_let_ref("p"), ident("first_name"), false),
            ],
        );
    }

    #[test]
    fn parse_set_property_expr() {
        parse(
            r#"
            data Person(
                first_name: string,
                last_name: string,
                age: int,
            );
            let p = Person();
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
                create_let("p", Some(create_call(create_let_ref("Person"), vec![]))),
                create_set_property(create_let_ref("p"), ident("age"), int(1)),
            ],
        );
        parse(
            r#"
            data Person(
                first_name: string,
                last_name: string,
                age: int,
            );
            let p = Person();
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
                create_let("p", Some(create_call(create_let_ref("Person"), vec![]))),
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
                        BinaryOperation::Subtract(Span::fake()),
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
}
