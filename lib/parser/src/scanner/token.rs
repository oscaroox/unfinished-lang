use super::TokenType;
use ast::{BinaryOperation, LogicOperation, UnaryOperation};
use span_util::Span;
use std::ops::Range;

macro_rules! impl_token {
    ($_meth:ident, $tok:ident) => {
        pub fn $_meth(label: Range<usize>) -> Token {
            Token {
                token_type: TokenType::$tok,
                value: TokenType::$tok.to_string(),
                span: label.clone().into(),
            }
        }
    };
}

macro_rules! impl_value_token {
    ($_meth:ident, $tok:ident) => {
        pub fn $_meth(value: String, label: Range<usize>) -> Token {
            Token {
                token_type: TokenType::$tok,
                value,
                span: label.clone().into(),
            }
        }
    };
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub value: String,
    pub span: Span,
}

impl Token {
    impl_value_token!(identifier, Identifier);
    impl_value_token!(int_const, IntConst);
    impl_value_token!(float_const, FloatConst);
    impl_value_token!(string_const, StringConst);

    impl_token!(int, Int);
    impl_token!(float, Float);
    impl_token!(string, String);
    impl_token!(bool, Bool);
    impl_token!(unit, Unit);

    impl_value_token!(illegal, Illegal);
    impl_value_token!(bad_token, BadToken);

    impl_token!(let_token, Let);
    impl_token!(fn_token, Fn);
    impl_token!(true_token, True);
    impl_token!(false_token, False);
    impl_token!(if_token, If);
    impl_token!(else_token, Else);
    impl_token!(return_token, Return);
    impl_token!(data, Data);
    impl_token!(self_token, SELF);
    impl_token!(null, Null);
    impl_token!(loop_token, Loop);
    impl_token!(break_token, Break);
    impl_token!(continue_token, Continue);
    impl_token!(in_token, In);

    impl_token!(and, And);
    impl_token!(or, Or);
    impl_token!(less_than, LessThan);
    impl_token!(less_than_equal, LessThanEqual);
    impl_token!(greater_than, GreaterThan);
    impl_token!(greater_than_equal, GreaterThanEqual);
    impl_token!(equal_equal, EqualEqual);
    impl_token!(not_equal, NotEqual);
    impl_token!(arrow, Arrow);

    impl_token!(plus, Plus);
    impl_token!(minus, Minus);
    impl_token!(star, Star);
    impl_token!(slash, Slash);

    impl_token!(left_paren, LeftParen);
    impl_token!(right_paren, RightParen);
    impl_token!(left_brace, LeftBrace);
    impl_token!(right_brace, RightBrace);
    impl_token!(left_bracket, LeftBracket);
    impl_token!(right_bracket, RightBracket);
    impl_token!(comma, Comma);
    impl_token!(semi_colon, SemiColon);
    impl_token!(bang, Bang);
    impl_token!(colon, Colon);
    impl_token!(colon_colon, ColonColon);
    impl_token!(dot, Dot);

    impl_token!(assign, Assign);
    impl_token!(assign_colon, AssignColon);
    impl_token!(assign_plus, AssignPlus);
    impl_token!(assign_minus, AssignMinus);
    impl_token!(assign_star, AssignStar);
    impl_token!(assign_slash, AssignSlash);

    impl_token!(dollar_sign, DollarSign);
    impl_token!(double_quote, DoubleQuote);
    impl_token!(eof, EOF);

    pub fn auto_semi_colon() -> Token {
        Token { 
            token_type: TokenType::SemiColon, 
            value: TokenType::SemiColon.to_string(), 
            span: (0..0).into()
        }
    }

    pub fn is_eof(&self) -> bool {
        self.token_type == TokenType::EOF
    }

    pub fn is_let(&self) -> bool {
        self.token_type == TokenType::Let
    }

    pub fn is_assign(&self) -> bool {
        self.token_type == TokenType::Assign
    }

    pub fn is_arrow(&self) -> bool {
        self.token_type == TokenType::Arrow
    }

    pub fn is_semi_colon(&self) -> bool {
        self.token_type == TokenType::SemiColon
    }

    pub fn is_auto_inserted_semi_colon(&self) -> bool {
        self.is_semi_colon() && self.span.start == 0 && self.span.end == 0
    }

    pub fn replace_span(&mut self, new_span: Span) {
        self.span = new_span;
    }

    pub fn to_binary_operator(&self) -> BinaryOperation {
        let span = self.span.clone();
        match self.token_type {
            TokenType::Plus | TokenType::AssignPlus => BinaryOperation::Add(span),
            TokenType::Minus | TokenType::AssignMinus => BinaryOperation::Subtract(span),
            TokenType::Star | TokenType::AssignStar => BinaryOperation::Multiply(span),
            TokenType::Slash | TokenType::AssignSlash => BinaryOperation::Divide(span),
            _ => panic!(
                "Cannot convert from token type: {} to binary operator",
                self.token_type
            ),
        }
    }

    pub fn to_logic_operator(&self) -> LogicOperation {
        let span = self.span.clone();
        match self.token_type {
            TokenType::And => LogicOperation::And(span),
            TokenType::Or => LogicOperation::Or(span),
            TokenType::LessThan => LogicOperation::LessThan(span),
            TokenType::LessThanEqual => LogicOperation::LessThanEqual(span),
            TokenType::GreaterThan => LogicOperation::GreaterThan(span),
            TokenType::GreaterThanEqual => LogicOperation::GreaterThanEqual(span),
            TokenType::EqualEqual => LogicOperation::Equal(span),
            TokenType::NotEqual => LogicOperation::NotEqual(span),

            _ => panic!(
                "Cannot convert from token type: {} to logic operation",
                self.token_type
            ),
        }
    }

    pub fn to_unary_operator(&self) -> UnaryOperation {
        let span = self.span.clone();
        match self.token_type {
            TokenType::Plus => UnaryOperation::Plus(span),
            TokenType::Minus => UnaryOperation::Minus(span),
            TokenType::Bang => UnaryOperation::Not(span),
            _ => panic!(
                "Cannot convert from token type: {} to unary operation",
                self.token_type
            ),
        }
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}
