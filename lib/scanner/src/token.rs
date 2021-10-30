use spanner::Span;

use crate::TokenType;

macro_rules! impl_token {
    ($_meth:ident, $tok:ident) => {
        pub fn $_meth(span: Span) -> Token {
            Token {
                token_type: TokenType::$tok,
                value: TokenType::$tok.to_string(),
                span,
            }
        }
    };
}

macro_rules! impl_value_token {
    ($_meth:ident, $tok:ident) => {
        pub fn $_meth(value: String, span: Span) -> Token {
            Token {
                token_type: TokenType::$tok,
                value,
                span,
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

    impl_value_token!(illegal, Illegal);
    impl_value_token!(bad_token, BadToken);

    impl_token!(let_token, Let);
    impl_token!(fun_token, Fun);
    impl_token!(true_token, True);
    impl_token!(false_token, False);
    impl_token!(if_token, If);
    impl_token!(else_token, Else);
    impl_token!(return_token, Return);
    impl_token!(data, Data);
    impl_token!(null, Null);

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

    impl_token!(assign, Assign);
    impl_token!(assign_colon, AssignColon);
    impl_token!(assign_plus, AssignPlus);
    impl_token!(assign_minus, AssignMinus);
    impl_token!(assign_star, AssignStar);
    impl_token!(assign_slash, AssignSlash);

    impl_token!(eof, EOF);

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
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}
