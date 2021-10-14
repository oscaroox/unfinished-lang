use crate::Position;
use crate::TokenType;

macro_rules! impl_token {
    ($_meth:ident, $tok:ident) => {
        pub fn $_meth(col_line: (isize, isize)) -> Token {
            Token {
                token_type: TokenType::$tok,
                value: TokenType::$tok.to_string(),
                pos: Position::new(col_line.0, col_line.1),
            }
        }
    };
}

macro_rules! impl_value_token {
    ($_meth:ident, $tok:ident) => {
        pub fn $_meth(value: String, col_line: (isize, isize)) -> Token {
            Token {
                token_type: TokenType::$tok,
                value,
                pos: Position::new(col_line.0, col_line.1),
            }
        }
    };
}

#[derive(Debug, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub value: String,
    pub pos: Position,
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
    impl_token!(null, Null);

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
}
