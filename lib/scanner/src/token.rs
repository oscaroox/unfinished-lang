use crate::position::Position;

macro_rules! impl_token {
    ($_meth:ident, $tok:ident) => {
        pub fn $_meth(col: isize, line: isize) -> Token {
            Token::$tok(Position::new(col, line))
        }
    };
}

macro_rules! impl_value_token {
    ($_meth:ident, $tok:ident) => {
        pub fn $_meth(val: String, col: isize, line: isize) -> Token {
            Token::$tok(val, Position::new(col, line))
        }
    };
}

#[derive(Debug, PartialEq)]
pub enum Token {
    Identifier(String, Position),
    IntConst(String, Position),
    FloatConst(String, Position),
    StringConst(String, Position),

    // keywords
    Let(Position),
    Fun(Position),
    True(Position),
    False(Position),
    Null(Position),

    Plus(Position),
    Minus(Position),
    Star(Position),
    Slash(Position),

    Assign(Position),
    AssignColon(Position),
    AssignPlus(Position),
    AssignMinus(Position),
    AssignStar(Position),
    AssignSlash(Position),

    LeftParen(Position),
    RightParen(Position),
    LeftBrace(Position),
    RightBrace(Position),
    LeftBracket(Position),
    RightBracket(Position),
    Comma(Position),

    Illegal(String, Position),
    EOF(Position),
    BadToken(String, Position),
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
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Identifier(_, _) => write!(f, "Identifier"),
            Token::IntConst(v, _) => write!(f, "{}", v),
            Token::FloatConst(v, _) => write!(f, "{}", v),
            Token::StringConst(v, _) => write!(f, "{}", v),
            Token::Let(_) => write!(f, "Let"),
            Token::Fun(_) => write!(f, "Fun"),
            Token::True(_) => write!(f, "True"),
            Token::False(_) => write!(f, "False"),
            Token::Null(_) => write!(f, "Null"),
            Token::Plus(_) => write!(f, "+"),
            Token::Minus(_) => write!(f, "-"),
            Token::Star(_) => write!(f, "*"),
            Token::Slash(_) => write!(f, "/"),
            Token::LeftParen(_) => write!(f, "("),
            Token::RightParen(_) => write!(f, ")"),
            Token::Illegal(v, _) => write!(f, "{}", v),
            Token::EOF(_) => write!(f, "EOF"),
            Token::BadToken(_, _) => write!(f, "BadToken"),
            Token::Assign(_) => write!(f, "="),
            Token::AssignPlus(_) => write!(f, "+="),
            Token::AssignMinus(_) => write!(f, "-="),
            Token::AssignStar(_) => write!(f, "*="),
            Token::AssignSlash(_) => write!(f, "/="),
            Token::AssignColon(_) => write!(f, ":="),
            Token::Comma(_) => write!(f, ","),
            Token::LeftBrace(_) => write!(f, "{{"),
            Token::RightBrace(_) => write!(f, "}}"),
            Token::LeftBracket(_) => write!(f, "["),
            Token::RightBracket(_) => write!(f, "]"),
        }
    }
}
