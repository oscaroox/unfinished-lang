#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    Identifier,
    IntConst,
    FloatConst,
    StringConst,

    // keywords
    Let,
    Fun,
    If,
    Else,
    True,
    False,
    Null,

    Plus,
    Minus,
    Star,
    Slash,

    Assign,
    AssignColon,
    AssignPlus,
    AssignMinus,
    AssignStar,
    AssignSlash,

    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Comma,
    SemiColon,

    Illegal,
    EOF,
    BadToken,
}

impl std::fmt::Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenType::Identifier => write!(f, "IDENTIFIER"),
            TokenType::IntConst => write!(f, "INTCONST"),
            TokenType::FloatConst => write!(f, "FLOATCONST"),
            TokenType::StringConst => write!(f, "STRINGCONST"),
            TokenType::Let => write!(f, "LET"),
            TokenType::Fun => write!(f, "FUN"),
            TokenType::If => write!(f, "IF"),
            TokenType::Else => write!(f, "ELSE"),
            TokenType::True => write!(f, "TRUE"),
            TokenType::False => write!(f, "FALSE"),
            TokenType::Null => write!(f, "NULL"),
            TokenType::Plus => write!(f, "+"),
            TokenType::Minus => write!(f, "-"),
            TokenType::Star => write!(f, "*"),
            TokenType::Slash => write!(f, "/"),
            TokenType::Assign => write!(f, "="),
            TokenType::AssignColon => write!(f, ":="),
            TokenType::AssignPlus => write!(f, "+="),
            TokenType::AssignMinus => write!(f, "-="),
            TokenType::AssignStar => write!(f, "*="),
            TokenType::AssignSlash => write!(f, "/="),
            TokenType::LeftParen => write!(f, "("),
            TokenType::RightParen => write!(f, ")"),
            TokenType::LeftBrace => write!(f, "{{"),
            TokenType::RightBrace => write!(f, "}}"),
            TokenType::LeftBracket => write!(f, "["),
            TokenType::RightBracket => write!(f, "]"),
            TokenType::Comma => write!(f, ","),
            TokenType::SemiColon => write!(f, ";"),
            TokenType::Illegal => write!(f, "ILLEGAL"),
            TokenType::EOF => write!(f, "EOF"),
            TokenType::BadToken => write!(f, "BADTOKEN"),
        }
    }
}
