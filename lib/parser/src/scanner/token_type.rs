#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    Identifier,
    IntConst,
    FloatConst,
    StringConst,

    // types
    Int,
    Float,
    String,
    Bool,
    Unit,

    // keywords
    Let,
    Fn,
    If,
    Else,
    True,
    False,
    Null,
    Return,
    SELF,
    Data,
    Loop,
    While,
    For,
    Break,
    Continue,
    In,

    And,
    Or,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
    EqualEqual,
    NotEqual,
    Arrow,

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
    Bang,
    Colon,
    ColonColon,
    Dot,
    Pipe, // |

    DoubleQuote, // "
    DollarSign,
    Illegal,
    EOF,
    BadToken,
}

impl std::fmt::Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenType::Identifier => write!(f, "IDENTIFIER"),
            TokenType::IntConst => write!(f, "INT_CONST"),
            TokenType::FloatConst => write!(f, "FLOAT_CONST"),
            TokenType::StringConst => write!(f, "STRING_CONST"),

            TokenType::Int => write!(f, "int"),
            TokenType::Float => write!(f, "float"),
            TokenType::String => write!(f, "string"),
            TokenType::Bool => write!(f, "bool"),
            TokenType::Unit => write!(f, "unit"),

            TokenType::Let => write!(f, "let"),
            TokenType::Fn => write!(f, "fn"),
            TokenType::If => write!(f, "if"),
            TokenType::Else => write!(f, "else"),
            TokenType::True => write!(f, "true"),
            TokenType::False => write!(f, "false"),
            TokenType::Null => write!(f, "null"),
            TokenType::And => write!(f, "&&"),
            TokenType::Or => write!(f, "||"),
            TokenType::Return => write!(f, "return"),
            TokenType::SELF => write!(f, "self"),
            TokenType::Data => write!(f, "data"),
            TokenType::Loop => write!(f, "loop"),
            TokenType::For => write!(f, "for"),
            TokenType::While => write!(f, "while"),
            TokenType::Break => write!(f, "break"),
            TokenType::Continue => write!(f, "continue"),
            TokenType::In => write!(f, "in"),
            TokenType::LessThan => write!(f, "<"),
            TokenType::LessThanEqual => write!(f, "<="),
            TokenType::GreaterThan => write!(f, ">"),
            TokenType::GreaterThanEqual => write!(f, ">="),
            TokenType::EqualEqual => write!(f, "=="),
            TokenType::NotEqual => write!(f, "!="),
            TokenType::Arrow => write!(f, "=>"),
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
            TokenType::Bang => write!(f, "!"),
            TokenType::Colon => write!(f, ":"),
            TokenType::ColonColon => write!(f, "::"),
            TokenType::Dot => write!(f, "."),
            TokenType::Pipe => write!(f, "|"),

            TokenType::DoubleQuote => write!(f, "\""),
            TokenType::DollarSign => write!(f, "$"),
            TokenType::Illegal => write!(f, "ILLEGAL"),
            TokenType::EOF => write!(f, "EOF"),
            TokenType::BadToken => write!(f, "BAD_TOKEN"),
        }
    }
}
