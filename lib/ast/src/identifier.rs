use scanner::TokenType;

#[derive(Debug, PartialEq, Clone)]
pub struct Identifier {
    pub value: String,
    token_type: Option<TokenType>,
}

impl Identifier {
    pub fn new(value: String) -> Identifier {
        Identifier {
            value,
            token_type: None,
        }
    }

    pub fn with_token_type(value: String, token_type: TokenType) -> Identifier {
        Identifier {
            value,
            token_type: Some(token_type),
        }
    }
    pub fn is_self(&self) -> bool {
        match &self.token_type {
            Some(tt) => TokenType::SELF == *tt,
            None => false,
        }
    }
}

impl std::fmt::Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}
