use scanner::TokenType;

use crate::Type;

#[derive(Debug, PartialEq, Clone)]
pub struct Identifier {
    pub value: String,
    pub token_type: Option<TokenType>,
    pub value_type: Option<Type>,
}

impl Identifier {
    pub fn new(value: String) -> Identifier {
        Identifier {
            value,
            token_type: None,
            value_type: None,
        }
    }

    pub fn with_all(value: String, token_type: TokenType, value_type: Type) -> Identifier {
        Identifier {
            value,
            value_type: Some(value_type),
            token_type: Some(token_type),
        }
    }

    pub fn with_token_type(value: String, token_type: TokenType) -> Identifier {
        Identifier {
            value,
            value_type: None,
            token_type: Some(token_type),
        }
    }

    pub fn with_value_type(value: String, value_type: Option<Type>) -> Identifier {
        Identifier {
            value,
            value_type,
            token_type: None,
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
