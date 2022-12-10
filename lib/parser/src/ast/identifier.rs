use crate::scanner::{TokenType};

use span_util::Span;

use type_core::Type;

#[derive(Debug, PartialEq, Clone)]
pub struct Identifier {
    pub value: String,
    pub token_type: Option<TokenType>,
    pub value_type: Option<Type>,
    pub span: Span,
}

impl Identifier {
    pub fn new(value: String, span: Span) -> Identifier {
        Identifier {
            value,
            token_type: None,
            value_type: None,
            span,
        }
    }

    pub fn with_all(
        value: String,
        token_type: TokenType,
        value_type: Type,
        span: Span,
    ) -> Identifier {
        Identifier {
            value,
            value_type: Some(value_type),
            token_type: Some(token_type),
            span,
        }
    }

    pub fn with_token_type(value: String, token_type: TokenType, span: Span) -> Identifier {
        Identifier {
            value,
            value_type: None,
            token_type: Some(token_type),
            span,
        }
    }

    pub fn with_value_type(value: String, value_type: Option<Type>, span: Span) -> Identifier {
        Identifier {
            value,
            value_type,
            token_type: None,
            span,
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

#[cfg(test)]
pub mod test {
    use span_util::Span;

    use super::Identifier;
    use type_core::Type;

    pub fn ident(name: &str) -> Identifier {
        Identifier::new(name.into(), Span::fake())
    }

    pub fn ident_string(name: &str) -> Identifier {
        Identifier::with_value_type(name.into(), Some(Type::string()), Span::fake())
    }

    pub fn ident_bool(name: &str) -> Identifier {
        Identifier::with_value_type(name.into(), Some(Type::bool()), Span::fake())
    }

    pub fn ident_int(name: &str) -> Identifier {
        Identifier::with_value_type(name.into(), Some(Type::int()), Span::fake())
    }

    pub fn ident_float(name: &str) -> Identifier {
        Identifier::with_value_type(name.into(), Some(Type::float()), Span::fake())
    }
}
