use scanner::TokenType;

#[derive(Debug, PartialEq, Clone)]
pub enum EType {
    Int,
    Float,
    String,
    Bool,
    Unit,
    // Fun(Vec<Type>, Type)
    User(String),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Type {
    pub is_array: bool,
    pub etype: EType,
}

impl Type {
    pub fn new(etype: EType, is_array: bool) -> Type {
        Type { is_array, etype }
    }
}
