use crate::Identifier;

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Int,
    Float,
    String,
    Bool,
    Unit,
    Fun(Vec<Identifier>, Box<Type>),
    Identifier(String),
    Array(Box<Type>),
}

impl Type {
    pub fn int() -> Type {
        Type::Int
    }
    pub fn float() -> Type {
        Type::Float
    }
    pub fn string() -> Type {
        Type::String
    }
    pub fn bool() -> Type {
        Type::Bool
    }
    pub fn unit() -> Type {
        Type::Unit
    }
    pub fn fun(params: Vec<Identifier>, return_type: Type) -> Type {
        Type::Fun(params, Box::new(return_type))
    }

    pub fn identifier(name: impl Into<String>) -> Type {
        Type::Identifier(name.into())
    }

    pub fn array(r#type: Type) -> Type {
        Type::Array(Box::new(r#type))
    }
}
