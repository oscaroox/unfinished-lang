use crate::Expression;

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Array(Vec<Expression>),
    Null,
}
