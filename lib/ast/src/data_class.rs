use crate::{Expression, Identifier};

#[derive(Debug, PartialEq, Clone)]
pub struct DataClass {
    pub name: Identifier,
    pub fields: Vec<Identifier>,
    pub methods: Vec<Expression>,
}
