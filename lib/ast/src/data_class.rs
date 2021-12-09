use crate::{Expression, Identifier};

#[derive(Debug, PartialEq, Clone)]
pub struct DataClass {
    pub name: Identifier,
    // TODO add spans to fields
    pub fields: Vec<Identifier>,
    pub methods: Vec<Expression>,
}
