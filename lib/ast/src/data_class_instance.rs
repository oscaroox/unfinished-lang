use crate::{Expression, Identifier};

#[derive(Debug, PartialEq, Clone)]
pub struct DataClassInstanceField {
    pub name: Identifier,
    pub value: Expression,
}

impl DataClassInstanceField {
    pub fn new(name: Identifier, value: Expression) -> DataClassInstanceField {
        DataClassInstanceField { name, value }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct DataClassInstance {
    pub name: Identifier,
    pub fields: Vec<DataClassInstanceField>,
}
