use span_util::Span;

use crate::{Expression, Identifier};

#[derive(Debug, PartialEq, Clone)]
pub struct DataStructInstanceField {
    pub name: Identifier,
    pub value: Expression,
}

impl DataStructInstanceField {
    pub fn new(name: Identifier, value: Expression) -> DataStructInstanceField {
        DataStructInstanceField { name, value }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct DataStructInstance {
    pub name: Identifier,
    pub fields: Vec<DataStructInstanceField>,
    pub span: Span,
}
