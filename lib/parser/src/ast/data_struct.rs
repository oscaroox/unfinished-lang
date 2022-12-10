use span_util::Span;

use super::{Expression, Identifier};

#[derive(Debug, PartialEq, Clone)]
pub struct DataStruct {
    pub name: Identifier,
    // TODO add spans to fields
    pub fields: Vec<Identifier>,
    pub methods: Vec<Expression>,
    pub span: Span,
}
