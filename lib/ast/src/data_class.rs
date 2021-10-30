use crate::Identifier;

#[derive(Debug, PartialEq, Clone)]
pub struct DataClass {
    pub name: Identifier,
    pub fields: Vec<Identifier>,
}
