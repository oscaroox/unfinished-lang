use crate::BinOp;

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    BinOp(BinOp),
}
