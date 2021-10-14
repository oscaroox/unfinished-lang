use crate::{BinOp, BinaryOperation};

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    BinOp(BinOp),
    Empty,
}

impl Expression {
    pub fn create_binop() -> Expression {
        Expression::BinOp(BinOp {
            left: Box::new(Expression::Empty),
            op: BinaryOperation::Add,
            right: Box::new(Expression::Empty),
        })
    }
}
