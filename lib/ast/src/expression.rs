use crate::{BinOp, BinaryOperation, Literal};

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    BinOp(BinOp),
    Literal(Literal),
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

    pub fn create_literal(lit: Literal) -> Expression {
        Expression::Literal(lit)
    }
}
