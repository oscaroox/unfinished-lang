use crate::{Expression, Let};

#[derive(Debug)]
pub enum Statement {
    Let(Let),
    Expr(Expression),
}
