use crate::{Expression, Let};

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Let(Let),
    Expr(Expression),
}

impl Statement {
    pub fn create_let(name: String, value: Option<Expression>) -> Statement {
        Statement::Let(Let { id: name, value })
    }
    pub fn create_expression(expr: Expression) -> Statement {
        Statement::Expr(expr)
    }
}
