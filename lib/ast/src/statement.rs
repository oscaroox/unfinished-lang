use crate::{Expression, Let};

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    // TODO just make everything an expression
    // let statements should just return the unit type
    Let(Let),
    Expr(Expression),
}

impl Statement {
    pub fn create_let(name: String, value: Option<Expression>) -> Statement {
        Statement::Let(Let { id: name, value })
    }
    pub fn create_expr(expr: Expression) -> Statement {
        Statement::Expr(expr)
    }
}
