use crate::{
    Assign, BinOp, BinaryOperation, Call, Function, Grouping, Identifier, LetRef, Literal,
    Statement, UnaryOp, UnaryOperation,
};

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    BinOp(BinOp),
    Literal(Literal),
    Assign(Assign),
    LetRef(LetRef),
    UnaryOp(UnaryOp),
    Grouping(Grouping),
    Call(Call),
    Function(Function),
    Empty,
}

impl Expression {
    pub fn create_binop(left: Expression, op: BinaryOperation, right: Expression) -> Expression {
        Expression::BinOp(BinOp {
            left: Box::new(left),
            op,
            right: Box::new(right),
        })
    }

    pub fn create_literal(lit: Literal) -> Expression {
        Expression::Literal(lit)
    }

    pub fn create_assign(name: Identifier, rhs: Expression) -> Expression {
        Expression::Assign(Assign {
            name,
            rhs: Box::new(rhs),
        })
    }

    pub fn create_let_ref(ident: Identifier) -> Expression {
        Expression::LetRef(LetRef { name: ident })
    }

    pub fn create_unaryop(op: UnaryOperation, rhs: Expression) -> Expression {
        Expression::UnaryOp(UnaryOp {
            op,
            rhs: Box::new(rhs),
        })
    }

    pub fn create_grouping(expr: Expression) -> Expression {
        Expression::Grouping(Grouping {
            expr: Box::new(expr),
        })
    }

    pub fn create_call(name: Expression, args: Vec<Expression>) -> Expression {
        Expression::Call(Call {
            name: Box::new(name),
            arguments: args,
        })
    }

    pub fn create_function(params: Vec<Identifier>, body: Vec<Statement>) -> Expression {
        Expression::Function(Function { params, body })
    }
}
