use crate::{
    Assign, BinOp, BinaryOperation, Block, Call, Function, Grouping, Identifier, IfConditional,
    Index, LetRef, Literal, Logic, LogicOperation, ReturnExpr, SetIndex, Statement, UnaryOp,
    UnaryOperation,
};

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    BinOp(BinOp),
    Literal(Literal),
    Assign(Assign),
    Index(Index),
    SetIndex(SetIndex),
    LetRef(LetRef),
    UnaryOp(UnaryOp),
    Grouping(Grouping),
    Logic(Logic),
    Call(Call),
    Function(Function),
    Block(Block),
    If(IfConditional),
    Return(ReturnExpr),
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

    pub fn create_call(callee: Expression, args: Vec<Expression>) -> Expression {
        Expression::Call(Call {
            callee: Box::new(callee),
            arguments: args,
        })
    }

    pub fn create_index(lhs: Expression, index: Expression) -> Expression {
        Expression::Index(Index {
            lhs: Box::new(lhs),
            index: Box::new(index),
        })
    }

    pub fn create_set_index(lhs: Expression, index: Expression, value: Expression) -> Expression {
        Expression::SetIndex(SetIndex {
            lhs: Box::new(lhs),
            index: Box::new(index),
            value: Box::new(value),
        })
    }

    pub fn create_function(
        name: Option<String>,
        params: Vec<Identifier>,
        body: Vec<Statement>,
    ) -> Expression {
        Expression::Function(Function { name, params, body })
    }

    pub fn create_if(
        condition: Expression,
        then: Vec<Statement>,
        not_then: Option<Vec<Statement>>,
    ) -> Expression {
        Expression::If(IfConditional {
            condition: Box::new(condition),
            then,
            not_then,
        })
    }

    pub fn create_logic(lhs: Expression, op: LogicOperation, rhs: Expression) -> Expression {
        Expression::Logic(Logic {
            lhs: Box::new(lhs),
            op,
            rhs: Box::new(rhs),
        })
    }

    pub fn create_block(stmts: Vec<Statement>) -> Expression {
        Expression::Block(Block { stmts })
    }

    pub fn create_return(value: Option<Expression>) -> Expression {
        Expression::Return(ReturnExpr {
            value: Box::new(value),
        })
    }
}
