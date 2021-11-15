use crate::{
    Assign, BinOp, BinaryOperation, Block, BreakExpr, Call, ContinueExpr, DataClass,
    DataClassInstance, DataClassInstanceField, Function, GetProperty, Grouping, Identifier,
    IfConditional, ImplicitReturn, Index, LetExpr, LetRef, Literal, Logic, LogicOperation,
    LoopExpr, ReturnExpr, SelfExpr, SetIndex, SetProperty, UnaryOp, UnaryOperation,
};

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    BinOp(BinOp),
    Literal(Literal),
    Assign(Assign),
    Index(Index),
    SetIndex(SetIndex),
    GetProperty(GetProperty),
    SetProperty(SetProperty),
    Let(LetExpr),
    LetRef(LetRef),
    UnaryOp(UnaryOp),
    Grouping(Grouping),
    Logic(Logic),
    Call(Call),
    Function(Function),
    DataClass(DataClass),
    DataClassInstance(DataClassInstance),
    Block(Block),
    If(IfConditional),
    ImplicitReturn(ImplicitReturn),
    Return(ReturnExpr),
    SelfExpr(SelfExpr),
    LoopExpr(LoopExpr),
    BreakExpr(BreakExpr),
    ContinueExpr(ContinueExpr),
}

impl Expression {
    pub fn to_assign(&self) -> Assign {
        match &self {
            Expression::Assign(e) => e.clone(),
            _ => panic!("Cannot cast to assign"),
        }
    }

    pub fn to_let(&self) -> LetExpr {
        match &self {
            Expression::Let(e) => e.clone(),
            _ => panic!("Cannot cast to assign"),
        }
    }

    pub fn to_let_ref(&self) -> LetRef {
        match &self {
            Expression::LetRef(e) => e.clone(),
            _ => panic!("Cannot cast to assign"),
        }
    }

    pub fn is_string_lit(&self) -> bool {
        match self {
            Expression::Literal(Literal::String(_)) => true,
            _ => false,
        }
    }

    pub fn create_let(name: Identifier, value: Option<Expression>) -> Expression {
        let value = if let Some(e) = value {
            Some(Box::new(e))
        } else {
            None
        };
        Expression::Let(LetExpr { name, value })
    }

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

    pub fn create_get_property(
        object: Expression,
        name: Identifier,
        is_callable: bool,
    ) -> Expression {
        Expression::GetProperty(GetProperty {
            object: Box::new(object),
            is_callable,
            name,
        })
    }

    pub fn create_set_property(
        object: Expression,
        name: Identifier,
        value: Expression,
    ) -> Expression {
        Expression::SetProperty(SetProperty {
            object: Box::new(object),
            name,
            value: Box::new(value),
        })
    }

    pub fn create_function(
        name: Option<String>,
        params: Vec<Identifier>,
        is_static: bool,
        body: Expression,
    ) -> Expression {
        Expression::Function(Function {
            name,
            params,
            body: Box::new(body),
            is_static,
        })
    }

    pub fn create_if(
        condition: Expression,
        then: Expression,
        not_then: Option<Expression>,
    ) -> Expression {
        let not_then = if let Some(e) = not_then {
            Some(Box::new(e))
        } else {
            None
        };
        Expression::If(IfConditional {
            condition: Box::new(condition),
            then: Box::new(then),
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

    pub fn create_block(exprs: Vec<Expression>) -> Expression {
        Expression::Block(Block { exprs })
    }

    pub fn create_return(value: Option<Expression>) -> Expression {
        Expression::Return(ReturnExpr {
            value: Box::new(value),
        })
    }

    pub fn create_implicit_return(value: Expression) -> Expression {
        Expression::ImplicitReturn(ImplicitReturn {
            value: Box::new(value),
        })
    }

    pub fn create_data_class(
        name: Identifier,
        fields: Vec<Identifier>,
        methods: Vec<Expression>,
    ) -> Expression {
        Expression::DataClass(DataClass {
            fields,
            name,
            methods,
        })
    }

    pub fn create_data_class_instance(
        name: Identifier,
        fields: Vec<DataClassInstanceField>,
    ) -> Expression {
        Expression::DataClassInstance(DataClassInstance { name, fields })
    }

    pub fn create_self(name: String) -> Expression {
        Expression::SelfExpr(SelfExpr { name })
    }

    pub fn create_loop(
        condition: Expression,
        body: Expression,
        iterator: Option<Expression>,
    ) -> Expression {
        let iterator = if let Some(e) = iterator {
            Some(Box::new(e))
        } else {
            None
        };
        Expression::LoopExpr(LoopExpr {
            body: Box::new(body),
            condition: Box::new(condition),
            iterator,
        })
    }

    pub fn create_break() -> Expression {
        Expression::BreakExpr(BreakExpr {})
    }

    pub fn create_continue() -> Expression {
        Expression::ContinueExpr(ContinueExpr {})
    }
}
