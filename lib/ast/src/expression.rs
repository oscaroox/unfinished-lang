use crate::{
    Assign, BinOp, BinaryOperation, Block, BreakExpr, Call, ContinueExpr, DataClass,
    DataClassInstance, DataClassInstanceField, Function, GetProperty, Grouping, Identifier,
    IfConditional, ImplicitReturn, Index, LetExpr, LetRef, Literal, Logic, LogicOperation,
    LoopExpr, ReturnExpr, SelfExpr, SetIndex, SetProperty, UnaryOp, UnaryOperation,
};
use span_util::{Span, WithSpan};
use std::ops::Range;

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    BinOp(WithSpan<BinOp>),
    Literal(WithSpan<Literal>),
    Assign(WithSpan<Assign>),
    Index(WithSpan<Index>),
    SetIndex(WithSpan<SetIndex>),
    GetProperty(WithSpan<GetProperty>),
    SetProperty(WithSpan<SetProperty>),
    Let(WithSpan<LetExpr>),
    LetRef(WithSpan<LetRef>),
    UnaryOp(WithSpan<UnaryOp>),
    Grouping(WithSpan<Grouping>),
    Logic(WithSpan<Logic>),
    Call(WithSpan<Call>),
    Function(WithSpan<Function>),
    DataClass(WithSpan<DataClass>),
    DataClassInstance(WithSpan<DataClassInstance>),
    Block(WithSpan<Block>),
    If(WithSpan<IfConditional>),
    ImplicitReturn(WithSpan<ImplicitReturn>),
    Return(WithSpan<ReturnExpr>),
    SelfExpr(WithSpan<SelfExpr>),
    LoopExpr(WithSpan<LoopExpr>),
    BreakExpr(WithSpan<BreakExpr>),
    ContinueExpr(WithSpan<ContinueExpr>),
}

impl Expression {
    pub fn to_assign(&self) -> Assign {
        match &self {
            Expression::Assign(e) => e.0.clone(),
            _ => panic!("Cannot cast to assign"),
        }
    }

    pub fn to_let(&self) -> WithSpan<LetExpr> {
        match &self {
            Expression::Let(e) => e.clone(),
            _ => panic!("Cannot cast to assign"),
        }
    }

    pub fn to_let_ref(&self) -> LetRef {
        match &self {
            Expression::LetRef(e) => e.0.clone(),
            _ => panic!("Cannot cast to assign"),
        }
    }

    pub fn is_string_lit(&self) -> bool {
        match self {
            Expression::Literal(lit) => match lit.0 {
                Literal::String(_) => true,
                _ => false,
            },
            _ => false,
        }
    }

    pub fn create_let(name: Identifier, value: Option<Expression>, span: Span) -> Expression {
        let value = if let Some(e) = value {
            Some(Box::new(e))
        } else {
            None
        };
        Expression::Let(WithSpan(LetExpr { name, value }, span))
    }

    pub fn create_binop(
        left: Expression,
        op: BinaryOperation,
        right: Expression,
        span: Span,
    ) -> Expression {
        Expression::BinOp(WithSpan(
            BinOp {
                left: Box::new(left),
                op,
                right: Box::new(right),
            },
            span,
        ))
    }

    pub fn create_literal(lit: Literal, span: Span) -> Expression {
        Expression::Literal(WithSpan(lit, span))
    }

    pub fn create_assign(name: Identifier, rhs: Expression, span: Span) -> Expression {
        Expression::Assign(WithSpan(
            Assign {
                name,
                rhs: Box::new(rhs),
            },
            span,
        ))
    }

    pub fn create_let_ref(ident: Identifier, span: Span) -> Expression {
        Expression::LetRef(WithSpan(LetRef { name: ident }, span))
    }

    pub fn create_unaryop(op: UnaryOperation, rhs: Expression, span: Span) -> Expression {
        Expression::UnaryOp(WithSpan(
            UnaryOp {
                op,
                rhs: Box::new(rhs),
            },
            span,
        ))
    }

    pub fn create_grouping(expr: Expression, span: Span) -> Expression {
        Expression::Grouping(WithSpan(
            Grouping {
                expr: Box::new(expr),
            },
            span,
        ))
    }

    pub fn create_call(callee: Expression, args: Vec<Expression>, span: Span) -> Expression {
        Expression::Call(WithSpan(
            Call {
                callee: Box::new(callee),
                arguments: args,
            },
            span,
        ))
    }

    pub fn create_index(lhs: Expression, index: Expression, span: Span) -> Expression {
        Expression::Index(WithSpan(
            Index {
                lhs: Box::new(lhs),
                index: Box::new(index),
            },
            span,
        ))
    }

    pub fn create_set_index(
        lhs: Expression,
        index: Expression,
        value: Expression,
        span: Span,
    ) -> Expression {
        Expression::SetIndex(WithSpan(
            SetIndex {
                lhs: Box::new(lhs),
                index: Box::new(index),
                value: Box::new(value),
            },
            span,
        ))
    }

    pub fn create_get_property(
        object: Expression,
        name: Identifier,
        is_callable: bool,
        span: Span,
    ) -> Expression {
        Expression::GetProperty(WithSpan(
            GetProperty {
                object: Box::new(object),
                is_callable,
                name,
            },
            span,
        ))
    }

    pub fn create_set_property(
        object: Expression,
        name: Identifier,
        value: Expression,
        span: Span,
    ) -> Expression {
        Expression::SetProperty(WithSpan(
            SetProperty {
                object: Box::new(object),
                name,
                value: Box::new(value),
            },
            span,
        ))
    }

    pub fn create_function(
        name: Option<String>,
        params: Vec<Identifier>,
        is_static: bool,
        body: Expression,
        span: Span,
    ) -> Expression {
        Expression::Function(WithSpan(
            Function {
                name,
                params,
                body: Box::new(body),
                is_static,
            },
            span,
        ))
    }

    pub fn create_if(
        condition: Expression,
        then: Expression,
        not_then: Option<Expression>,
        span: Span,
    ) -> Expression {
        let not_then = if let Some(e) = not_then {
            Some(Box::new(e))
        } else {
            None
        };
        Expression::If(WithSpan(
            IfConditional {
                condition: Box::new(condition),
                then: Box::new(then),
                not_then,
            },
            span,
        ))
    }

    pub fn create_logic(
        lhs: Expression,
        op: LogicOperation,
        rhs: Expression,
        span: Span,
    ) -> Expression {
        Expression::Logic(WithSpan(
            Logic {
                lhs: Box::new(lhs),
                op,
                rhs: Box::new(rhs),
            },
            span,
        ))
    }

    pub fn create_block(exprs: Vec<Expression>, span: Span) -> Expression {
        Expression::Block(WithSpan(Block { exprs }, span))
    }

    pub fn create_return(value: Option<Expression>, span: Range<usize>) -> Expression {
        Expression::Return(WithSpan(
            ReturnExpr {
                value: Box::new(value),
            },
            span.into(),
        ))
    }

    pub fn create_implicit_return(value: Expression, span: Span) -> Expression {
        Expression::ImplicitReturn(WithSpan(
            ImplicitReturn {
                value: Box::new(value),
            },
            span,
        ))
    }

    pub fn create_data_class(
        name: Identifier,
        fields: Vec<Identifier>,
        methods: Vec<Expression>,
        span: Span,
    ) -> Expression {
        Expression::DataClass(WithSpan(
            DataClass {
                fields,
                name,
                methods,
            },
            span,
        ))
    }

    pub fn create_data_class_instance(
        name: Identifier,
        fields: Vec<DataClassInstanceField>,
        span: Span,
    ) -> Expression {
        Expression::DataClassInstance(WithSpan(DataClassInstance { name, fields }, span))
    }

    pub fn create_self(name: String, span: Span) -> Expression {
        Expression::SelfExpr(WithSpan(SelfExpr { name }, span))
    }

    pub fn create_loop(
        condition: Expression,
        body: Expression,
        iterator: Option<Expression>,
        span: Span,
    ) -> Expression {
        let iterator = if let Some(e) = iterator {
            Some(Box::new(e))
        } else {
            None
        };
        Expression::LoopExpr(WithSpan(
            LoopExpr {
                body: Box::new(body),
                condition: Box::new(condition),
                iterator,
            },
            span,
        ))
    }

    pub fn create_break(span: Span) -> Expression {
        Expression::BreakExpr(WithSpan(BreakExpr {}, span))
    }

    pub fn create_continue(span: Span) -> Expression {
        Expression::ContinueExpr(WithSpan(ContinueExpr {}, span))
    }
}
