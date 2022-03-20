use crate::{
    Assign, BinOp, BinaryOperation, Block, BreakExpr, Call, ContinueExpr, DataStruct,
    DataStructInstance, DataStructInstanceField, Function, GetIndex, GetProperty, Grouping,
    Identifier, IfConditional, ImplicitReturn, LetExpr, LetRef, Literal, Logic, LogicOperation,
    LoopExpr, ReturnExpr, SelfExpr, SetIndex, SetProperty, Type, UnaryOp, UnaryOperation,
};
use span_util::{Span, WithSpan};

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    BinOp(BinOp),
    Literal(WithSpan<Literal>),
    Assign(Assign),
    GetIndex(GetIndex),
    SetIndex(SetIndex),
    GetProperty(GetProperty),
    SetProperty(SetProperty),
    Let(WithSpan<LetExpr>),
    LetRef(WithSpan<LetRef>),
    UnaryOp(UnaryOp),
    Grouping(WithSpan<Grouping>),
    Logic(WithSpan<Logic>),
    Call(Call),
    Function(Function),
    DataStruct(DataStruct),
    DataStructInstance(WithSpan<DataStructInstance>),
    Block(WithSpan<Block>),
    If(IfConditional),
    ImplicitReturn(WithSpan<ImplicitReturn>),
    Return(ReturnExpr),
    SelfExpr(WithSpan<SelfExpr>),
    LoopExpr(WithSpan<LoopExpr>),
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

    pub fn is_array_lit(&self) -> bool {
        match self {
            Expression::Literal(lit) => match lit.0 {
                Literal::Array(_) => true,
                _ => false,
            },
            _ => false,
        }
    }

    pub fn is_function(&self) -> bool {
        match self {
            Expression::Function(_) => true,
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
        Expression::BinOp(BinOp {
            left: Box::new(left),
            op,
            right: Box::new(right),
            span,
        })
    }

    pub fn create_literal(lit: Literal, span: Span) -> Expression {
        Expression::Literal(WithSpan(lit, span))
    }

    pub fn create_assign(name: Identifier, rhs: Expression, span: Span) -> Expression {
        Expression::Assign(Assign {
            name,
            rhs: Box::new(rhs),
            span,
        })
    }

    pub fn create_let_ref(ident: Identifier, span: Span) -> Expression {
        Expression::LetRef(WithSpan(LetRef { name: ident }, span))
    }

    pub fn create_unaryop(op: UnaryOperation, rhs: Expression, span: Span) -> Expression {
        Expression::UnaryOp(UnaryOp {
            op,
            rhs: Box::new(rhs),
            span,
        })
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
        Expression::Call(Call {
            callee: Box::new(callee),
            arguments: args,
            span,
        })
    }

    pub fn create_index(lhs: Expression, index: Expression, span: Span) -> Expression {
        Expression::GetIndex(GetIndex {
            lhs: Box::new(lhs),
            index: Box::new(index),
            span,
        })
    }

    pub fn create_set_index(
        lhs: Expression,
        index: Expression,
        value: Expression,
        span: Span,
    ) -> Expression {
        Expression::SetIndex(SetIndex {
            lhs: Box::new(lhs),
            index: Box::new(index),
            value: Box::new(value),
            span,
        })
    }

    pub fn create_get_property(
        object: Expression,
        name: Identifier,
        is_callable: bool,
        span: Span,
    ) -> Expression {
        Expression::GetProperty(GetProperty {
            object: Box::new(object),
            is_callable,
            name,
            span,
        })
    }

    pub fn create_set_property(
        object: Expression,
        name: Identifier,
        value: Expression,
        span: Span,
    ) -> Expression {
        Expression::SetProperty(SetProperty {
            object: Box::new(object),
            name,
            value: Box::new(value),
            span,
        })
    }

    pub fn create_function(
        name: Option<String>,
        params: Vec<Identifier>,
        return_type: Type,
        is_static: bool,
        body: Expression,
        span: Span,
    ) -> Expression {
        Expression::Function(Function {
            name,
            params,
            return_type,
            body: Box::new(body),
            is_static,
            span,
        })
    }

    pub fn create_if(
        condition: Expression,
        then: Expression,
        not_then: Option<Expression>,
        span: Span,
        if_token_span: Span,
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
            if_token: if_token_span,
            span,
        })
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

    pub fn create_return(
        value: Option<Expression>,
        span: Span,
        return_token_span: Span,
    ) -> Expression {
        Expression::Return(ReturnExpr {
            value: Box::new(value),
            span,
            return_token: return_token_span,
        })
    }

    pub fn create_implicit_return(value: Expression, span: Span) -> Expression {
        Expression::ImplicitReturn(WithSpan(
            ImplicitReturn {
                value: Box::new(value),
            },
            span,
        ))
    }

    pub fn create_data_struct(
        name: Identifier,
        fields: Vec<Identifier>,
        methods: Vec<Expression>,
        span: Span,
    ) -> Expression {
        Expression::DataStruct(DataStruct {
            fields,
            name,
            methods,
            span,
        })
    }

    pub fn create_data_struct_instance(
        name: Identifier,
        fields: Vec<DataStructInstanceField>,
        span: Span,
    ) -> Expression {
        Expression::DataStructInstance(WithSpan(DataStructInstance { name, fields }, span))
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
        Expression::BreakExpr(BreakExpr { span })
    }

    pub fn create_continue(span: Span) -> Expression {
        Expression::ContinueExpr(ContinueExpr { span })
    }

    pub fn get_span(&self) -> Span {
        match &self {
            Expression::BinOp(s) => s.span.clone(),
            Expression::Literal(s) => s.1.clone(),
            Expression::Assign(s) => s.span.clone(),
            Expression::GetIndex(s) => s.span.clone(),
            Expression::SetIndex(s) => s.span.clone(),
            Expression::GetProperty(s) => s.span.clone(),
            Expression::SetProperty(s) => s.span.clone(),
            Expression::Let(s) => s.1.clone(),
            Expression::LetRef(s) => s.1.clone(),
            Expression::UnaryOp(s) => s.span.clone(),
            Expression::Grouping(s) => s.1.clone(),
            Expression::Logic(s) => s.1.clone(),
            Expression::Call(s) => s.span.clone(),
            Expression::Function(s) => s.span.clone(),
            Expression::DataStruct(s) => s.span.clone(),
            Expression::DataStructInstance(s) => s.1.clone(),
            Expression::Block(s) => s.1.clone(),
            Expression::If(s) => s.span.clone(),
            Expression::ImplicitReturn(s) => s.1.clone(),
            Expression::Return(s) => s.span.clone(),
            Expression::SelfExpr(s) => s.1.clone(),
            Expression::LoopExpr(s) => s.1.clone(),
            Expression::BreakExpr(s) => s.span.clone(),
            Expression::ContinueExpr(s) => s.span.clone(),
        }
    }
}
