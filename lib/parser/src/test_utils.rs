use span_util::Span;
use type_core::{Type, FunctionParam};

use crate::scanner::TokenType;
use ast::{Expression, Identifier, CallArgs, LiteralValue, BinaryOperation, UnaryOperation, LogicOperation};

pub fn create_let(id: &str, value: Option<Expression>) -> Expression {
    Expression::create_let(ident(id), value, Span::fake(), Span::fake())
}

pub fn create_let_type(id: Identifier, value: Option<Expression>) -> Expression {
    Expression::create_let(id, value, Span::fake(), Span::fake())
}

pub fn arg(arg: Expression) -> CallArgs {
    CallArgs(None, arg)
}

pub fn named_arg(name: Identifier, arg: Expression) -> CallArgs {
    CallArgs(Some(name), arg)
}

pub fn int(val: i64) -> Expression {
    Expression::create_literal(LiteralValue::Int(val), Span::fake())
}

pub fn float(val: f64) -> Expression {
    Expression::create_literal(LiteralValue::Float(val), Span::fake())
}

pub fn array(val: Vec<Expression>) -> Expression {
    Expression::create_literal(LiteralValue::Array(val), Span::fake())
}

pub fn null() -> Expression {
    Expression::create_literal(LiteralValue::Null, Span::fake())
}

pub fn bool_lit(val: bool) -> Expression {
    Expression::create_literal(LiteralValue::Bool(val), Span::fake())
}

pub fn string_lit(val: &str) -> Expression {
    Expression::create_literal(LiteralValue::String(val.into()), Span::fake())
}

pub fn unit_type() -> Type {
    Type::unit()
}

pub fn ident(val: &str) -> Identifier {
    Identifier::new(val.to_string(), Span::fake())
}

pub fn ident_type(val: &str, value_type: Type) -> Identifier {
    Identifier::with_value_type(val.into(), Some(value_type), Span::fake())
}

pub fn ident_self(r#type: &str) -> Identifier {
    Identifier {
        value: "self".to_string(),
        // token_type: Some(TokenType::SELF),
        value_type: Some(Type::Identifier(r#type.into())),
        span: Span::fake(),
    }
}

pub fn create_binop(left: Expression, op: BinaryOperation, right: Expression) -> Expression {
    Expression::create_binop(left, op, right, Span::fake())
}

pub fn create_let_ref(val: &str) -> Expression {
    create_let_ref_with_scope(val, None)
}

pub fn create_let_ref_with_scope(val: &str, scope: Option<usize>) -> Expression {
    Expression::create_let_ref(ident(val), Span::fake(), scope)
}

pub fn create_block(exprs: Vec<Expression>) -> Expression {
    Expression::create_block(exprs, Span::fake())
}

pub fn create_return(val: Option<Expression>) -> Expression {
    Expression::create_return(val, Span::fake(), Span::fake())
}

pub fn create_implicit_return(val: Expression) -> Expression {
    Expression::create_implicit_return(val, Span::fake())
}

pub fn create_data_struct(
    name: &str,
    fields: Vec<(&str, Type)>,
    methods: Vec<Expression>,
) -> Expression {
    Expression::create_data_struct(
        Identifier::new(name.to_string(), Span::fake()),
        fields
            .iter()
            .map(|v| ident_type(v.0, v.1.clone()))
            .collect(),
        methods,
        Span::fake(),
    )
}


pub fn create_param_type(name: &str, ttype: Type) -> FunctionParam {
    FunctionParam { name: name.to_string(), ttype }
}

pub fn create_assign(name: &str, rhs: Expression) -> Expression {
    create_assign_with_scope(name, rhs, None)
}

pub fn create_assign_with_scope(name: &str, rhs: Expression, scope: Option<usize>) -> Expression {
    Expression::create_assign(ident(name), rhs, Span::fake(), scope)
}

pub fn create_index(lhs: Expression, index: Expression) -> Expression {
    Expression::create_index(lhs, index, Span::fake())
}

pub fn create_unaryop(op: UnaryOperation, rhs: Expression) -> Expression {
    Expression::create_unaryop(op, rhs, Span::fake())
}

pub fn create_grouping(expr: Expression) -> Expression {
    Expression::create_grouping(expr, Span::fake())
}

pub fn create_logic(lhs: Expression, op: LogicOperation, rhs: Expression) -> Expression {
    Expression::create_logic(lhs, op, rhs, Span::fake())
}

pub fn create_call(callee: Expression, args: Vec<CallArgs>) -> Expression {
    Expression::create_call(callee, args, Span::fake())
}

pub fn create_self(name: &str) -> Expression {
    Expression::create_self(name.into(), Span::fake())
}

pub fn create_break() -> Expression {
    Expression::create_break(Span::fake())
}

pub fn create_continue() -> Expression {
    Expression::create_continue(Span::fake())
}

pub fn create_loop(
    condition: Expression,
    body: Expression,
    iterator: Option<Expression>,
) -> Expression {
    Expression::create_loop(condition, body, iterator, Span::fake(), Span::fake())
}

pub fn create_if(
    condition: Expression,
    then: Expression,
    not_then: Option<Expression>,
) -> Expression {
    Expression::create_if(condition, then, not_then, Span::fake(), Span::fake())
}

pub fn create_set_index(lhs: Expression, index: Expression, value: Expression) -> Expression {
    Expression::create_set_index(lhs, index, value, Span::fake())
}

pub fn create_get_property(object: Expression, name: Identifier, is_callable: bool) -> Expression {
    Expression::create_get_property(object, name, is_callable, Span::fake())
}

pub fn create_set_property(object: Expression, name: Identifier, value: Expression) -> Expression {
    Expression::create_set_property(object, name, value, Span::fake())
}

pub fn create_function(
    name: Option<String>,
    params: Vec<Identifier>,
    return_type: Type,
    is_static: bool,
    body: Expression,
) -> Expression {
    Expression::create_function(name, params, return_type, is_static, body, Span::fake())
}
