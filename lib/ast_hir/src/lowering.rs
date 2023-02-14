fn lower_binop_kind(op: ast::BinaryOperation) -> hir::BinOpKind {
    match op {
        ast::BinaryOperation::Add(s) => hir::BinOpKind::Add(s),
        ast::BinaryOperation::ConcatInterpolation => hir::BinOpKind::Concat,
        ast::BinaryOperation::Subtract(s) => hir::BinOpKind::Sub(s),
        ast::BinaryOperation::Multiply(s) => hir::BinOpKind::Mul(s),
        ast::BinaryOperation::Divide(s) => hir::BinOpKind::Div(s),
    }
}

fn lower_unaryop_kind(op: ast::UnaryOperation) -> hir::UnaryOpKind {
    match op {
        ast::UnaryOperation::Minus(s) => hir::UnaryOpKind::Negate(s),
        ast::UnaryOperation::Not(s) => hir::UnaryOpKind::Not(s),
        ast::UnaryOperation::Plus(_) => todo!(),
    }
}

fn lower_logicop_kind(op: ast::LogicOperation) -> hir::LogicOpKind {
    match op {
        ast::LogicOperation::Or(s) => hir::LogicOpKind::Or(s),
        ast::LogicOperation::And(s) => hir::LogicOpKind::And(s),
        ast::LogicOperation::Equal(s) => hir::LogicOpKind::Eq(s),
        ast::LogicOperation::NotEqual(s) => hir::LogicOpKind::Ne(s),
        ast::LogicOperation::LessThan(s) => hir::LogicOpKind::Lt(s),
        ast::LogicOperation::LessThanEqual(s) => hir::LogicOpKind::Lte(s),
        ast::LogicOperation::GreaterThan(s) => hir::LogicOpKind::Gt(s),
        ast::LogicOperation::GreaterThanEqual(s) => hir::LogicOpKind::Gte(s),
    }
}

fn lower_literal(lit: ast::LiteralValue) -> hir::LiteralKind {
    match lit {
        ast::LiteralValue::Int(l) => hir::LiteralKind::Int(l),
        ast::LiteralValue::Float(l) => hir::LiteralKind::Float(l),
        ast::LiteralValue::Bool(l) => hir::LiteralKind::Bool(l),
        ast::LiteralValue::String(l) => hir::LiteralKind::String(l),
        ast::LiteralValue::Array(l) | ast::LiteralValue::Tuple(l) => {
            hir::LiteralKind::Array(l.iter().map(|e| lower_expression(e)).collect())
        }
        ast::LiteralValue::Null => todo!("Remove nulls"),
    }
}

fn lower_identifier(ident: ast::Identifier) -> hir::Ident {
    hir::Ident {
        name: ident.value,
        ty: ident.value_type,
        span: ident.span,
    }
}

pub fn lower_expression(expr: &ast::Expression) -> hir::Expression {
    match expr {
        ast::Expression::BinOp(e) => hir::Expression::BinOp(hir::BinOp {
            lhs: Box::new(lower_expression(&e.left)),
            rhs: Box::new(lower_expression(&e.right)),
            op: lower_binop_kind(e.op.clone()),
            span: e.span.clone(),
        }),
        ast::Expression::Literal(e) => hir::Expression::Literal(hir::Literal {
            kind: lower_literal(e.value.clone()),
            span: e.span.clone(),
        }),
        ast::Expression::Assign(e) => hir::Expression::Assign(hir::Assign {
            lhs: lower_identifier(e.name.clone()),
            rhs: Box::new(lower_expression(&e.rhs)),
            scope_distance: e.scope_distance.unwrap(),
            span: e.span.clone(),
        }),
        ast::Expression::GetIndex(e) => hir::Expression::GetIndex(hir::GetIndex {
            lhs: Box::new(lower_expression(&e.lhs)),
            idx: Box::new(lower_expression(&e.index)),
            span: e.span.clone(),
        }),
        ast::Expression::SetIndex(e) => hir::Expression::SetIndex(hir::SetIndex {
            lhs: Box::new(lower_expression(&e.lhs)),
            idx: Box::new(lower_expression(&e.index)),
            rhs: Box::new(lower_expression(&e.value)),
            span: e.span.clone(),
        }),
        ast::Expression::GetProperty(e) => hir::Expression::GetProperty(hir::GetProperty {
            lhs: Box::new(lower_expression(&e.object)),
            ident: lower_identifier(e.name.clone()),
            is_callable: e.is_callable,
            span: e.span.clone(),
        }),
        ast::Expression::SetProperty(e) => hir::Expression::SetProperty(hir::SetProperty {
            lhs: Box::new(lower_expression(&e.object)),
            ident: lower_identifier(e.name.clone()),
            rhs: Box::new(lower_expression(&e.value)),
            span: e.span.clone(),
        }),
        ast::Expression::Let(e) => hir::Expression::Let(hir::Let {
            ident: lower_identifier(e.name.clone()),
            init: e
                .value
                .as_ref()
                .and_then(|e| Some(Box::new(lower_expression(&e)))),
            span: e.span.clone(),
            kw_span: e.let_token.clone(),
        }),
        ast::Expression::LetRef(e) => hir::Expression::LetRef(hir::LetRef {
            ident: lower_identifier(e.name.clone()),
            scope_distance: todo!("fix scoping distance"),
            span: e.span.clone(),
        }),
        ast::Expression::UnaryOp(e) => hir::Expression::UnaryOp(hir::UnaryOp {
            rhs: Box::new(lower_expression(&e.rhs)),
            op: lower_unaryop_kind(e.op.clone()),
            span: e.span.clone(),
        }),
        ast::Expression::Grouping(e) => hir::Expression::Grouping(hir::Grouping {
            expr: Box::new(lower_expression(&e.expr)),
            span: e.span.clone(),
        }),
        ast::Expression::Logic(e) => hir::Expression::LogicOp(hir::LogicOp {
            lhs: Box::new(lower_expression(&e.lhs)),
            rhs: Box::new(lower_expression(&e.rhs)),
            op: lower_logicop_kind(e.op.clone()),
            span: e.span.clone(),
        }),
        ast::Expression::Call(e) => hir::Expression::Call(hir::Call {
            span: e.span.clone(),
            callee: Box::new(lower_expression(&e.callee)),
            arguments: e
                .arguments
                .iter()
                .map(|e| {
                    let ident = e.0.as_ref().and_then(|i| Some(lower_identifier(i.clone())));
                    hir::Arg(ident, lower_expression(&e.1))
                })
                .collect(),
        }),
        ast::Expression::Function(e) => hir::Expression::Function(hir::Function {
            name: e.name.clone(),
            body: Box::new(lower_expression(&e.body)),
            // TODO determine if imp self
            imp_self: hir::ImpSelf::Implicit,
            return_type: e.return_type.clone(),
            span: e.span.clone(),
            params: e
                .params
                .iter()
                .map(|p| lower_identifier(p.clone()))
                .map(|p| hir::Param {
                    ident: p.name,
                    span: p.span.clone(),
                    ty: p.ty,
                })
                .collect(),
        }),
        ast::Expression::DataStruct(e) => hir::Expression::DataStruct(hir::DataStruct {
            ident: lower_identifier(e.name.clone()),
            fields: e
                .fields
                .iter()
                .map(|f| hir::DataStructField {
                    name: f.value.clone(),
                    ty: f.value_type.clone(),
                    span: f.span.clone(),
                })
                .collect(),
            methods: todo!(),
            span: e.span.clone(),
        }),
        ast::Expression::DataStructInstance(_) => todo!(),
        ast::Expression::Block(e) => hir::Expression::Block(hir::Block {
            exprs: e.exprs.iter().map(|e| lower_expression(e)).collect(),
            span: e.span.clone(),
        }),
        ast::Expression::If(e) => hir::Expression::If(hir::If {
            condition: Box::new(lower_expression(&e.condition)),
            then: Box::new(lower_expression(&e.then)),
            not_then: e
                .not_then
                .as_ref()
                .and_then(|nt| Some(Box::new(lower_expression(nt)))),
            kw_span: e.if_token.clone(),
            span: e.span.clone(),
        }),
        ast::Expression::ImplicitReturn(e) => hir::Expression::Return(hir::Return {
            imp_return: true,
            kw_span: e.span.clone(),
            span: e.span.clone(),
            value: Some(Box::new(lower_expression(&e.value))),
        }),
        ast::Expression::Return(e) => hir::Expression::Return(hir::Return {
            imp_return: false,
            kw_span: e.return_token.clone(),
            span: e.span.clone(),
            value: e
                .value
                .as_ref()
                .and_then(|e| Some(Box::new(lower_expression(e)))),
        }),
        ast::Expression::SelfExpr(e) => hir::Expression::SelfExpr(hir::SelfExpr {
            target: e.name.clone(),
            span: e.span.clone(),
        }),
        ast::Expression::LoopExpr(e) => hir::Expression::While(hir::While {
            condition: Box::new(hir::Expression::Literal(hir::Literal {
                kind: hir::LiteralKind::Bool(true),
                span: e.loop_token.clone(),
            })),
            body: Box::new(lower_expression(&e.body)),
            kw_span: e.loop_token.clone(),
        }),
        ast::Expression::WhileExpr(e) => hir::Expression::While(hir::While {
            condition: Box::new(lower_expression(&e.condition)),
            body: Box::new(lower_expression(&e.body)),
            kw_span: e.while_token.clone(),
        }),
        ast::Expression::ForExpr(_) => todo!(),
        ast::Expression::BreakExpr(e) => hir::Expression::Break(hir::Break {
            span: e.span.clone(),
        }),
        ast::Expression::ContinueExpr(e) => hir::Expression::Continue(hir::Continue {
            span: e.span.clone(),
        }),
    }
}
