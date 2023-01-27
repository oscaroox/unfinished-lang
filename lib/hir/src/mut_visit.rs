use crate::hir;

pub trait MutVisitable {
    fn accept(&mut self, visitor: &mut impl MutVisitor);
}

pub trait MutVisitor: Sized {
    fn visit_binop(&mut self, e: &mut hir::BinOp) {
        walk_binop(self, e);
    }

    fn visit_literal(&mut self, _e: &mut hir::Literal) {}

    fn visit_assign(&mut self, e: &mut hir::Assign) {
        walk_assign(self, e);
    }

    fn visit_get_index(&mut self, e: &mut hir::GetIndex) {
        walk_get_index(self, e)
    }

    fn visit_set_index(&mut self, e: &mut hir::SetIndex) {
        walk_set_index(self, e);
    }

    fn visit_get_property(&mut self, e: &mut hir::GetProperty) {
        walk_get_property(self, e);
    }

    fn visit_set_property(&mut self, e: &mut hir::SetProperty) {
        walk_set_property(self, e);
    }

    fn visit_let(&mut self, e: &mut hir::Let) {
        walk_let(self, e);
    }

    fn visit_let_ref(&mut self, _e: &mut hir::LetRef) {}

    fn visit_unaryop(&mut self, e: &mut hir::UnaryOp) {
        walk_unaryop(self, e);
    }

    fn visit_grouping(&mut self, e: &mut hir::Grouping) {
        walk_grouping(self, e);
    }

    fn visit_logic(&mut self, e: &mut hir::LogicOp) {
        walk_logic(self, e);
    }

    fn visit_call(&mut self, e: &mut hir::Call) {
        walk_call(self, e);
    }

    fn visit_function(&mut self, e: &mut hir::Function) {
        walk_function(self, e);
    }

    fn visit_data_struct(&mut self, e: &mut hir::DataStruct) {
        walk_data_struct(self, e);
    }

    fn visit_block(&mut self, e: &mut hir::Block) {
        walk_block(self, e);
    }

    fn visit_if(&mut self, e: &mut hir::If) {
        walk_if(self, e);
    }

    fn visit_return(&mut self, e: &mut hir::Return) {
        walk_return(self, e);
    }

    fn visit_self(&mut self, _e: &mut hir::SelfExpr) {}

    fn visit_loop(&mut self, e: &mut hir::While) {
        walk_loop(self, e);
    }

    fn visit_break(&mut self, _e: &mut hir::Break) {}

    fn visit_continue(&mut self, _e: &mut hir::Continue) {}

    fn visit_expr(&mut self, expr: &mut hir::Expression) {
        walk_expr(self, expr);
    }
}

pub fn walk_expr<V: MutVisitor>(vis: &mut V, expr: &mut hir::Expression) {
    match expr {
        hir::Expression::BinOp(e) => vis.visit_binop(e),
        hir::Expression::Literal(e) => vis.visit_literal(e),
        hir::Expression::Assign(e) => vis.visit_assign(e),
        hir::Expression::GetIndex(e) => vis.visit_get_index(e),
        hir::Expression::SetIndex(e) => vis.visit_set_index(e),
        hir::Expression::GetProperty(e) => vis.visit_get_property(e),
        hir::Expression::SetProperty(e) => vis.visit_set_property(e),
        hir::Expression::Let(e) => vis.visit_let(e),
        hir::Expression::LetRef(e) => vis.visit_let_ref(e),
        hir::Expression::UnaryOp(e) => vis.visit_unaryop(e),
        hir::Expression::Grouping(e) => vis.visit_grouping(e),
        hir::Expression::LogicOp(e) => vis.visit_logic(e),
        hir::Expression::Call(e) => vis.visit_call(e),
        hir::Expression::Function(e) => vis.visit_function(e),
        hir::Expression::DataStruct(e) => vis.visit_data_struct(e),
        hir::Expression::Block(e) => vis.visit_block(e),
        hir::Expression::If(e) => vis.visit_if(e),
        hir::Expression::Return(e) => vis.visit_return(e),
        hir::Expression::SelfExpr(e) => vis.visit_self(e),
        hir::Expression::While(e) => vis.visit_loop(e),
        hir::Expression::Break(e) => vis.visit_break(e),
        hir::Expression::Continue(e) => vis.visit_continue(e),
    }
}

pub fn walk_loop<V: MutVisitor>(vis: &mut V, e: &mut hir::While) {
    vis.visit_expr(&mut e.condition);
    vis.visit_expr(&mut e.body);
}

pub fn walk_return<V: MutVisitor>(vis: &mut V, e: &mut hir::Return) {
    if let Some(e) = &mut e.value {
        vis.visit_expr(e)
    }
}

pub fn walk_if<V: MutVisitor>(vis: &mut V, e: &mut hir::If) {
    vis.visit_expr(&mut e.condition);
    vis.visit_expr(&mut e.then);
    if let Some(e) = &mut e.not_then {
        vis.visit_expr(e);
    }
}

pub fn walk_block<V: MutVisitor>(vis: &mut V, e: &mut hir::Block) {
    for expr in &mut e.exprs {
        vis.visit_expr(expr)
    }
}

pub fn walk_data_struct<V: MutVisitor>(vis: &mut V, e: &mut hir::DataStruct) {
    for m in &mut e.methods {
        vis.visit_expr(&mut *m.body)
    }
}

pub fn walk_function<V: MutVisitor>(vis: &mut V, e: &mut hir::Function) {
    vis.visit_expr(&mut e.body)
}

pub fn walk_call<V: MutVisitor>(vis: &mut V, e: &mut hir::Call) {
    vis.visit_expr(&mut e.callee)
}

pub fn walk_grouping<V: MutVisitor>(vis: &mut V, e: &mut hir::Grouping) {
    vis.visit_expr(&mut e.expr)
}

pub fn walk_unaryop<V: MutVisitor>(vis: &mut V, e: &mut hir::UnaryOp) {
    vis.visit_expr(&mut e.rhs)
}

pub fn walk_let<V: MutVisitor>(vis: &mut V, e: &mut hir::Let) {
    if let Some(e) = &mut e.init {
        vis.visit_expr(e)
    }
}

pub fn walk_set_property<V: MutVisitor>(vis: &mut V, e: &mut hir::SetProperty) {
    vis.visit_expr(&mut e.lhs);
    vis.visit_expr(&mut e.value);
}

pub fn walk_get_property<V: MutVisitor>(vis: &mut V, e: &mut hir::GetProperty) {
    vis.visit_expr(&mut e.object);
}

pub fn walk_set_index<V: MutVisitor>(vis: &mut V, e: &mut hir::SetIndex) {
    vis.visit_expr(&mut e.lhs);
    vis.visit_expr(&mut e.idx);
    vis.visit_expr(&mut e.value);
}

pub fn walk_get_index<V: MutVisitor>(vis: &mut V, e: &mut hir::GetIndex) {
    vis.visit_expr(&mut e.lhs);
    vis.visit_expr(&mut e.idx);
}

pub fn walk_assign<V: MutVisitor>(vis: &mut V, e: &mut hir::Assign) {
    vis.visit_expr(&mut e.rhs);
}

pub fn walk_binop<V: MutVisitor>(vis: &mut V, e: &mut hir::BinOp) {
    vis.visit_expr(&mut e.lhs);
    vis.visit_expr(&mut e.rhs);
}

pub fn walk_logic<V: MutVisitor>(vis: &mut V, e: &mut hir::LogicOp) {
    vis.visit_expr(&mut e.lhs);
    vis.visit_expr(&mut e.rhs);
}
