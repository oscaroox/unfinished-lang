use crate::hir;

pub trait Visitable {
    fn accept(&mut self, visitor: &mut impl Visitor);
}

pub trait Visitor: Sized {
    fn visit_binop(&mut self, e: &hir::BinOp) {
        walk_binop(self, e);
    }

    fn visit_literal(&mut self, _e: &hir::Literal) {}

    fn visit_assign(&mut self, e: &hir::Assign) {
        walk_assign(self, e);
    }

    fn visit_get_index(&mut self, e: &hir::GetIndex) {
        walk_get_index(self, e)
    }

    fn visit_set_index(&mut self, e: &hir::SetIndex) {
        walk_set_index(self, e);
    }

    fn visit_get_property(&mut self, e: &hir::GetProperty) {
        walk_get_property(self, e);
    }

    fn visit_set_property(&mut self, e: &hir::SetProperty) {
        walk_set_property(self, e);
    }

    fn visit_let(&mut self, e: &hir::Let) {
        walk_let(self, e);
    }

    fn visit_let_ref(&mut self, _e: &hir::LetRef) {}

    fn visit_unaryop(&mut self, e: &hir::UnaryOp) {
        walk_unaryop(self, e);
    }

    fn visit_grouping(&mut self, e: &hir::Grouping) {
        walk_grouping(self, e);
    }

    fn visit_logic(&mut self, e: &hir::LogicOp) {
        walk_logic(self, e);
    }

    fn visit_call(&mut self, e: &hir::Call) {
        walk_call(self, e);
    }

    fn visit_function(&mut self, e: &hir::Function) {
        walk_function(self, e);
    }

    fn visit_data_struct(&mut self, e: &hir::DataStruct) {
        walk_data_struct(self, e);
    }

    fn visit_block(&mut self, e: &hir::Block) {
        walk_block(self, e);
    }

    fn visit_if(&mut self, e: &hir::If) {
        walk_if(self, e);
    }

    fn visit_return(&mut self, e: &hir::Return) {
        walk_return(self, e);
    }

    fn visit_self(&mut self, _e: &hir::SelfExpr) {}

    fn visit_loop(&mut self, e: &hir::While) {
        walk_loop(self, e);
    }

    fn visit_break(&mut self, _e: &hir::Break) {}

    fn visit_continue(&mut self, _e: &hir::Continue) {}

    fn visit_expr(&mut self, expr: &hir::Expression) {
        walk_expr(self, expr);
    }
}

pub fn walk_expr<V: Visitor>(vis: &mut V, expr: &hir::Expression) {
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

pub fn walk_loop<V: Visitor>(vis: &mut V, e: &hir::While) {
    vis.visit_expr(&e.condition);
    vis.visit_expr(&e.body);
}

pub fn walk_return<V: Visitor>(vis: &mut V, e: &hir::Return) {
    if let Some(e) = &e.value {
        vis.visit_expr(e)
    }
}

pub fn walk_if<V: Visitor>(vis: &mut V, e: &hir::If) {
    vis.visit_expr(&e.condition);
    vis.visit_expr(&e.then);
    if let Some(e) = &e.not_then {
        vis.visit_expr(e);
    }
}

pub fn walk_block<V: Visitor>(vis: &mut V, e: &hir::Block) {
    for expr in &e.exprs {
        vis.visit_expr(expr)
    }
}

pub fn walk_data_struct<V: Visitor>(vis: &mut V, e: &hir::DataStruct) {
    for m in &e.methods {
        vis.visit_expr(&*m.body)
    }
}

pub fn walk_function<V: Visitor>(vis: &mut V, e: &hir::Function) {
    vis.visit_expr(&e.body)
}

pub fn walk_call<V: Visitor>(vis: &mut V, e: &hir::Call) {
    vis.visit_expr(&e.callee)
}

pub fn walk_grouping<V: Visitor>(vis: &mut V, e: &hir::Grouping) {
    vis.visit_expr(&e.expr)
}

pub fn walk_unaryop<V: Visitor>(vis: &mut V, e: &hir::UnaryOp) {
    vis.visit_expr(&e.rhs)
}

pub fn walk_let<V: Visitor>(vis: &mut V, e: &hir::Let) {
    if let Some(e) = &e.init {
        vis.visit_expr(e)
    }
}

pub fn walk_set_property<V: Visitor>(vis: &mut V, e: &hir::SetProperty) {
    vis.visit_expr(&e.lhs);
    vis.visit_expr(&e.value);
}

pub fn walk_get_property<V: Visitor>(vis: &mut V, e: &hir::GetProperty) {
    vis.visit_expr(&e.object);
}

pub fn walk_set_index<V: Visitor>(vis: &mut V, e: &hir::SetIndex) {
    vis.visit_expr(&e.lhs);
    vis.visit_expr(&e.idx);
    vis.visit_expr(&e.value);
}

pub fn walk_get_index<V: Visitor>(vis: &mut V, e: &hir::GetIndex) {
    vis.visit_expr(&e.lhs);
    vis.visit_expr(&e.idx);
}

pub fn walk_assign<V: Visitor>(vis: &mut V, e: &hir::Assign) {
    vis.visit_expr(&e.rhs);
}

pub fn walk_binop<V: Visitor>(vis: &mut V, e: &hir::BinOp) {
    vis.visit_expr(&e.lhs);
    vis.visit_expr(&e.rhs);
}

pub fn walk_logic<V: Visitor>(vis: &mut V, e: &hir::LogicOp) {
    vis.visit_expr(&e.lhs);
    vis.visit_expr(&e.rhs);
}
