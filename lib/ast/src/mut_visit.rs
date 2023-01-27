use crate::ast;

pub trait MutVisitable {
    fn accept(&mut self, visitor: &mut impl MutVisitor);
}

pub trait MutVisitor: Sized {
    fn visit_binop(&mut self, e: &mut ast::BinOp) {
        walk_binop(self, e);
    }

    fn visit_literal(&mut self, _e: &mut ast::Literal) {}

    fn visit_assign(&mut self, e: &mut ast::Assign) {
        walk_assign(self, e);
    }

    fn visit_get_index(&mut self, e: &mut ast::GetIndex) {
        walk_get_index(self, e)
    }

    fn visit_set_index(&mut self, e: &mut ast::SetIndex) {
        walk_set_index(self, e);
    }

    fn visit_get_property(&mut self, e: &mut ast::GetProperty) {
        walk_get_property(self, e);
    }

    fn visit_set_property(&mut self, e: &mut ast::SetProperty) {
        walk_set_property(self, e);
    }

    fn visit_let(&mut self, e: &mut ast::LetExpr) {
        walk_let(self, e);
    }

    fn visit_let_ref(&mut self, _e: &mut ast::LetRef) {}

    fn visit_unaryop(&mut self, e: &mut ast::UnaryOp) {
        walk_unaryop(self, e);
    }

    fn visit_grouping(&mut self, e: &mut ast::Grouping) {
        walk_grouping(self, e);
    }

    fn visit_logic(&mut self, e: &mut ast::Logic) {
        walk_logic(self, e);
    }

    fn visit_call(&mut self, e: &mut ast::Call) {
        walk_call(self, e);
    }

    fn visit_function(&mut self, e: &mut ast::Function) {
        walk_function(self, e);
    }

    fn visit_data_struct(&mut self, e: &mut ast::DataStruct) {
        walk_data_struct(self, e);
    }

    fn visit_data_struct_instance(&mut self, _e: &mut ast::DataStructInstance) {}

    fn visit_block(&mut self, e: &mut ast::Block) {
        walk_block(self, e);
    }

    fn visit_if(&mut self, e: &mut ast::IfConditional) {
        walk_if(self, e);
    }

    fn visit_implicit_return(&mut self, e: &mut ast::ImplicitReturn) {
        walk_implicit_return(self, e);
    }

    fn visit_return(&mut self, e: &mut ast::ReturnExpr) {
        walk_return(self, e);
    }

    fn visit_self(&mut self, _e: &mut ast::SelfExpr) {}

    fn visit_loop(&mut self, e: &mut ast::LoopExpr) {
        walk_loop(self, e);
    }

    fn visit_while(&mut self, e: &mut ast::WhileExpr) {
        walk_while(self, e);
    }
    fn visit_for(&mut self, e: &mut ast::ForExpr) {
        walk_for(self, e);
    }

    fn visit_break(&mut self, _e: &mut ast::BreakExpr) {}

    fn visit_continue(&mut self, _e: &mut ast::ContinueExpr) {}

    fn visit_expr(&mut self, expr: &mut ast::Expression) {
        walk_expr(self, expr);
    }
}

pub fn walk_expr<V: MutVisitor>(vis: &mut V, expr: &mut ast::Expression) {
    match expr {
        ast::Expression::BinOp(e) => vis.visit_binop(e),
        ast::Expression::Literal(e) => vis.visit_literal(e),
        ast::Expression::Assign(e) => vis.visit_assign(e),
        ast::Expression::GetIndex(e) => vis.visit_get_index(e),
        ast::Expression::SetIndex(e) => vis.visit_set_index(e),
        ast::Expression::GetProperty(e) => vis.visit_get_property(e),
        ast::Expression::SetProperty(e) => vis.visit_set_property(e),
        ast::Expression::Let(e) => vis.visit_let(e),
        ast::Expression::LetRef(e) => vis.visit_let_ref(e),
        ast::Expression::UnaryOp(e) => vis.visit_unaryop(e),
        ast::Expression::Grouping(e) => vis.visit_grouping(e),
        ast::Expression::Logic(e) => vis.visit_logic(e),
        ast::Expression::Call(e) => vis.visit_call(e),
        ast::Expression::Function(e) => vis.visit_function(e),
        ast::Expression::DataStruct(e) => vis.visit_data_struct(e),
        ast::Expression::DataStructInstance(e) => vis.visit_data_struct_instance(e),
        ast::Expression::Block(e) => vis.visit_block(e),
        ast::Expression::If(e) => vis.visit_if(e),
        ast::Expression::ImplicitReturn(e) => vis.visit_implicit_return(e),
        ast::Expression::Return(e) => vis.visit_return(e),
        ast::Expression::SelfExpr(e) => vis.visit_self(e),
        ast::Expression::LoopExpr(e) => vis.visit_loop(e),
        ast::Expression::WhileExpr(e) => vis.visit_while(e),
        ast::Expression::ForExpr(e) => vis.visit_for(e),
        ast::Expression::BreakExpr(e) => vis.visit_break(e),
        ast::Expression::ContinueExpr(e) => vis.visit_continue(e),
    }
}

pub fn walk_for<V: MutVisitor>(vis: &mut V, e: &mut ast::ForExpr) {
    vis.visit_expr(&mut e.iterator);
    vis.visit_expr(&mut e.body);
}

pub fn walk_while<V: MutVisitor>(vis: &mut V, e: &mut ast::WhileExpr) {
    vis.visit_expr(&mut e.condition);
    vis.visit_expr(&mut e.body);
}

pub fn walk_loop<V: MutVisitor>(vis: &mut V, e: &mut ast::LoopExpr) {
    vis.visit_expr(&mut e.body);
}

pub fn walk_return<V: MutVisitor>(vis: &mut V, e: &mut ast::ReturnExpr) {
    if let Some(e) = &mut *e.value {
        vis.visit_expr(e)
    }
}

pub fn walk_implicit_return<V: MutVisitor>(vis: &mut V, e: &mut ast::ImplicitReturn) {
    vis.visit_expr(&mut e.value)
}

pub fn walk_if<V: MutVisitor>(vis: &mut V, e: &mut ast::IfConditional) {
    vis.visit_expr(&mut e.condition);
    vis.visit_expr(&mut e.then);
    if let Some(e) = &mut e.not_then {
        vis.visit_expr(e);
    }
}

pub fn walk_block<V: MutVisitor>(vis: &mut V, e: &mut ast::Block) {
    for expr in &mut e.exprs {
        vis.visit_expr(expr)
    }
}

pub fn walk_data_struct<V: MutVisitor>(vis: &mut V, e: &mut ast::DataStruct) {
    for m in &mut e.methods {
        vis.visit_expr(m)
    }
}

pub fn walk_function<V: MutVisitor>(vis: &mut V, e: &mut ast::Function) {
    vis.visit_expr(&mut e.body)
}

pub fn walk_call<V: MutVisitor>(vis: &mut V, e: &mut ast::Call) {
    vis.visit_expr(&mut e.callee)
}

pub fn walk_grouping<V: MutVisitor>(vis: &mut V, e: &mut ast::Grouping) {
    vis.visit_expr(&mut e.expr)
}

pub fn walk_unaryop<V: MutVisitor>(vis: &mut V, e: &mut ast::UnaryOp) {
    vis.visit_expr(&mut e.rhs)
}

pub fn walk_let<V: MutVisitor>(vis: &mut V, e: &mut ast::LetExpr) {
    if let Some(e) = &mut e.value {
        vis.visit_expr(e)
    }
}

pub fn walk_set_property<V: MutVisitor>(vis: &mut V, e: &mut ast::SetProperty) {
    vis.visit_expr(&mut e.object);
    vis.visit_expr(&mut e.value);
}

pub fn walk_get_property<V: MutVisitor>(vis: &mut V, e: &mut ast::GetProperty) {
    vis.visit_expr(&mut e.object);
}

pub fn walk_set_index<V: MutVisitor>(vis: &mut V, e: &mut ast::SetIndex) {
    vis.visit_expr(&mut e.lhs);
    vis.visit_expr(&mut e.index);
    vis.visit_expr(&mut e.value);
}

pub fn walk_get_index<V: MutVisitor>(vis: &mut V, e: &mut ast::GetIndex) {
    vis.visit_expr(&mut e.lhs);
    vis.visit_expr(&mut e.index);
}

pub fn walk_assign<V: MutVisitor>(vis: &mut V, e: &mut ast::Assign) {
    vis.visit_expr(&mut e.rhs);
}

pub fn walk_binop<V: MutVisitor>(vis: &mut V, e: &mut ast::BinOp) {
    vis.visit_expr(&mut e.left);
    vis.visit_expr(&mut e.right);
}

pub fn walk_logic<V: MutVisitor>(vis: &mut V, e: &mut ast::Logic) {
    vis.visit_expr(&mut e.lhs);
    vis.visit_expr(&mut e.rhs);
}
