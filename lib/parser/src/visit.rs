use ast;

pub trait Visitable {
    fn accept(&mut self, visitor: &mut impl Visitor);
}


pub trait Visitor : Sized {

    fn visit_binop(&mut self, e: &ast::BinOp) {
        walk_binop(self, e);
    }

    fn visit_literal(&mut self, _e: &ast::Literal) {
    }

    fn visit_assign(&mut self, e: &ast::Assign) {
        walk_assign(self, e);
    }

    fn visit_get_index(&mut self, e: &ast::GetIndex) {
        walk_get_index(self, e)
    }

    fn visit_set_index(&mut self, e: &ast::SetIndex) {
        walk_set_index(self, e);
    }

    fn visit_get_property(&mut self, e: &ast::GetProperty) {
        walk_get_property(self, e);
    }

    fn visit_set_property(&mut self, e: &ast::SetProperty) {
        walk_set_property(self, e);
    }

    fn visit_let(&mut self, e: &ast::LetExpr) {
        walk_let(self, e);
    }

    fn visit_let_ref(&mut self, _e: &ast::LetRef) {
    }

    fn visit_unaryop(&mut self, e: &ast::UnaryOp) {
        walk_unaryop(self, e);
    }

    fn visit_grouping(&mut self, e: &ast::Grouping) {
        walk_grouping(self, e);
    }

    fn visit_logic(&mut self, e: &ast::Logic) {
        walk_logic(self, e);
    }

    fn visit_call(&mut self, e: &ast::Call) {
        walk_call(self, e);
    }

    fn visit_function(&mut self, e: &ast::Function) {
        walk_function(self, e);
    }

    fn visit_data_struct(&mut self, e: &ast::DataStruct) {
        walk_data_struct(self, e);
    }

    fn visit_data_struct_instance(&mut self, _e: &ast::DataStructInstance) {
    }

    fn visit_block(&mut self, e: &ast::Block) {
        walk_block(self, e);
    }

    fn visit_if(&mut self, e: &ast::IfConditional) {
        walk_if(self, e);
    }

    fn visit_implicit_return(&mut self, e: &ast::ImplicitReturn) {
        walk_implicit_return(self, e);
    }

    fn visit_return(&mut self, e: &ast::ReturnExpr) {
        walk_return(self, e);
    }

    fn visit_self(&mut self, _e: &ast::SelfExpr) {
    }

    fn visit_loop(&mut self, e: &ast::LoopExpr) {
        walk_loop(self, e);
    }

    fn visit_break(&mut self, _e: &ast::BreakExpr) {
    }

    fn visit_continue(&mut self, _e: &ast::ContinueExpr) {
    }

    fn visit_expr(&mut self, expr: &ast::Expression) {
        walk_expr(self, expr);
    }

}

pub fn walk_expr<V: Visitor>(vis: &mut V, expr: &ast::Expression) {
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
        ast::Expression::BreakExpr(e) => vis.visit_break(e),
        ast::Expression::ContinueExpr(e) => vis.visit_continue(e),
    }
}

pub fn walk_loop<V: Visitor>(vis: &mut V, e: &ast::LoopExpr) {
    vis.visit_expr(&e.condition);
    if let Some(i) = &e.iterator {
        vis.visit_expr(i)
    }
    vis.visit_expr(&e.body);
}

pub fn walk_return<V: Visitor>(vis: &mut V, e: &ast::ReturnExpr) {
    if let Some(e) = &*e.value {
        vis.visit_expr(e)
    }
}

pub fn walk_implicit_return<V: Visitor>(vis: &mut V, e: &ast::ImplicitReturn) {
    vis.visit_expr(&e.value)
}

pub fn walk_if<V: Visitor>(vis: &mut V, e: &ast::IfConditional) {
    vis.visit_expr(&e.condition);
    vis.visit_expr(&e.then);
    if let Some(e) = &e.not_then {
        vis.visit_expr(e);
    }
}

pub fn walk_block<V: Visitor>(vis: &mut V, e: &ast::Block) {
    for expr in &e.exprs {
        vis.visit_expr(expr)
    }
}

pub fn walk_data_struct<V: Visitor>(vis: &mut V, e: &ast::DataStruct) {
    for m in &e.methods {
        vis.visit_expr(m)
    }
}

pub fn walk_function<V: Visitor>(vis: &mut V, e: &ast::Function) {
    vis.visit_expr(&e.body)
}

pub fn walk_call<V: Visitor>(vis: &mut V, e: &ast::Call) {
    vis.visit_expr(&e.callee)
}

pub fn walk_grouping<V: Visitor>(vis: &mut V, e: &ast::Grouping) {
    vis.visit_expr(&e.expr)
}

pub fn walk_unaryop<V: Visitor>(vis: &mut V, e: &ast::UnaryOp) {
    vis.visit_expr(&e.rhs)
}

pub fn walk_let<V: Visitor>(vis: &mut V, e: &ast::LetExpr) {
    if let Some(e) = &e.value {
        vis.visit_expr(e)
    }
}

pub fn walk_set_property<V: Visitor>(vis: &mut V, e: &ast::SetProperty) {
    vis.visit_expr(&e.object);
    vis.visit_expr(&e.value);
}

pub fn walk_get_property<V: Visitor>(vis: &mut V, e: &ast::GetProperty) {
    vis.visit_expr(&e.object);
}

pub fn walk_set_index<V: Visitor>(vis: &mut V, e: &ast::SetIndex) {
    vis.visit_expr(&e.lhs);
    vis.visit_expr(&e.index);
    vis.visit_expr(&e.value);
}

pub fn walk_get_index<V: Visitor>(vis: &mut V, e: &ast::GetIndex) {
    vis.visit_expr(&e.lhs);
    vis.visit_expr(&e.index);
}

pub fn walk_assign<V: Visitor>(vis: &mut V, e: &ast::Assign) {
    vis.visit_expr(&e.rhs);
}

pub fn walk_binop<V: Visitor>(vis: &mut V, e: &ast::BinOp) {
    vis.visit_expr(&e.left);
    vis.visit_expr(&e.right);
}


pub fn walk_logic<V: Visitor>(vis: &mut V, e: &ast::Logic) {
    vis.visit_expr(&e.lhs);
    vis.visit_expr(&e.rhs);
}

