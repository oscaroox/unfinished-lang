use crate::ast;

pub trait Visitable {
    fn accept(&mut self, visitor: &mut impl Visitor);
}


pub trait Visitor {

    fn visit_binop(&mut self, e: &ast::BinOp) {
        self.visit_expr(&e.left);
        self.visit_expr(&e.right);
    }

    fn visit_literal(&mut self, _e: &ast::Literal) {
    }

    fn visit_assign(&mut self, e: &ast::Assign) {
        self.visit_expr(&e.rhs);
    }

    fn visit_get_index(&mut self, e: &ast::GetIndex) {
        self.visit_expr(&e.lhs);
        self.visit_expr(&e.index);
    }

    fn visit_set_index(&mut self, e: &ast::SetIndex) {
        self.visit_expr(&e.lhs);
        self.visit_expr(&e.index);
        self.visit_expr(&e.value);
    }

    fn visit_get_property(&mut self, e: &ast::GetProperty) {
        self.visit_expr(&e.object);
    }

    fn visit_set_property(&mut self, e: &ast::SetProperty) {
        self.visit_expr(&e.object);
        self.visit_expr(&e.value);
    }

    fn visit_let(&mut self, e: &ast::LetExpr) {
        if let Some(e) = &e.value {
            self.visit_expr(e)
        }
    }

    fn visit_let_ref(&mut self, _e: &ast::LetRef) {
    }

    fn visit_unary_op(&mut self, e: &ast::UnaryOp) {
        self.visit_expr(&e.rhs)
    }

    fn visit_grouping(&mut self, e: &ast::Grouping) {
        self.visit_expr(&e.expr)
    }

    fn visit_logic(&mut self, e: &ast::Logic) {
        self.visit_expr(&e.lhs);
        self.visit_expr(&e.rhs);
    }

    fn visit_call(&mut self, e: &ast::Call) {
        self.visit_expr(&e.callee)
    }

    fn visit_function(&mut self, e: &ast::Function) {
        self.visit_expr(&e.body)
    }

    fn visit_data_struct(&mut self, e: &ast::DataStruct) {
        for m in &e.methods {
            self.visit_expr(m)
        }
    }

    fn visit_data_struct_instance(&mut self, _e: &ast::DataStructInstance) {
    }

    fn visit_block(&mut self, e: &ast::Block) {
        for expr in &e.exprs {
            self.visit_expr(expr)
        }
    }

    fn visit_if(&mut self, e: &ast::IfConditional) {
        self.visit_expr(&e.condition);
        self.visit_expr(&e.then);
        if let Some(e) = &e.not_then {
            self.visit_expr(e);
        }
    }

    fn visit_implicit_return(&mut self, e: &ast::ImplicitReturn) {
        self.visit_expr(&e.value)
    }

    fn visit_return(&mut self, e: &ast::ReturnExpr) {
        if let Some(e) = &*e.value {
            self.visit_expr(e)
        }
    }

    fn visit_self(&mut self, _e: &ast::SelfExpr) {
    }

    fn visit_loop(&mut self, e: &ast::LoopExpr) {
        self.visit_expr(&e.condition);
        if let Some(i) = &e.iterator {
            self.visit_expr(i)
        }
        self.visit_expr(&e.body);
    }

    fn visit_break(&mut self, _e: &ast::BreakExpr) {
    }

    fn visit_continue(&mut self, _e: &ast::ContinueExpr) {
    }

    fn visit_expr(&mut self, expr: &ast::Expression) {
        match expr {
            ast::Expression::BinOp(e) => self.visit_binop(e),
            ast::Expression::Literal(e) => self.visit_literal(e),
            ast::Expression::Assign(e) => self.visit_assign(e),
            ast::Expression::GetIndex(e) => self.visit_get_index(e),
            ast::Expression::SetIndex(e) => self.visit_set_index(e),
            ast::Expression::GetProperty(e) => self.visit_get_property(e),
            ast::Expression::SetProperty(e) => self.visit_set_property(e),
            ast::Expression::Let(e) => self.visit_let(e),
            ast::Expression::LetRef(e) => self.visit_let_ref(e),
            ast::Expression::UnaryOp(e) => self.visit_unary_op(e),
            ast::Expression::Grouping(e) => self.visit_grouping(e),
            ast::Expression::Logic(e) => self.visit_logic(e),
            ast::Expression::Call(e) => self.visit_call(e),
            ast::Expression::Function(e) => self.visit_function(e),
            ast::Expression::DataStruct(e) => self.visit_data_struct(e),
            ast::Expression::DataStructInstance(e) => self.visit_data_struct_instance(e),
            ast::Expression::Block(e) => self.visit_block(e),
            ast::Expression::If(e) => self.visit_if(e),
            ast::Expression::ImplicitReturn(e) => self.visit_implicit_return(e),
            ast::Expression::Return(e) => self.visit_return(e),
            ast::Expression::SelfExpr(e) => self.visit_self(e),
            ast::Expression::LoopExpr(e) => self.visit_loop(e),
            ast::Expression::BreakExpr(e) => self.visit_break(e),
            ast::Expression::ContinueExpr(e) => self.visit_continue(e),
        }
    }
}

