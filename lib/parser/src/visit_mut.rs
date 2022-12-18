use crate::ast;

pub trait VisitableMut {
    fn accept(&mut self, visitor: &mut impl VisitorMut);
}


pub trait VisitorMut {

    fn visit_binop(&mut self, e: &mut ast::BinOp) {
        self.visit_expr(&mut e.left);
        self.visit_expr(&mut e.right);
    }

    fn visit_literal(&mut self, _e: &mut ast::Literal) {
    }

    fn visit_assign(&mut self, e: &mut ast::Assign) {
        self.visit_expr(&mut e.rhs);
    }

    fn visit_get_index(&mut self, e: &mut ast::GetIndex) {
        self.visit_expr(&mut e.lhs);
        self.visit_expr(&mut e.index);
    }

    fn visit_set_index(&mut self, e: &mut ast::SetIndex) {
        self.visit_expr(&mut e.lhs);
        self.visit_expr(&mut e.index);
        self.visit_expr(&mut e.value);
    }

    fn visit_get_property(&mut self, e: &mut ast::GetProperty) {
        self.visit_expr(&mut e.object);
    }

    fn visit_set_property(&mut self, e: &mut ast::SetProperty) {
        self.visit_expr(&mut e.object);
        self.visit_expr(&mut e.value);
    }

    fn visit_let(&mut self, e: &mut ast::LetExpr) {
        if let Some(e) = &mut e.value {
            self.visit_expr(e)
        }
    }

    fn visit_let_ref(&mut self, _e: &mut ast::LetRef) {
    }

    fn visit_unary_op(&mut self, e: &mut ast::UnaryOp) {
        self.visit_expr(&mut e.rhs)
    }

    fn visit_grouping(&mut self, e: &mut ast::Grouping) {
        self.visit_expr(&mut e.expr)
    }

    fn visit_logic(&mut self, e: &mut ast::Logic) {
        self.visit_expr(&mut e.lhs);
        self.visit_expr(&mut e.rhs);
    }

    fn visit_call(&mut self, e: &mut ast::Call) {
        self.visit_expr(&mut e.callee)
    }

    fn visit_function(&mut self, e: &mut ast::Function) {
        self.visit_expr(&mut e.body)
    }

    fn visit_data_struct(&mut self, e: &mut ast::DataStruct) {
        for m in &mut e.methods {
            self.visit_expr(m)
        }
    }

    fn visit_data_struct_instance(&mut self, _e: &mut ast::DataStructInstance) {
    }

    fn visit_block(&mut self, e: &mut ast::Block) {
        for expr in &mut e.exprs {
            self.visit_expr(expr)
        }
    }

    fn visit_if(&mut self, e: &mut ast::IfConditional) {
        self.visit_expr(&mut e.condition);
        self.visit_expr(&mut e.then);
        if let Some(e) = &mut e.not_then {
            self.visit_expr(e);
        }
    }

    fn visit_implicit_return(&mut self, e: &mut ast::ImplicitReturn) {
        self.visit_expr(&mut e.value)
    }

    fn visit_return(&mut self, e: &mut ast::ReturnExpr) {
        if let Some(e) = &mut *e.value {
            self.visit_expr(e)
        }
    }

    fn visit_self(&mut self, _e: &mut ast::SelfExpr) {
    }

    fn visit_loop(&mut self, e: &mut ast::LoopExpr) {
        self.visit_expr(&mut e.condition);
        if let Some(i) = &mut e.iterator {
            self.visit_expr(i)
        }
        self.visit_expr(&mut e.body);
    }

    fn visit_break(&mut self, _e: &mut ast::BreakExpr) {
    }

    fn visit_continue(&mut self, _e: &mut ast::ContinueExpr) {
    }

    fn visit_expr(&mut self, expr: &mut ast::Expression) {
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

