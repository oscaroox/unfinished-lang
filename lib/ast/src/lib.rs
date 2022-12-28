mod ast;
pub mod mut_visit;
pub mod visit;

pub use ast::*;




impl visit::Visitable for Expression {
    fn accept(&mut self, visitor: &mut impl visit::Visitor) {
        visitor.visit_expr(self)
    }
}

impl mut_visit::MutVisitable for Expression {
    fn accept(&mut self, visitor: &mut impl mut_visit::MutVisitor) {
        visitor.visit_expr(self)
    }
}
