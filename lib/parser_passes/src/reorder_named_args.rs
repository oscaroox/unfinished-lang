use parser::mut_visit::MutVisitor;

pub struct ReorderNamedArgs;

impl ReorderNamedArgs {
    pub fn new() -> Self {
        ReorderNamedArgs
    }
}

impl MutVisitor for ReorderNamedArgs {
    fn visit_function(&mut self, e: &mut parser::ast::Function) {

    }
}