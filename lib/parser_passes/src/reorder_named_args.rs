use std::cell::RefCell;
use std::rc::Rc;

use parser::mut_visit::{MutVisitor, walk_function, MutVisitable};
use parser::ast::{self, Expression, Program};


#[derive(Debug, Clone)]
struct SymbolTable {
    store: Vec<String>,
    parent: Option<Rc<RefCell<SymbolTable>>>
}

pub struct ReorderNamedArgs {
    scope: SymbolTable
}

impl ReorderNamedArgs {
    pub fn new() -> Self {
        ReorderNamedArgs {
            scope: SymbolTable { store: vec![], parent: None }
        }
    }

    fn run(&mut self, exprs: &mut Program) {
        for expr in exprs {
            expr.accept(self)
        }
    }
}

impl MutVisitor for ReorderNamedArgs {
    fn visit_function(&mut self, e: &mut parser::ast::Function) {
    }

    fn visit_data_struct(&mut self, e: &mut parser::ast::DataStruct) {
    }
}


#[cfg(test)]
mod test {
    use parser::ast::Program;

    use super::ReorderNamedArgs;

    fn analyze(src: &str) -> Program {
        let mut ast = parser::parse_panic(src);
        let mut analyzer = ReorderNamedArgs::new();
        analyzer.run(&mut ast);
        ast
    }


    #[test]
    fn check() {
        let ast = analyze("
            let x = 1;
            let y = {};
            let main = fn (x: int, y: int) {

            };
        ");

        println!("{ast:#?}")
    }
}