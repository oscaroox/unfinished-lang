mod lowering;

use lowering::lower_expression;

pub fn lower_ast(ast: &ast::Program) -> hir::Program {
    ast.iter().map(|e| lower_expression(e)).collect()
}
