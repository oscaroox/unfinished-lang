use std::{collections::HashSet, hash::Hash};
use parser::ast::Program;
use parser::mut_visit::{MutVisitor, MutVisitable, walk_block, walk_let, walk_assign};

use crate::errors::PassesError;
use crate::scope_table::ScopeTable;

pub struct NameResolver {
    errors: Vec<PassesError>,
    scope_table: ScopeTable
}

impl NameResolver {
    pub fn new() -> Self {
        Self { 
            errors: vec![],
            scope_table: ScopeTable::with_global_scope()
        }
    }

    pub fn run(&mut self, program: &mut Program) {
        program
            .iter_mut()
            .for_each(|e| e.accept(self))
    }

    pub fn errors(self) -> Vec<PassesError> {
        self.errors
    }

    pub fn add_error(&mut self, value: PassesError) {
        self.errors.push(value)
    }
}


impl MutVisitor for NameResolver {

    fn visit_let(&mut self, e: &mut parser::ast::LetExpr) {
        self.scope_table.define(e.name.value.to_string());
        walk_let(self, e)
    }

    fn visit_let_ref(&mut self, e: &mut parser::ast::LetRef) {
        let scope_distance = self.scope_table.resolve(&e.name.value);
        e.scope_distance = self.scope_table.resolve(&e.name.value);

        if scope_distance.is_none() {
            self.add_error(PassesError::CannotFindValue(e.name.value.to_string(), e.name.span.clone()))
        }
    }

    fn visit_assign(&mut self, e: &mut parser::ast::Assign) {
        walk_assign(self, e);
        e.scope_distance = self.scope_table.resolve(&e.name.value);
    }

    fn visit_block(&mut self, e: &mut parser::ast::Block) {
        self.scope_table.add_scope();
        walk_block(self, e);
        self.scope_table.pop_scope();
    }
}


#[cfg(test)]
mod name_resolver_test {
    use parser::ast::Program;
    use parser::test_utils::{create_let, int, create_function, unit_type, create_block, create_assign_with_scope, create_let_ref_with_scope};
    use pretty_assertions::assert_eq;
    use span_util::Span;
    use crate::errors::PassesError;

    use super::NameResolver;

    fn analyze(src: &str) -> (Program, NameResolver) {
        let mut ast = parser::parse_panic(src);
        let mut name_resolver = NameResolver::new();
        name_resolver.run(&mut ast);
        (ast, name_resolver)
    }

    fn analyze_ok(src: &str, expected: Program) {
        let (ast, _) = analyze(src);
        assert_eq!(ast, expected);
    }
    
    fn analyze_error(src: &str, expected: Vec<PassesError>) {
        let (_, name_resolver) = analyze(src);
        assert_eq!(name_resolver.errors(), expected);
    }

    #[test]
    fn resolve_assign() {
        analyze_ok("
            let x = 2;
            let y = 3;
            let main = fn {
                y = x;
            }
        ", vec![
            create_let("x", Some(int(2))),
            create_let("y", Some(int(3))),
            create_let("main", Some(create_function(
                Some("main".to_string()), 
                vec![], 
                unit_type(), 
                true, 
                create_block(vec![
                    create_assign_with_scope(
                        "y", 
                        create_let_ref_with_scope("x", Some(1)),
                         Some(1)
                    )
                ]))
            ))
        ]);   
    }

    #[test]
    fn resolve_let_ref() {
        analyze_ok("
        let x = 2;
        fn {
            {
                {
                    let y = x;
                }
            }
        }
        ", vec![
            create_let("x", Some(int(2))),
            create_function(
                None, 
                vec![], 
                unit_type(), 
                true, 
                create_block(vec![
                    create_block(vec![
                        create_block(vec![
                            create_let(
                                "y", 
                                Some(create_let_ref_with_scope("x", Some(3)))
                            )
                        ])
                    ])
                ])
            )
        ])
    }

    #[test]
    fn resolve_correct_scope_distance() {
        analyze_ok("
        let x = 2;
        {
            {
                let y = x;
            }
            let x = 3;
            {
                let y = x;
            }
        }
        ", vec![
            create_let("x", Some(int(2))),
            create_block(vec![
                create_block(vec![
                    create_let("y", Some(create_let_ref_with_scope("x", Some(2))))
                ]),
                create_let("x", Some(int(3))),
                create_block(vec![
                    create_let("y", Some(create_let_ref_with_scope("x", Some(1))))
                ])
            ])
        ])
    }

    #[test]
    fn resolve_unknown_reference() {
        analyze_error("
        let x = 2;
        let y = t;
        {
            {
                let n = i;
                {
                    z
                }
            }
            let i = 2;
        }
        ", vec![
            PassesError::CannotFindValue("t".to_string(), Span::fake()),
            PassesError::CannotFindValue("i".to_string(), Span::fake()),
            PassesError::CannotFindValue("z".to_string(), Span::fake()),
        ])
    }
}