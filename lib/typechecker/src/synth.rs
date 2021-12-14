use std::{cell::RefCell, rc::Rc};

use crate::Type;
use crate::{types::DataStructType, Env::Env};
use ast::{Expression, Literal};

pub fn check(expr: &Expression, ttype: Type, env: &Rc<RefCell<Env>>) {
    if expr.is_array_lit() {
        return check_array(&expr, ttype, env);
    }

    let synth_type = synth(&expr, env);

    if !synth_type.is_subtype(ttype.clone()) {
        panic!("Expected {}, got {}", ttype, synth_type)
    }
}

pub fn check_array(expr: &Expression, ttype: Type, env: &Rc<RefCell<Env>>) {
    let arr = match expr {
        Expression::Literal(lit) => match &lit.0 {
            Literal::Array(arr) => arr,
            _ => unreachable!(),
        },
        _ => unreachable!(),
    };

    arr.into_iter().for_each(|e| check(e, ttype.clone(), env));
}

pub fn synth(expr: &Expression, env: &Rc<RefCell<Env>>) -> Type {
    match expr {
        Expression::Literal(lit) => match &lit.0 {
            ast::Literal::Int(_) => Type::Int,
            ast::Literal::Float(_) => Type::Float,
            ast::Literal::Bool(_) => Type::Bool,
            ast::Literal::String(_) => Type::String,
            ast::Literal::Array(arr) => match arr.is_empty() {
                false => {
                    let first = synth(&arr[0], env);

                    arr.into_iter().for_each(|e| {
                        let t = synth(e, env);
                        if first != t {
                            panic!("Expected {}, got {}", first, t);
                        }
                    });

                    return Type::Array(Box::new(first.clone()));
                }
                true => Type::Array(Box::new(Type::Unknown)),
            },
            ast::Literal::Null => todo!(),
        },
        Expression::Let(expr) => {
            let expr = &expr.0;

            if let Some(t) = &expr.name.value_type {
                let ttype = Type::from_ast_type(t.clone());
                if let Some(e) = &expr.value {
                    check(e, ttype.clone(), env);
                }
                env.borrow_mut().set(&expr.name.value, ttype);
            } else if let Some(e) = &expr.value {
                let ttype = synth(e, env);
                env.borrow_mut().set(&expr.name.value, ttype);
            }

            Type::Unit
        }
        Expression::LetRef(expr) => {
            let expr = &expr.0;
            match env.borrow_mut().get(&expr.name.value) {
                Some(t) => t,
                None => panic!("Reference undefined variable {}", expr.name),
            }
        }
        Expression::Assign(expr) => {
            let expr = &expr.0;
            let ttype = match env.borrow_mut().get(&expr.name.value) {
                Some(t) => t,
                None => panic!("Reference undefined variable {}", expr.name),
            };

            check(&expr.rhs, ttype.clone(), env);

            ttype
        }
        Expression::Block(expr) => {
            let expr = &expr.0;
            let env = Rc::new(RefCell::new(Env::with_parent(Rc::clone(&env))));

            let mut ret_type = Type::Unit;

            for e in &expr.exprs {
                let ttype = synth(e, &env);
                if matches!(e, Expression::ImplicitReturn(_)) || matches!(e, Expression::Return(_))
                {
                    ret_type = ttype
                }
            }

            ret_type
        }
        Expression::ImplicitReturn(expr) => {
            let expr = &expr.0;
            synth(&expr.value, &env)
        }
        Expression::Return(expr) => {
            let expr = &expr.0;
            match &*expr.value {
                Some(e) => synth(e, env),
                None => Type::Unit,
            }
        }
        Expression::Function(expr) => {
            let expr = &expr.0;

            let env = Rc::new(RefCell::new(Env::with_parent(Rc::clone(&env))));

            let param_types: Vec<Type> = expr
                .params
                .iter()
                .map(|e| {
                    let ttype = Type::from_ast_type(e.value_type.as_ref().unwrap().clone());
                    env.borrow_mut().set(&e.value, ttype.clone());
                    ttype
                })
                .collect();

            let ret_type = Type::from_ast_type(expr.return_type.clone());

            check(&expr.body, ret_type.clone(), &env);

            Type::Function(param_types, Box::new(ret_type))
        }
        Expression::Index(expr) => {
            let expr = &expr.0;
            let obj = synth(&expr.lhs, env);
            let idx = synth(&expr.index, env);

            if !idx.is_int() {
                panic!("Can only index arrays with int, got {}", idx);
            }

            let ret = match obj {
                Type::Array(t) => t,
                _ => panic!("Can only index arrays"),
            };

            *ret
        }
        Expression::Call(expr) => {
            let expr = &expr.0;

            let callee = synth(&expr.callee, env);

            match callee {
                Type::Function(params, ret) => {
                    if params.len() != expr.arguments.len() {
                        panic!(
                            "Exepcted {} args, got {}",
                            params.len(),
                            expr.arguments.len()
                        )
                    }

                    for (t, e) in params.iter().zip(expr.arguments.iter()) {
                        check(e, t.clone(), &env);
                    }
                    *ret
                }
                _ => panic!("Can only call functions"),
            }
        }
        Expression::DataStruct(expr) => {
            let expr = &expr.0;

            let mut fields = vec![];
            for field in &expr.fields {
                let v = field.value_type.as_ref().unwrap().clone();
                let ttype = Type::from_ast_type(v);
                fields.push(DataStructType {
                    name: field.value.to_string(),
                    ttype: ttype,
                })
            }

            let ttype = Type::DataStruct(expr.name.value.to_string(), fields);

            env.borrow_mut().set(&expr.name.value, ttype.clone());

            Type::Unit
        }
        Expression::DataStructInstance(expr) => {
            let expr = &expr.0;

            let ttype = match env.borrow_mut().get(&expr.name.value) {
                Some(t) => t,
                None => panic!("Reference undefined variable {}", expr.name),
            };

            match &ttype {
                Type::DataStruct(_, fields) => {
                    for field in fields {
                        let prop = expr.fields.iter().find(|p| p.name.value == field.name);
                        match prop {
                            None => panic!("Missing propery {}", field.name),
                            _ => {}
                        }
                    }

                    for field in &expr.fields {
                        let prop = fields.iter().find(|p| p.name == field.name.value);
                        match prop {
                            Some(p) => check(&field.value, p.ttype.clone(), env),
                            None => panic!("Extra property {}", field.name.value),
                        }
                    }

                    ttype
                }
                _ => panic!("Can only instantiate data structs"),
            }
        }
        Expression::GetProperty(expr) => {
            let expr = &expr.0;

            let ttype = synth(&expr.object, env);

            match ttype {
                Type::DataStruct(_, t) => {
                    let prop = t.iter().find(|e| e.name == expr.name.value);
                    match prop {
                        Some(p) => p.ttype.clone(),
                        None => panic!("Unknown property {}", expr.name.value),
                    }
                }
                _ => panic!("Can only get properties from data structs"),
            }
        }
        _ => todo!("unimplemented {:#?}", expr),
    }
}

pub fn typecheck(exprs: &Vec<Expression>, parent_env: Option<Rc<RefCell<Env>>>) -> Vec<Type> {
    let env = match parent_env {
        Some(env) => Env::with_parent(env),
        None => Env::new(),
    };
    let env = Rc::new(RefCell::new(env));
    let types = exprs.into_iter().map(|e| synth(e, &env)).collect();
    types
}

#[cfg(test)]
mod test {
    use crate::typecheck;
    use parser::Parser;
    use scanner::Scanner;

    fn parse(src: &str) {
        let scanner = Scanner::new(src.into());
        let mut parser = Parser::new(scanner);

        let (exprs, errors) = parser.parse();

        if !errors.is_empty() {
            panic!("Parser emitted errors {:#?}", errors);
        }

        let res = typecheck(&exprs, None);

        println!("res: {:#?}", res);
    }

    #[test]
    fn test_synth() {
        parse(r#"1; 2.2; "test"; true; false;"#);
    }

    #[test]
    fn test_array_synth() {
        parse(r#"[1];"#);
    }

    #[test]
    fn test_let_synth() {
        parse(r#"let x: string; x = "test"; let x = 1; x = 2;"#);
    }

    #[test]
    fn test_block_synth() {
        parse(
            r#"
            let x = 1;
            { 
                let f = 2;
                x = 2;
                f = 1;
            };
            x = 1;
        "#,
        );
    }

    #[test]
    fn test_function_synth() {
        parse(
            "
            let main = fun(x: int): int => x;
            main;
        ",
        );
    }

    #[test]
    fn test_function_call_synth() {
        parse(
            r#"
            let main = fun(): int => 1;
            main();
            let main = fun(x: int, y: int): int => x;
            let y = 2;
            main(1, y);
        "#,
        );
    }

    #[test]
    fn test_index_synth() {
        parse(
            "
            let x = [1,2,3];

            let y = x[0];
            x;y;
        ",
        )
    }

    #[test]
    fn test_data_struct_synth() {
        parse(
            r#"
            data Person {
                id: int,
                name: string,
                is_valid: bool,
            };

            let person = Person { id: 1, name: "test", is_valid: true };
            person;
            let x = person.is_valid;
            x;
        "#,
        )
    }
}
