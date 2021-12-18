use std::{cell::RefCell, rc::Rc};

use crate::types::{DataStructField, DataStructMethod, Singleton, Type};
use crate::Env::Env;
use ast::{Expression, Function};

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
            ast::Literal::Array(arr) => arr,
            _ => unreachable!(),
        },
        _ => unreachable!(),
    };

    arr.into_iter().for_each(|e| check(e, ttype.clone(), env));
}

pub fn synth_function_signature(expr: &Function, env: &Rc<RefCell<Env>>) -> Type {
    let param_types: Vec<Type> = expr
        .params
        .iter()
        .map(|e| {
            let ttype = Type::from_ast_type(e.value_type.as_ref().unwrap().clone(), &env);
            ttype
        })
        .collect();

    let ret_type = Type::from_ast_type(expr.return_type.clone(), &env);

    Type::Function(param_types, Box::new(ret_type))
}

pub fn synth(expr: &Expression, env: &Rc<RefCell<Env>>) -> Type {
    match expr {
        Expression::Literal(lit) => match &lit.0 {
            ast::Literal::Int(_) => Type::int(),
            ast::Literal::Float(_) => Type::float(),
            ast::Literal::Bool(_) => Type::bool(),
            ast::Literal::String(_) => Type::string(),
            ast::Literal::Array(arr) => match arr.is_empty() {
                false => {
                    let first = synth(&arr[0], env);

                    arr.into_iter().for_each(|e| {
                        let t = synth(e, env);
                        if first != t {
                            panic!("Expected {}, got {}", first, t);
                        }
                    });

                    return Type::array(first.clone());
                }
                true => Type::array(Type::Unknown),
            },
            ast::Literal::Null => todo!(),
        },
        Expression::BreakExpr(_) => Type::Unit,
        Expression::ContinueExpr(_) => Type::Unit,
        Expression::Let(expr) => {
            let expr = &expr.0;

            if let Some(t) = &expr.name.value_type {
                let ttype = Type::from_ast_type(t.clone(), env);
                if let Some(e) = &expr.value {
                    check(e, ttype.clone(), env);
                }
                env.borrow_mut().set(&expr.name.value, ttype);
            } else if let Some(e) = &expr.value {
                let ttype = synth(e, env);
                env.borrow_mut().set(&expr.name.value, ttype);
            } else {
                env.borrow_mut().set(&expr.name.value, Type::Unknown);
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
            let fn_type = synth_function_signature(expr, &env);
            let env = Rc::new(RefCell::new(Env::with_parent(Rc::clone(&env))));

            let ret_type = match &fn_type {
                Type::Function(params, ret) => {
                    for (e, ttype) in expr.params.iter().zip(params) {
                        env.borrow_mut().set(&e.value, ttype.clone());
                    }
                    ret
                }
                _ => unreachable!(),
            };

            check(&expr.body, *ret_type.clone(), &env);

            fn_type
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
        Expression::SetIndex(expr) => {
            let expr = &expr.0;
            let obj = synth(&expr.lhs, env);
            let idx = synth(&expr.index, env);

            if !idx.is_int() {
                panic!("Can only index arrays with int, got {}", idx);
            }

            match obj {
                Type::Array(t) => {
                    check(&expr.value, *t.clone(), env);
                    *t
                }
                _ => panic!("Can only index arrays"),
            }
        }
        Expression::Call(expr) => {
            let expr = &expr.0;

            let callee = synth(&expr.callee, env);

            match callee {
                Type::Function(params, ret) => {
                    // TODO handle self param in methods
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
                _ => panic!("Can only call functions, got {}", callee),
            }
        }
        Expression::DataStruct(expr) => {
            let expr = &expr.0;
            let filler = Type::data_struct(expr.name.value.to_string(), vec![], vec![]);

            // register data struct name making it possible to rerefence in methods
            env.borrow_mut().set(&expr.name.value, filler);

            let mut fields = vec![];
            let mut methods = vec![];

            for field in &expr.fields {
                let v = field.value_type.as_ref().unwrap().clone();
                let ttype = Type::from_ast_type(v, &env);
                fields.push(DataStructField {
                    name: field.value.to_string(),
                    ttype,
                });
            }

            // save data struct with fields only
            let filler =
                Type::data_struct(expr.name.value.to_string(), fields.clone(), methods.clone());
            env.borrow_mut().set(&expr.name.value, filler);

            // first pass over function collects all methods signature
            for method in &expr.methods {
                match method {
                    Expression::Function(expr) => {
                        let name = expr.0.name.as_ref().unwrap().to_string();

                        let ttype = synth_function_signature(&expr.0, env);
                        methods.push(DataStructMethod {
                            name,
                            ttype,
                            is_static: expr.0.is_static,
                        });
                    }
                    _ => unreachable!(),
                };
            }

            // set first pass
            let filler =
                Type::data_struct(expr.name.value.to_string(), fields.clone(), methods.clone());
            env.borrow_mut().set(&expr.name.value, filler.clone());

            let mut methods = vec![];

            // update references to the method with updated method signature
            //  data Person { fun new: Person {  } }
            for method in &expr.methods {
                match method {
                    Expression::Function(expr) => {
                        let name = expr.0.name.as_ref().unwrap().to_string();
                        let ttype = synth_function_signature(&expr.0, env);
                        methods.push(DataStructMethod {
                            name,
                            ttype,
                            is_static: expr.0.is_static,
                        });
                    }
                    _ => unreachable!(),
                };
            }

            let ttype =
                Type::data_struct(expr.name.value.to_string(), fields.clone(), methods.clone());
            env.borrow_mut().set(&expr.name.value, ttype.clone());

            for method in &expr.methods {
                synth(&method, env);
            }

            let ttype =
                Type::data_struct(expr.name.value.to_string(), fields.clone(), methods.clone());
            env.borrow_mut().set(&expr.name.value, ttype.clone());

            Type::Unit
        }
        Expression::DataStructInstance(expr) => {
            let expr = &expr.0;

            let ttype = match env.borrow_mut().get(&expr.name.value) {
                Some(t) => t,
                None => panic!("Reference undefined data struct {}", expr.name),
            };

            match &ttype {
                Type::DataStruct(d) => {
                    for field in &d.fields {
                        let prop = expr.fields.iter().find(|p| p.name.value == field.name);
                        match prop {
                            None => panic!("Missing propery {}", field.name),
                            _ => {}
                        }
                    }

                    for field in &expr.fields {
                        let prop = d.fields.iter().find(|p| p.name == field.name.value);
                        match prop {
                            Some(p) => check(&field.value, p.ttype.clone(), env),
                            None => panic!("Extra property {}", field.name.value),
                        }
                    }

                    Type::DataStructInstance(d.clone())
                }
                _ => panic!("Can only instantiate data structs"),
            }
        }
        Expression::SelfExpr(expr) => match env.borrow_mut().get(&expr.0.name) {
            Some(t) => t,
            None => unreachable!(),
        },
        Expression::GetProperty(expr) => {
            let expr = &expr.0;

            let ttype = synth(&expr.object, env);

            match &ttype {
                Type::DataStruct(d) | Type::DataStructInstance(d) => {
                    let is_instance = ttype.is_data_struct_instance();
                    let prop = match expr.is_callable {
                        true => match d.methods.iter().find(|e| e.name == expr.name.value) {
                            Some(t) => {
                                if is_instance && t.is_static {
                                    panic!("Cannot call static method from instance");
                                }
                                Some(t.ttype.clone())
                            }
                            None => todo!(),
                        },
                        false => match d.fields.iter().find(|e| e.name == expr.name.value) {
                            Some(d) => Some(d.ttype.clone()),
                            None => None,
                        },
                    };

                    match prop {
                        Some(p) => p,
                        None => panic!("Unknown property {}", expr.name.value),
                    }
                }
                // Type::DataStructInstance(t) => {
                //     let (m, t) = match *t {
                //         Type::DataStruct(_, t, m) => (m, t),
                //         _ => unreachable!(),
                //     };

                //     let prop = match expr.is_callable {
                //         true => m.iter().find(|e| e.name == expr.name.value),
                //         false => t.iter().find(|e| e.name == expr.name.value),
                //     };

                //     match prop {
                //         Some(p) => p.ttype.clone(),
                //         None => panic!("Unknown property {}", expr.name.value),
                //     }
                // }
                _ => panic!("Can only get properties from data structs"),
            }
        }
        Expression::SetProperty(expr) => {
            let expr = &expr.0;

            let ttype = synth(&expr.object, env);

            // TODO same as the get property
            let prop_type = match ttype {
                Type::DataStruct(d) | Type::DataStructInstance(d) => {
                    let prop = d.fields.iter().find(|e| e.name == expr.name.value);
                    match prop {
                        Some(p) => p.ttype.clone(),
                        None => panic!("Unknown propert {}", expr.name.value),
                    }
                }
                _ => panic!("Can only set properties from data structs"),
            };

            check(&expr.value, prop_type.clone(), env);

            prop_type
        }
        Expression::Grouping(expr) => synth(&expr.0.expr, env),

        Expression::BinOp(expr) => {
            let expr = &expr.0;
            let lhs = synth(&expr.left, env);
            let rhs = synth(&expr.right, env);

            match &expr.op {
                ast::BinaryOperation::Add
                | ast::BinaryOperation::Substract
                | ast::BinaryOperation::Multiply
                | ast::BinaryOperation::Divide => match (&lhs, &rhs) {
                    (Type::Singleton(Singleton::Int), Type::Singleton(Singleton::Int)) => {
                        Type::int()
                    }
                    (Type::Singleton(Singleton::Float), Type::Singleton(Singleton::Float)) => {
                        Type::float()
                    }
                    _ => {
                        let msg = match &expr.op {
                            ast::BinaryOperation::Add => format!("Cannot add {} to {}", rhs, lhs),
                            ast::BinaryOperation::ConcatInterpolation => todo!(),
                            ast::BinaryOperation::Substract => {
                                format!("Cannot substract {} from {}", rhs, lhs)
                            }
                            ast::BinaryOperation::Multiply => {
                                format!("Cannot multiply {} with {}", lhs, rhs)
                            }
                            ast::BinaryOperation::Divide => {
                                format!("Cannot divide {} with {}", lhs, rhs)
                            }
                        };
                        panic!("{}", msg);
                    }
                },
                ast::BinaryOperation::ConcatInterpolation => todo!(),
            }
        }
        Expression::UnaryOp(expr) => {
            let expr = &expr.0;
            let rhs = synth(&expr.rhs, env);

            match &expr.op {
                ast::UnaryOperation::Minus | ast::UnaryOperation::Plus => match rhs {
                    Type::Singleton(Singleton::Int) | Type::Singleton(Singleton::Float) => {
                        Type::int()
                    }
                    _ => panic!("Invalid unary operation on {}", rhs),
                },
                ast::UnaryOperation::Not => match rhs {
                    Type::Singleton(Singleton::Bool) => Type::bool(),
                    _ => panic!("Invalid unary operation on {}", rhs),
                },
            }
        }
        Expression::Logic(expr) => {
            let expr = &expr.0;
            let lhs = synth(&expr.lhs, env);
            let rhs = synth(&expr.rhs, env);

            match &expr.op {
                ast::LogicOperation::Or | ast::LogicOperation::And => match (&lhs, &rhs) {
                    (Type::Singleton(Singleton::Bool), Type::Singleton(Singleton::Bool)) => {
                        Type::bool()
                    }
                    (_, Type::Singleton(Singleton::Bool)) => panic!("Expected bool got {}", lhs),
                    (Type::Singleton(Singleton::Bool), _) => panic!("Expected bool got {}", rhs),
                    _ => panic!("Expected bool got {}", lhs),
                },

                ast::LogicOperation::Equal
                | ast::LogicOperation::NotEqual
                | ast::LogicOperation::LessThan
                | ast::LogicOperation::LessThanEqual
                | ast::LogicOperation::GreaterThan
                | ast::LogicOperation::GreaterThanEqual => match (&lhs, &rhs) {
                    (Type::Singleton(Singleton::Int), Type::Singleton(Singleton::Int)) => {
                        Type::bool()
                    }
                    (Type::Singleton(Singleton::Float), Type::Singleton(Singleton::Float)) => {
                        Type::bool()
                    }
                    (Type::Singleton(Singleton::String), Type::Singleton(Singleton::String)) => {
                        Type::bool()
                    }
                    (Type::Singleton(Singleton::Bool), Type::Singleton(Singleton::Bool)) => {
                        Type::bool()
                    }
                    _ => panic!("Cannot compare {} and {}", lhs, rhs),
                },
            }
        }
        Expression::If(expr) => {
            let expr = &expr.0;
            let cond = synth(&expr.condition, env);

            if !cond.is_bool() {
                panic!("Expeced bool got {}", cond);
            }

            let then = synth(&expr.then, env);

            if let Some(not_then) = &expr.not_then {
                let not_then = synth(not_then, env);
                if then != not_then {
                    panic!(
                        "If expression has incompatible types expected {}, found {}",
                        then, not_then
                    )
                }
            }

            then
        }
        Expression::LoopExpr(expr) => {
            let expr = &expr.0;
            if let Some(e) = &expr.iterator {
                let it = synth(e, env);
                let env = Rc::new(RefCell::new(Env::with_parent(env.clone())));
                let iterator_type = match it {
                    Type::Array(arr) => match *arr {
                        Type::Unknown => panic!("type annotations needed, cannot infer type"),
                        _ => *arr,
                    },
                    _ => panic!("Cannot only iterate over arrays got {}", it),
                };

                let name = match &*expr.condition {
                    Expression::Let(l) => l.0.name.value.to_string(),
                    _ => unreachable!(),
                };

                // set let expression to unknown value
                synth(&expr.condition, &env);

                // update let expression with iterator type
                env.borrow_mut().set(&name, iterator_type);

                synth(&expr.body, &env);
            } else {
                let cond = synth(&expr.condition, env);
                if !cond.is_bool() {
                    panic!("Expected bool got {}", cond);
                }
            }

            Type::Unit
        }
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
    fn test_function_callback_synth() {
        // TODO dont throw annnotation needed error in parser
        parse(
            "
            let main = fun(cb: Fun(id: int)) {
                cb(1);
            };

            main(fun(id) => id == 1);
        ",
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
        );

        parse(
            r#"
            let x = ["test", "test2", "test3"];
            
            x[0] = "test";

            x;
        "#,
        );
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

            // let Person = Person { id: 1, name: "test", is_valid: true };
            // Person;
            let p = Person { id: 1, name: "test", is_valid: true };
            p;
            let x = Person.is_valid;
            x;
            
            let main = fun(p: Person): bool {
                p.is_valid
            };
            main(p);
        "#,
        )
    }

    #[test]
    fn test_data_struct_set_synth() {
        parse(
            r#"
            data Person {
                id: int,
                name: string,
                is_valid: bool,
            };

            let gen_id = fun(): string {
                233
            };
            let person = Person {id: 1, name: "test", is_valid: true};
            person.id = gen_id();
            "#,
        );
    }

    #[test]
    fn test_data_struct_methods_synth() {
        parse(
            r#"
            data Person { id: int } :: {
                fun new: Person {
                    let p = Person { id: 1 };
                    p
                }

                fun get_int(self) {
        
                }
            };
            
            let p = Person.new();
            p;
            let main = fun(t: Person): int {
                t.get_int(t);
                let nn = fun(p: Person): int {
                    p.get_int(p);
                    p.id
                };
                let p2 = nn(t);
                p2
            };

            main(p);
          
        "#,
        );
    }

    #[test]
    fn test_grouping_expr_synth() {
        parse(
            r#"
            (123);
            (2.2);
            ("test");
            (true);
        "#,
        );
    }

    #[test]
    fn test_binop_synth() {
        parse(
            "
            let x = 1;
            let y = 2.0;
            x / y;

        ",
        );
    }

    #[test]
    fn test_unary_synth() {
        parse(
            "
            -1;
            -2.0;
            !false;
            true;
        ",
        );
    }

    #[test]
    fn test_logic_synth() {
        parse(
            r#"
        true && true;
        true || true;

        1 == 2 && 2 == 1;
        1 >= 2;
        2 <= 1;
        "test" == "test";
        "#,
        );
    }

    #[test]
    fn test_if_synth() {
        parse(
            r#"
            if true {
                1;
            };

            if true {
                1
            };

            if 1 == 2 {

            };

            if 1 == 2 {
                2.0
            } else {
                2.0
            };
        "#,
        );
    }

    #[test]
    fn test_loop_synth() {
        // let x = vec![];

        parse(
            "
        let x = 1;
        loop x < 2 {

        };

        let x = [1];

        loop i in x {
            if i  < 1 {

            }
        };

        ",
        );
    }
}
