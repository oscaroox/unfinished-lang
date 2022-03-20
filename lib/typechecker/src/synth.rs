use std::{cell::RefCell, rc::Rc};

use crate::env::Env;
use crate::type_error::TypeError;
use crate::types::{DataStructField, DataStructMethod, Singleton, Type};
use ast::{Expression, Function};

pub struct TypeChecker {
    errors: Vec<TypeError>,
}

impl TypeChecker {
    pub fn new() -> TypeChecker {
        TypeChecker { errors: vec![] }
    }

    pub fn type_check(
        &mut self,
        exprs: &Vec<Expression>,
        parent_env: Option<Rc<RefCell<Env>>>,
    ) -> (Vec<Type>, Vec<TypeError>) {
        let env = match parent_env {
            Some(env) => Env::with_parent(env),
            None => Env::new(),
        };
        let env = Rc::new(RefCell::new(env));
        let mut types = vec![];
        for e in exprs {
            match self.synth(e, &env) {
                Ok(ttype) => types.push(ttype),
                Err(error) => self.add_error(error),
            }
        }

        (types, self.errors.clone())
    }

    fn add_error(&mut self, error: TypeError) {
        match error {
            TypeError::LeftAndRight(e1, e2) => {
                self.add_error(*e1);
                self.add_error(*e2);
            }
            _ => self.errors.push(error),
        }
    }

    fn synth(&self, expr: &Expression, env: &Rc<RefCell<Env>>) -> Result<Type, TypeError> {
        match expr {
            Expression::Literal(lit) => Ok(match &lit.0 {
                ast::Literal::Int(_) => Type::int(),
                ast::Literal::Float(_) => Type::float(),
                ast::Literal::Bool(_) => Type::bool(),
                ast::Literal::String(_) => Type::string(),
                ast::Literal::Array(arr) => match arr.is_empty() {
                    false => {
                        let first = self.synth(&arr[0], env)?;

                        for e in arr {
                            let t = self.synth(e, env)?;
                            if first != t {
                                return Err(TypeError::MismatchedTypes(first, t, e.get_span()));
                            }
                        }

                        return Ok(Type::array(first.clone()));
                    }
                    true => Type::array(Type::Unknown),
                },
                ast::Literal::Null => todo!(),
            }),
            Expression::BreakExpr(_) => Ok(Type::Unit),
            Expression::ContinueExpr(_) => Ok(Type::Unit),
            Expression::Let(expr) => {
                let expr = &expr.0;

                if let Some(t) = &expr.name.value_type {
                    let ttype = Type::from_ast_type(t.clone(), env);
                    if let Some(e) = &expr.value {
                        self.check(e, ttype.clone(), env)?;
                    }
                    env.borrow_mut().set(&expr.name.value, ttype);
                } else if let Some(e) = &expr.value {
                    let ttype = self.synth(e, env)?;
                    env.borrow_mut().set(&expr.name.value, ttype);
                } else {
                    env.borrow_mut().set(&expr.name.value, Type::Unknown);
                }

                Ok(Type::Unit)
            }
            Expression::LetRef(expr) => {
                let let_ref = &expr.0;
                match env.borrow_mut().get(&let_ref.name.value) {
                    Some(t) => Ok(t),
                    None => Err(TypeError::UndefinedVariable(
                        let_ref.name.value.to_string(),
                        expr.1.clone(),
                    )),
                }
            }
            Expression::Assign(expr) => {
                let ttype = match env.borrow_mut().get(&expr.name.value) {
                    Some(t) => t,
                    None => {
                        return Err(TypeError::UndefinedVariable(
                            expr.name.value.to_string(),
                            expr.span.clone(),
                        ))
                    }
                };

                self.check(&expr.rhs, ttype.clone(), env)?;

                Ok(ttype)
            }
            Expression::Block(expr) => {
                let expr = &expr.0;
                let env = Rc::new(RefCell::new(Env::with_parent(Rc::clone(&env))));

                let mut ret_type = Type::Unit;

                for e in &expr.exprs {
                    let ttype = self.synth(e, &env)?;
                    if matches!(e, Expression::ImplicitReturn(_))
                        || matches!(e, Expression::Return(_))
                    {
                        ret_type = ttype
                    }
                }

                Ok(ret_type)
            }
            Expression::ImplicitReturn(expr) => {
                let expr = &expr.0;
                self.synth(&expr.value, &env)
            }
            Expression::Return(expr) => {
                let expr = &expr.0;
                match &*expr.value {
                    Some(e) => self.synth(e, env),
                    None => Ok(Type::Unit),
                }
            }
            Expression::Function(expr) => {
                let fn_type = self.synth_function_signature(expr, &env)?;
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

                self.check(&expr.body, *ret_type.clone(), &env)?;

                Ok(fn_type)
            }
            Expression::GetIndex(expr) => {
                let obj = self.synth(&expr.lhs, env)?;
                let idx = self.synth(&expr.index, env)?;

                if !idx.is_int() {
                    return Err(TypeError::CannotIndexByType(idx, expr.index.get_span()));
                }

                match obj {
                    Type::Array(t) => Ok(*t),
                    _ => Err(TypeError::CannotIndex(obj, expr.lhs.get_span())),
                }
            }
            Expression::SetIndex(expr) => {
                let obj = self.synth(&expr.lhs, env)?;
                let idx = self.synth(&expr.index, env)?;

                if !idx.is_int() {
                    return Err(TypeError::CannotIndexByType(idx, expr.index.get_span()));
                }

                match obj {
                    Type::Array(t) => {
                        self.check(&expr.value, *t.clone(), env)?;
                        Ok(*t)
                    }
                    _ => Err(TypeError::CannotIndex(obj, expr.lhs.get_span())),
                }
            }
            Expression::Call(expr) => {
                let callee = self.synth(&expr.callee, env)?;

                match callee {
                    Type::Function(params, ret) => {
                        // TODO handle self param in methods
                        if params.len() != expr.arguments.len() {
                            return Err(TypeError::ExpectedArguments(
                                params.len(),
                                expr.arguments.len(),
                                expr.span.clone(),
                            ));
                        }
                        for (t, e) in params.iter().zip(expr.arguments.iter()) {
                            self.check(e, t.clone(), &env)?;
                        }
                        Ok(*ret)
                    }
                    _ => Err(TypeError::NotCallable(callee, expr.callee.get_span())),
                }
            }
            Expression::DataStruct(expr) => {
                let filler = Type::data_struct(expr.name.value.to_string(), vec![], vec![]);

                // register data struct name making it possible to reference in methods
                env.borrow_mut().set(&expr.name.value, filler);

                let mut fields = vec![];
                let mut methods = vec![];

                for field in &expr.fields {
                    let t = match &field.value_type {
                        Some(t) => t.clone(),
                        None => panic!("Type Annotation needed"), // TODO: get better spans for identifier
                    };
                    let ttype = Type::from_ast_type(t, &env);
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
                            let name = expr.name.as_ref().unwrap().to_string();

                            let ttype = self.synth_function_signature(&expr, env)?;
                            methods.push(DataStructMethod {
                                name,
                                ttype,
                                is_static: expr.is_static,
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
                            let name = expr.name.as_ref().unwrap().to_string();
                            let ttype = self.synth_function_signature(&expr, env)?;
                            methods.push(DataStructMethod {
                                name,
                                ttype,
                                is_static: expr.is_static,
                            });
                        }
                        _ => unreachable!(),
                    };
                }

                let ttype =
                    Type::data_struct(expr.name.value.to_string(), fields.clone(), methods.clone());
                env.borrow_mut().set(&expr.name.value, ttype.clone());

                for method in &expr.methods {
                    self.synth(&method, env)?;
                }

                let ttype =
                    Type::data_struct(expr.name.value.to_string(), fields.clone(), methods.clone());
                env.borrow_mut().set(&expr.name.value, ttype.clone());

                Ok(Type::Unit)
            }
            Expression::DataStructInstance(expr) => {
                let instance = &expr.0;
                let ttype = match env.borrow_mut().get(&instance.name.value) {
                    Some(t) => t,
                    None => {
                        return Err(TypeError::UndefinedVariable(
                            instance.name.value.to_string(),
                            expr.1.clone(),
                        ));
                    }
                };

                match &ttype {
                    Type::DataStruct(d) => {
                        for field in &d.fields {
                            let prop = instance.fields.iter().find(|p| p.name.value == field.name);
                            match prop {
                                None => {
                                    return Err(TypeError::MissingProperty(
                                        field.name.to_string(),
                                        expr.1.clone(),
                                    ));
                                }
                                _ => {}
                            }
                        }

                        for field in &instance.fields {
                            let prop = d.fields.iter().find(|p| p.name == field.name.value);
                            match prop {
                                Some(p) => self.check(&field.value, p.ttype.clone(), env)?,
                                None => {
                                    return Err(TypeError::ExtraProperty(
                                        field.name.value.to_string(),
                                        expr.1.clone(),
                                    ));
                                }
                            }
                        }

                        Ok(Type::DataStructInstance(d.clone()))
                    }
                    _ => Err(TypeError::InvalidDataStructInstantiation(expr.1.clone())),
                }
            }
            Expression::SelfExpr(expr) => match env.borrow_mut().get(&expr.0.name) {
                Some(t) => Ok(t),
                None => unreachable!(),
            },
            Expression::GetProperty(expr) => {
                let ttype = self.synth(&expr.object, env)?;

                match &ttype {
                    Type::DataStruct(d) | Type::DataStructInstance(d) => {
                        let is_instance = ttype.is_data_struct_instance();
                        let prop = match expr.is_callable {
                            true => match d.methods.iter().find(|e| e.name == expr.name.value) {
                                Some(t) => {
                                    if is_instance && t.is_static {
                                        return Err(TypeError::CannotCallStaticMethod(
                                            expr.span.clone(),
                                        ));
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
                            Some(p) => Ok(p),
                            None => {
                                return Err(TypeError::UnknownProperty(
                                    expr.name.value.to_string(),
                                    expr.span.clone(),
                                ))
                            }
                        }
                    }
                    _ => Err(TypeError::InvalidPropertyAccess(
                        ttype,
                        expr.object.get_span(),
                    )),
                }
            }
            Expression::SetProperty(expr) => {
                let ttype = self.synth(&expr.object, env)?;

                // TODO same as the get property
                let prop_type = match ttype {
                    Type::DataStruct(d) | Type::DataStructInstance(d) => {
                        let prop = d.fields.iter().find(|e| e.name == expr.name.value);
                        match prop {
                            Some(p) => p.ttype.clone(),
                            None => {
                                return Err(TypeError::UnknownProperty(
                                    expr.name.value.to_string(),
                                    expr.span.clone(),
                                ))
                            }
                        }
                    }
                    _ => return Err(TypeError::InvalidPropertyAccess(ttype, expr.span.clone())),
                };

                self.check(&expr.value, prop_type.clone(), env)?;

                Ok(prop_type)
            }
            Expression::Grouping(expr) => self.synth(&expr.0.expr, env),

            Expression::BinOp(expr) => {
                let binop = &expr;

                let (lhs, rhs) = self.synth_left_and_right(&binop.left, &binop.right, env)?;

                match &binop.op {
                    ast::BinaryOperation::Add(_)
                    | ast::BinaryOperation::Substract(_)
                    | ast::BinaryOperation::Multiply(_)
                    | ast::BinaryOperation::Divide(_) => match (&lhs, &rhs) {
                        (Type::Singleton(Singleton::Int), Type::Singleton(Singleton::Int)) => {
                            Ok(Type::int())
                        }
                        (Type::Singleton(Singleton::Float), Type::Singleton(Singleton::Float)) => {
                            Ok(Type::float())
                        }
                        _ => Err(TypeError::InvalidBinaryOperation(
                            binop.op.to_string(),
                            lhs,
                            rhs,
                            expr.span.clone(),
                        )),
                    },
                    ast::BinaryOperation::ConcatInterpolation => todo!(),
                }
            }
            Expression::UnaryOp(expr) => {
                let rhs = self.synth(&expr.rhs, env)?;

                match &expr.op {
                    ast::UnaryOperation::Minus(_) | ast::UnaryOperation::Plus(_) => match rhs {
                        Type::Singleton(Singleton::Int) => Ok(Type::int()),
                        Type::Singleton(Singleton::Float) => Ok(Type::float()),
                        _ => Err(TypeError::InvalidUnaryOperation(
                            expr.op.to_string(),
                            rhs,
                            expr.span.clone(),
                        )),
                    },
                    ast::UnaryOperation::Not(_) => match rhs {
                        Type::Singleton(Singleton::Bool) => Ok(Type::bool()),
                        _ => Err(TypeError::InvalidUnaryOperation(
                            expr.op.to_string(),
                            rhs,
                            expr.span.clone(),
                        )),
                    },
                }
            }
            Expression::Logic(expr) => {
                let expr = &expr.0;

                let (lhs, rhs) = self.synth_left_and_right(&expr.lhs, &expr.rhs, env)?;

                match &expr.op {
                    ast::LogicOperation::Or | ast::LogicOperation::And => match (&lhs, &rhs) {
                        (Type::Singleton(Singleton::Bool), Type::Singleton(Singleton::Bool)) => {
                            Ok(Type::bool())
                        }
                        (_, Type::Singleton(Singleton::Bool)) => {
                            return Err(TypeError::Expected(
                                Type::bool(),
                                lhs,
                                expr.lhs.get_span(),
                            ));
                        }
                        (Type::Singleton(Singleton::Bool), _) => {
                            return Err(TypeError::Expected(
                                Type::bool(),
                                rhs,
                                expr.rhs.get_span(),
                            ));
                        }
                        _ => {
                            return Err(TypeError::LeftAndRight(
                                Box::new(TypeError::Expected(
                                    Type::bool(),
                                    lhs,
                                    expr.lhs.get_span(),
                                )),
                                Box::new(TypeError::Expected(
                                    Type::bool(),
                                    rhs,
                                    expr.rhs.get_span(),
                                )),
                            ))
                        }
                    },

                    ast::LogicOperation::Equal
                    | ast::LogicOperation::NotEqual
                    | ast::LogicOperation::LessThan
                    | ast::LogicOperation::LessThanEqual
                    | ast::LogicOperation::GreaterThan
                    | ast::LogicOperation::GreaterThanEqual => match (&lhs, &rhs) {
                        (Type::Singleton(Singleton::Int), Type::Singleton(Singleton::Int)) => {
                            Ok(Type::bool())
                        }
                        (Type::Singleton(Singleton::Float), Type::Singleton(Singleton::Float)) => {
                            Ok(Type::bool())
                        }
                        (
                            Type::Singleton(Singleton::String),
                            Type::Singleton(Singleton::String),
                        ) => Ok(Type::bool()),
                        (Type::Singleton(Singleton::Bool), Type::Singleton(Singleton::Bool)) => {
                            Ok(Type::bool())
                        }
                        _ => Err(TypeError::CantCompare(lhs, rhs, expr.lhs.get_span())),
                    },
                }
            }
            Expression::If(expr) => {
                let cond = self.synth(&expr.condition, env)?;

                if !cond.is_bool() {
                    return Err(TypeError::Expected(
                        Type::bool(),
                        cond,
                        expr.condition.get_span(),
                    ));
                }

                let then = self.synth(&expr.then, env)?;

                if let Some(not_then) = &expr.not_then {
                    let not_then = self.synth(not_then, env)?;
                    if then != not_then {
                        return Err(TypeError::BranchesIncompatibleTypes(
                            then,
                            not_then,
                            // TODO: span should point to 'if' token instead of condition
                            expr.span.clone(),
                        ));
                    }
                }

                Ok(then)
            }
            Expression::LoopExpr(expr) => {
                let expr = &expr.0;
                if let Some(e) = &expr.iterator {
                    let it = self.synth(e, env)?;
                    let env = Rc::new(RefCell::new(Env::with_parent(env.clone())));
                    let iterator_type = match it {
                        Type::Array(arr) => match *arr {
                            Type::Unknown => {
                                return Err(TypeError::AnnotationNeeded(e.get_span()));
                            }
                            _ => *arr,
                        },
                        _ => {
                            return Err(TypeError::CannotIterate(it, e.get_span()));
                        }
                    };

                    let name = match &*expr.condition {
                        Expression::Let(l) => l.0.name.value.to_string(),
                        _ => unreachable!(),
                    };

                    // set let expression to unknown value
                    self.synth(&expr.condition, &env)?;

                    // update let expression with iterator type
                    env.borrow_mut().set(&name, iterator_type);

                    self.synth(&expr.body, &env)?;
                } else {
                    let cond = self.synth(&expr.condition, env)?;
                    if !cond.is_bool() {
                        return Err(TypeError::Expected(
                            Type::bool(),
                            cond,
                            expr.condition.get_span(),
                        ));
                    }
                }

                Ok(Type::Unit)
            }
        }
    }

    /**
     * Gather errors from Binary and Logical Expressions
     */
    fn synth_left_and_right(
        &self,
        left: &Expression,
        right: &Expression,
        env: &Rc<RefCell<Env>>,
    ) -> Result<(Type, Type), TypeError> {
        let lhs = self.synth(left, env);
        let rhs = self.synth(right, env);

        match (lhs, rhs) {
            (Ok(r1), Ok(r2)) => Ok((r1, r2)),
            (Ok(_), Err(e)) | (Err(e), Ok(_)) => Err(e),
            (Err(e1), Err(e2)) => Err(TypeError::LeftAndRight(Box::new(e1), Box::new(e2))),
        }
    }

    fn synth_function_signature(
        &self,
        expr: &Function,
        env: &Rc<RefCell<Env>>,
    ) -> Result<Type, TypeError> {
        let param_types: Vec<Type> = expr
            .params
            .iter()
            .map(|e| match &e.value_type {
                Some(t) => Type::from_ast_type(t.clone(), &env),
                None => panic!("Annotation type needed"),
            })
            .collect();

        let ret_type = Type::from_ast_type(expr.return_type.clone(), &env);

        Ok(Type::Function(param_types, Box::new(ret_type)))
    }

    fn check(
        &self,
        expr: &Expression,
        ttype: Type,
        env: &Rc<RefCell<Env>>,
    ) -> Result<(), TypeError> {
        if expr.is_array_lit() {
            return self.check_array(&expr, ttype, env);
        }

        let synth_type = if expr.is_function() && ttype.is_function() {
            self.check_function(expr, ttype.clone(), env)?
        } else {
            self.synth(&expr, env)?
        };

        if !synth_type.is_subtype(ttype.clone()) {
            return Err(TypeError::Expected(ttype, synth_type, expr.get_span()));
        }

        Ok(())
    }

    fn check_function(
        &self,
        expr: &Expression,
        ttype: Type,
        env: &Rc<RefCell<Env>>,
    ) -> Result<Type, TypeError> {
        let fun = match &expr {
            Expression::Function(e) => e,
            _ => unreachable!(),
        };

        let fun_type = match &ttype {
            Type::Function(p, r) => (p, r),
            _ => unreachable!(),
        };

        let params = &fun.params;

        if params.len() != fun_type.0.len() {
            return Err(TypeError::ExpectedArguments(
                params.len(),
                fun_type.0.len(),
                expr.get_span(),
            ));
        }

        let env = Rc::new(RefCell::new(Env::with_parent(Rc::clone(&env))));

        let type_params: Vec<Type> = params
            .into_iter()
            .zip(fun_type.0.into_iter())
            .map(|e| {
                let ident = e.0;
                let ttype = e.1;

                env.borrow_mut().set(&ident.value, ttype.clone());

                match &ident.value_type {
                    Some(v) => Type::from_ast_type(v.clone(), &env),
                    None => ttype.clone(),
                }
            })
            .collect();

        let ret = self.synth(&fun.body, &env)?;
        Ok(Type::Function(type_params, Box::new(ret)))
    }

    fn check_array(
        &self,
        expr: &Expression,
        ttype: Type,
        env: &Rc<RefCell<Env>>,
    ) -> Result<(), TypeError> {
        let arr = match expr {
            Expression::Literal(lit) => match &lit.0 {
                ast::Literal::Array(arr) => arr,
                _ => unreachable!(),
            },
            _ => unreachable!(),
        };

        for e in arr {
            self.check(e, ttype.clone(), env)?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod test {
    use crate::TypeChecker;
    use ariadne::Source;
    use parser::Parser;
    use scanner::Scanner;

    fn parse(src: &str) {
        let scanner = Scanner::new(src.into());
        let mut parser = Parser::new(scanner);

        let (exprs, errors) = parser.parse();

        if !errors.is_empty() {
            panic!("Parser emitted errors {:#?}", errors);
        }
        let mut type_checker = TypeChecker::new();
        let (res, errors) = type_checker.type_check(&exprs, None);

        println!("res: {:#?}", res);

        for mut err in errors.clone() {
            match err.into_report().print(Source::from(src.to_string())) {
                Err(err) => println!("{}", err),
                _ => {}
            }
        }
        // println!("errors: {:#?}", errors);
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
        // let n: fn(x: usize) -> usize = |x: f64| 2;

        // TODO dont throw annnotation needed error in parser
        parse(
            "
            let main: Fun(cb: Fun(id: int)) = fun(cb) {
                cb(1);
            };

            main(fun(id) => { id == 1; });
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
