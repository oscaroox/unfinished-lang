use std::{
    cell::RefCell,
    collections::{hash_map::Entry, HashMap},
    rc::Rc,
};

use crate::env::Env;
use crate::type_error::TypeError;
use ast::{self, Expression, Function, Identifier};
use type_core::{DataStructField, DataStructMethod, FunctionParam, Singleton, Type};

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

    fn resolve_type(&mut self, ttype: Type, env: &Rc<RefCell<Env>>) -> Type {
        match ttype {
            Type::Identifier(identifier) => match env.borrow_mut().get(&identifier) {
                Some(t) => match &t {
                    Type::DataStruct(d) => Type::DataStructInstance(d.clone()),
                    _ => t,
                },
                None => panic!("Undefined type {}", identifier),
            },
            Type::Array(t) => self.resolve_type(*t, env),
            Type::Function(t) => {
                let p: Vec<FunctionParam> = t
                    .params
                    .iter()
                    .map(|i| FunctionParam {
                        name: i.name.to_string(),
                        ttype: self.resolve_type(i.ttype.clone(), env),
                    })
                    .collect();
                let ret = self.resolve_type(*t.return_type, env);
                Type::function(p, ret)
            }
            _ => ttype,
        }
    }

    fn synth(&mut self, expr: &Expression, env: &Rc<RefCell<Env>>) -> Result<Type, TypeError> {
        match expr {
            Expression::Literal(lit) => Ok(match &lit.value {
                ast::LiteralValue::Int(_) => Type::int(),
                ast::LiteralValue::Float(_) => Type::float(),
                ast::LiteralValue::Bool(_) => Type::bool(),
                ast::LiteralValue::String(_) => Type::string(),
                ast::LiteralValue::Tuple(_) => todo!(),
                ast::LiteralValue::Array(arr) => match arr.is_empty() {
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
                ast::LiteralValue::Null => todo!(),
            }),
            Expression::BreakExpr(_) => Ok(Type::Unit),
            Expression::ContinueExpr(_) => Ok(Type::Unit),
            Expression::Let(expr) => {
                if let Some(ttype) = &expr.name.value_type {
                    let ttype = self.resolve_type(ttype.clone(), env);
                    env.borrow_mut().set(&expr.name.value, ttype.clone());
                    if let Some(e) = &expr.value {
                        self.check(e, ttype.clone(), env)?;
                    }
                } else if let Some(e) = &expr.value {
                    env.borrow_mut().set(&expr.name.value, Type::Unit);
                    let ttype = self.synth(e, env)?;

                    env.borrow_mut().set(&expr.name.value, ttype);
                } else {
                    env.borrow_mut().set(&expr.name.value, Type::Unknown);
                }

                Ok(Type::Unit)
            }
            Expression::LetRef(expr) => match env.borrow_mut().get(&expr.name.value) {
                Some(t) => Ok(t),
                None => Err(TypeError::UndefinedVariable(
                    expr.name.value.to_string(),
                    expr.span.clone(),
                )),
            },
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
            Expression::ImplicitReturn(expr) => self.synth(&expr.value, &env),
            Expression::Return(expr) => match &expr.value {
                Some(e) => self.synth(e, env),
                None => Ok(Type::Unit),
            },
            Expression::Function(expr) => {
                let fn_type = self.synth_function_signature(expr, &env)?;
                let env = Rc::new(RefCell::new(Env::with_parent(Rc::clone(&env))));
                // TODO check if function is in method and static
                // TODO check if self parameter
                let ret_type = match &fn_type {
                    Type::Function(fn_expr) => {
                        for (e, param) in expr.params.iter().zip(&fn_expr.params) {
                            env.borrow_mut().set(&e.value, param.ttype.clone());
                        }
                        fn_expr.return_type.clone()
                    }
                    _ => unreachable!(),
                };

                if let Err(e) = self.check(&expr.body, *ret_type.clone(), &env) {
                    self.add_error(e);
                };

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
                    Type::Function(fn_expr) => {
                        // TODO handle self param in methods
                        if fn_expr.params.len() != expr.arguments.len() {
                            return Err(TypeError::ExpectedArguments(
                                fn_expr.params.len(),
                                expr.arguments.len(),
                                expr.span.clone(),
                            ));
                        }

                        // check if this function uses named arguments
                        // the rest of the arguments should also be named
                        let is_named = match expr.arguments.get(0) {
                            Some(e) => e.is_named(),
                            None => false,
                        };

                        for (t, e) in fn_expr.params.iter().zip(expr.arguments.iter()) {
                            if !is_named {
                                self.check(&e.1, t.ttype.clone(), &env)?;
                            }
                        }

                        if !is_named {
                            for (t, e) in fn_expr.params.iter().zip(expr.arguments.iter()) {
                                self.check(&e.1, t.ttype.clone(), &env)?;
                            }
                        } else {
                            let mut provided_args: HashMap<String, (&Identifier, &Expression)> =
                                HashMap::new();

                            for arg in expr.arguments.iter() {
                                let arg_ident = arg.0.as_ref().unwrap();
                                let arg_name = arg_ident.value.to_string();

                                // Check if the named argument was already provided,
                                // if true true a duplicate named argument error
                                if provided_args.contains_key(&arg_name) {
                                    self.add_error(TypeError::DuplicateNamedArgument(
                                        arg_name,
                                        arg_ident.span.clone(),
                                    ));
                                } else if !fn_expr.params.iter().any(|p| p.name == arg_name) {
                                    // check if the argument provided exists in the function parameter list
                                    // throw unknown argument error if it does not exists
                                    self.add_error(TypeError::UnknownNamedArgument(
                                        arg_name,
                                        arg_ident.span.clone(),
                                    ));
                                } else {
                                    provided_args.insert(arg_name, (arg_ident, &arg.1));
                                }
                            }

                            for t in fn_expr.params.iter() {
                                if let Some(v) = provided_args.get(&t.name) {
                                    self.check(v.1, t.ttype.clone(), env)?;
                                }
                            }
                        }
                        Ok(*fn_expr.return_type)
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
                    let ttype = match &field.value_type {
                        Some(t) => self.resolve_type(t.clone(), env),
                        None => panic!("Type Annotation needed"), // TODO: get better spans for identifier
                    };

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
                let ttype = match env.borrow_mut().get(&expr.name.value) {
                    Some(t) => t,
                    None => {
                        return Err(TypeError::UndefinedVariable(
                            expr.name.value.to_string(),
                            expr.span.clone(),
                        ));
                    }
                };

                match &ttype {
                    Type::DataStruct(d) => {
                        for field in &d.fields {
                            let prop = expr.fields.iter().find(|p| p.name.value == field.name);
                            match prop {
                                None => {
                                    return Err(TypeError::MissingProperty(
                                        field.name.to_string(),
                                        expr.span.clone(),
                                    ));
                                }
                                _ => {}
                            }
                        }

                        for field in &expr.fields {
                            let prop = d.fields.iter().find(|p| p.name == field.name.value);
                            match prop {
                                Some(p) => self.check(&field.value, p.ttype.clone(), env)?,
                                None => {
                                    return Err(TypeError::ExtraProperty(
                                        field.name.value.to_string(),
                                        expr.span.clone(),
                                    ));
                                }
                            }
                        }

                        Ok(Type::DataStructInstance(d.clone()))
                    }
                    _ => Err(TypeError::InvalidDataStructInstantiation(expr.span.clone())),
                }
            }
            Expression::SelfExpr(expr) => match env.borrow_mut().get(&expr.name) {
                Some(t) => Ok(t),
                None => Ok(Type::Unit), // TODO merge analyzer in type_checker
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
                                None => {
                                    return Err(TypeError::UnknownMethod(
                                        expr.name.value.to_string(),
                                        expr.span.clone(),
                                    ))
                                }
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
            Expression::Grouping(expr) => self.synth(&expr.expr, env),

            Expression::BinOp(expr) => {
                let (lhs, rhs) = self.synth_left_and_right(&expr.left, &expr.right, env)?;

                match &expr.op {
                    ast::BinaryOperation::Add(span)
                    | ast::BinaryOperation::Subtract(span)
                    | ast::BinaryOperation::Multiply(span)
                    | ast::BinaryOperation::Divide(span) => match (&lhs, &rhs) {
                        (Type::Singleton(Singleton::Int), Type::Singleton(Singleton::Int)) => {
                            Ok(Type::int())
                        }
                        (Type::Singleton(Singleton::Float), Type::Singleton(Singleton::Float)) => {
                            Ok(Type::float())
                        }
                        _ => Err(TypeError::InvalidBinaryOperation(
                            expr.op.to_string(),
                            lhs,
                            rhs,
                            span.clone(),
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
                let (lhs, rhs) = self.synth_left_and_right(&expr.lhs, &expr.rhs, env)?;

                match &expr.op {
                    ast::LogicOperation::Or(_) | ast::LogicOperation::And(_) => {
                        match (&lhs, &rhs) {
                            (
                                Type::Singleton(Singleton::Bool),
                                Type::Singleton(Singleton::Bool),
                            ) => Ok(Type::bool()),
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
                        }
                    }

                    ast::LogicOperation::Equal(_)
                    | ast::LogicOperation::NotEqual(_)
                    | ast::LogicOperation::LessThan(_)
                    | ast::LogicOperation::LessThanEqual(_)
                    | ast::LogicOperation::GreaterThan(_)
                    | ast::LogicOperation::GreaterThanEqual(_) => match (&lhs, &rhs) {
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
                            expr.if_token.clone(),
                        ));
                    }
                }

                Ok(then)
            }
            Expression::LoopExpr(expr) => {
                // TODO fix
                // if let Some(e) = &expr.iterator {
                //     let it = self.synth(e, env)?;
                //     let env = Rc::new(RefCell::new(Env::with_parent(env.clone())));
                //     let iterator_type = match it {
                //         Type::Array(arr) => match *arr {
                //             Type::Unknown => {
                //                 return Err(TypeError::AnnotationNeeded(e.get_span()));
                //             }
                //             _ => *arr,
                //         },
                //         _ => {
                //             return Err(TypeError::CannotIterate(it, e.get_span()));
                //         }
                //     };

                //     let name = match &*expr.condition {
                //         Expression::Let(l) => l.name.value.to_string(),
                //         _ => unreachable!(),
                //     };

                //     // set let expression to unknown value
                //     self.synth(&expr.condition, &env)?;

                //     // update let expression with iterator type
                //     env.borrow_mut().set(&name, iterator_type);

                //     self.synth(&expr.body, &env)?;
                // } else {
                //     let cond = self.synth(&expr.condition, env)?;
                //     if !cond.is_bool() {
                //         return Err(TypeError::Expected(
                //             Type::bool(),
                //             cond,
                //             expr.condition.get_span(),
                //         ));
                //     }
                // }

                Ok(Type::Unit)
            }
            Expression::ForExpr(_) => todo!(),
            Expression::WhileExpr(_) => todo!(),
        }
    }

    /**
     * Gather errors from Binary and Logical Expressions
     */
    fn synth_left_and_right(
        &mut self,
        left: &Expression,
        right: &Expression,
        env: &Rc<RefCell<Env>>,
    ) -> Result<(Type, Type), TypeError> {
        let lhs = self.synth(left, env);
        let rhs = self.synth(right, env);

        match (lhs, rhs) {
            (Ok(r1), Ok(r2)) => Ok((r1, r2)),
            (Err(e1), Err(e2)) => Err(TypeError::LeftAndRight(Box::new(e1), Box::new(e2))),
            (Ok(_), Err(e)) | (Err(e), Ok(_)) => Err(e),
        }
    }

    fn synth_function_signature(
        &mut self,
        expr: &Function,
        env: &Rc<RefCell<Env>>,
    ) -> Result<Type, TypeError> {
        let param_types: Vec<FunctionParam> = expr
            .params
            .iter()
            .map(|e| match &e.value_type {
                Some(t) => FunctionParam {
                    name: e.value.to_string(),
                    ttype: self.resolve_type(t.clone(), env),
                },
                None => panic!("Annotation type needed"),
            })
            .collect();

        Ok(Type::function(
            param_types,
            self.resolve_type(expr.return_type.clone(), env),
        ))
    }

    fn check(
        &mut self,
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
        &mut self,
        expr: &Expression,
        ttype: Type,
        env: &Rc<RefCell<Env>>,
    ) -> Result<Type, TypeError> {
        let fun = match &expr {
            Expression::Function(e) => e,
            _ => unreachable!(),
        };

        let fun_type = match &ttype {
            Type::Function(ttype) => (&ttype.params, &ttype.return_type),
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

        let type_params: Vec<FunctionParam> = params
            .into_iter()
            .zip(fun_type.0.into_iter())
            .map(|e| {
                let ident = e.0;
                let param = e.1;

                env.borrow_mut().set(&ident.value, param.ttype.clone());

                match &ident.value_type {
                    Some(v) => FunctionParam {
                        name: ident.value.to_string(),
                        ttype: self.resolve_type(v.clone(), &env),
                    },
                    None => param.clone(),
                }
            })
            .collect();

        let ret = self.synth(&fun.body, &env)?;
        Ok(Type::function(type_params, ret))
    }

    fn check_array(
        &mut self,
        expr: &Expression,
        ttype: Type,
        env: &Rc<RefCell<Env>>,
    ) -> Result<(), TypeError> {
        let arr = match expr {
            Expression::Literal(lit) => match &lit.value {
                ast::LiteralValue::Array(arr) => arr,
                _ => unreachable!(),
            },
            _ => unreachable!(),
        };

        for e in arr {
            if let Err(e) = self.check(e, ttype.clone(), env) {
                self.add_error(e.clone())
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod test {
    use crate::{type_error::TypeError, TypeChecker};
    use span_util::Span;
    use type_core::{FunctionParam, Type};

    fn check(src: &str) -> (Vec<Type>, Vec<TypeError>) {
        let exprs = parser::parse_panic(src);

        let mut type_checker = TypeChecker::new();
        return type_checker.type_check(&exprs, None);
    }

    fn run(src: &str, expected_types: Vec<Type>) {
        let (types, errors) = check(src);
        println!("{types:#?}");
        if errors.len() > 0 {
            panic!("typechecker emitted errors: {:#?}", errors);
        }

        // ignore checking for types if array is empty
        if expected_types.is_empty() {
            return;
        }

        if types.len() != expected_types.len() {
            panic!(
                "typechecker emitted {} types, expected types has {} types",
                types.len(),
                expected_types.len()
            );
        }

        for (ttype, expected) in types.iter().zip(expected_types) {
            assert_eq!(*ttype, expected);
        }
    }

    fn run_error(src: &str, expected_errors: Vec<TypeError>) {
        let (_, errors) = check(src);

        if errors.is_empty() {
            panic!("analyzer did not emit errors");
        };

        if errors.len() != expected_errors.len() {
            println!("{errors:#?}");
            panic!(
                "analyzer emitted {} errors, expected errors has {} errors",
                errors.len(),
                expected_errors.len()
            );
        }
        for (err, expected) in errors.iter().zip(expected_errors) {
            assert_eq!(*err, expected);
        }
    }

    #[test]
    fn test_synth() {
        run(
            r#"1; 2.2; "test"; true; false;"#,
            vec![
                Type::int(),
                Type::float(),
                Type::string(),
                Type::bool(),
                Type::bool(),
            ],
        );
    }

    #[test]
    fn test_array_synth() {
        run(r#"[1];"#, vec![Type::array(Type::int())]);
    }

    #[test]
    fn test_let_synth() {
        run(
            r#"let x: string; x = "test"; let x = 1; x = 2;"#,
            vec![Type::unit(), Type::string(), Type::unit(), Type::int()],
        );
    }

    #[test]
    fn test_block_synth() {
        run(
            r#"
            let x = 1;
            { 
                let f = 2;
                x = 2;
                f = 1;
            };
            x = 1;
        "#,
            vec![Type::unit(), Type::unit(), Type::int()],
        );
    }

    #[test]
    fn test_function_synth() {
        run(
            "
            let main = fn(x: int): int => x;
            main;
        ",
            vec![
                Type::unit(),
                Type::function(
                    vec![FunctionParam {
                        name: "x".into(),
                        ttype: Type::int(),
                    }],
                    Type::int(),
                ),
            ],
        );
    }

    #[test]
    fn test_function_call_synth() {
        run(
            r#"
            let main = fn(): int => 1;
            main();
            let main = fn(x: int, y: int): int => x;
            let y = 2;
            main(1, y);
        "#,
            vec![
                Type::unit(),
                Type::int(),
                Type::unit(),
                Type::unit(),
                Type::int(),
            ],
        );
    }

    #[test]
    fn test_function_call_named_arguments() {
        run(
            r#"
            let main = fn(a: string, b: int) {};
            main(b=1, a="test");
            main(a="test", b=2);
        "#,
            vec![Type::unit(), Type::unit(), Type::unit()],
        );

        run_error(
            r#"
            let main = fn(a: string, b: int) {};
            main(b=1, b = 1);
            main(a="testing", c="testing");
            "#,
            vec![
                TypeError::DuplicateNamedArgument("b".into(), Span::fake()),
                TypeError::UnknownNamedArgument("c".into(), Span::fake()),
            ],
        );
    }

    #[test]
    fn test_function_callback_synth() {
        // TODO don't throw annotation needed error in parser
        run(
            "
            let main: fn(cb: fn(id: int)) = fn(cb) {
                cb(1);
            };

            main(fn(id) => { id == 1; });
        ",
            vec![Type::unit(), Type::unit()],
        );
    }

    #[test]
    fn test_index_synth() {
        run(
            "
            let x = [1,2,3];

            let y = x[0];
            x;y;
        ",
            vec![
                Type::unit(),
                Type::unit(),
                Type::array(Type::int()),
                Type::int(),
            ],
        );

        run(
            r#"
            let x = ["test", "test2", "test3"];
            
            x[0] = "test";

            x;
        "#,
            vec![Type::unit(), Type::string(), Type::array(Type::string())],
        );
    }

    #[test]
    fn test_data_struct_synth() {
        run(
            r#"
            data Person(
                id: int,
                name: string,
                is_valid: bool,
            );

            // let Person = Person(id = 1, name = "test", is_valid = true);
            // Person;
            let p = Person(id = 1, name = "test", is_valid = true);
            p;
            let x = Person.is_valid;
            x;
            
            let main = fn(p: Person): bool {
                p.is_valid
            };
            main(p);
        "#,
            vec![],
        );
    }
    #[test]
    fn test_data_struct_set_synth() {
        run(
            r#"
            data Person {
                id: int,
                name: string,
                is_valid: bool,
            };

            let gen_id = fn(): string {
                233
            };
            let person = Person {id: 1, name: "test", is_valid: true};
            person.id = gen_id();
            "#,
            vec![],
        );
    }

    #[test]
    fn test_data_struct_methods_synth() {
        run(
            r#"
            data Person { id: int } :: {
                fn new: Person {
                    let p = Person { id: 1 };
                    p
                }

                fn get_int(self) {
        
                }
            };
            
            let p = Person.new();
            p;
            let main = fn(t: Person): int {
                t.get_int(t);
                let nn = fn(p: Person): int {
                    p.get_int(p);
                    p.id
                };
                let p2 = nn(t);
                p2
            };

            main(p);
          
        "#,
            vec![],
        );
    }

    #[test]
    fn test_grouping_expr_synth() {
        run(
            r#"
            (123);
            (2.2);
            ("test");
            (true);
        "#,
            vec![Type::int(), Type::float(), Type::string(), Type::bool()],
        );
    }

    #[test]
    fn test_binop_synth() {
        run_error(
            "
            let x = 1;
            let y = 2.0;
            x / y;
        ",
            vec![TypeError::InvalidBinaryOperation(
                "/".into(),
                Type::int(),
                Type::float(),
                Span::fake(),
            )],
        );
    }

    #[test]
    fn test_unary_synth() {
        run(
            "
            -1;
            -2.0;
            !false;
            true;
        ",
            vec![Type::int(), Type::float(), Type::bool(), Type::bool()],
        );
    }

    #[test]
    fn test_logic_synth() {
        run(
            r#"
        true && true;
        true || true;

        1 == 2 && 2 == 1;
        1 >= 2;
        2 <= 1;
        "test" == "test";
        "#,
            vec![
                Type::bool(),
                Type::bool(),
                Type::bool(),
                Type::bool(),
                Type::bool(),
                Type::bool(),
            ],
        );
    }

    #[test]
    fn test_if_synth() {
        run(
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
            vec![Type::unit(), Type::int(), Type::unit(), Type::float()],
        );
    }

    #[test]
    fn test_loop_synth() {
        run(
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
            vec![],
        );
    }
}
