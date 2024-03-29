use std::{cell::RefCell, collections::HashMap, convert::TryInto, fmt::Debug, rc::Rc};

use crate::{builtin::get_builtins, environment::Environment, Value};
use ast::{
    self, Assign, BinOp, BinaryOperation, Block, Call, DataStruct, Expression, Function, GetIndex,
    GetProperty, IfConditional, ImplicitReturn, LetExpr, Literal, LiteralValue, Logic,
    LogicOperation, LoopExpr, Program, ReturnExpr, SelfExpr, SetIndex, SetProperty, UnaryOp,
    UnaryOperation,
};

#[derive(Debug)]
pub enum RuntimeError {}

#[derive(Debug)]
pub struct Interpreter {
    env: Rc<RefCell<Environment>>,
}

type InterpreterResult = Result<Value, RuntimeError>;

// macro used for swapping the parent environment with the new supplied environment
// this also makes sure this code isn't duplicated when evaluating if, block and function calls
macro_rules! eval_with_new_env_in_scope {
    ($self:ident, $env:expr, $fun:ident, $exprs:expr) => {{
        let parent_env = $self.env.clone();
        $self.env = $env;
        let res = $self.$fun($exprs);
        $self.env = parent_env;
        res
    }};
}

macro_rules! eval_loop_body {
    ($self:ident, $env:expr, $out:expr, $fun:ident, $exprs:expr) => {{
        let val = eval_with_new_env_in_scope!($self, $env.clone(), $fun, $exprs)?;
        match val {
            Value::Break | Value::ReturnVal(_) => {
                if let Value::ReturnVal(_) = val {
                    $out = val;
                }
                break;
            }
            Value::Continue => {
                continue;
            }
            _ => {}
        }
    }};
}

impl Interpreter {
    pub fn new() -> Interpreter {
        let globals = Rc::new(RefCell::new(Environment::new()));
        let builtins = get_builtins();
        globals.borrow_mut().extend(builtins);

        Interpreter {
            env: Rc::new(RefCell::new(Environment::with_outer(globals.clone()))),
        }
    }

    pub fn with_env(env: Rc<RefCell<Environment>>) -> Interpreter {
        let builtins = get_builtins();
        env.borrow_mut().extend(builtins);
        Interpreter { env }
    }

    pub fn run(&mut self, exprs: Program) -> InterpreterResult {
        self.eval(&exprs)
    }

    fn eval(&mut self, exprs: &Program) -> InterpreterResult {
        let mut res = Value::Unit;
        for expr in exprs {
            res = self.expression(expr)?;
        }
        Ok(res)
    }

    fn eval_function_expressions(&mut self, expression: &Expression) -> InterpreterResult {
        let block = match expression {
            Expression::Block(block) => block,
            _ => unreachable!(),
        };

        let res = Value::Unit;
        for expr in &block.exprs {
            match self.expression(expr)? {
                Value::ReturnVal(v) | Value::ImplicitReturnVal(v) => return Ok(*v),
                _ => {}
            }
        }
        Ok(res)
    }

    fn eval_expressions(&mut self, exprs: &Program) -> InterpreterResult {
        let res = Value::Unit;
        for expr in exprs.iter() {
            let value = self.expression(expr)?;
            match value {
                Value::ImplicitReturnVal(v) => return Ok(*v),
                Value::ReturnVal(_) | Value::Break | Value::Continue => return Ok(value),
                _ => {}
            }
        }
        Ok(res)
    }

    fn expression(&mut self, expression: &Expression) -> InterpreterResult {
        match expression {
            Expression::Let(expr) => self.eval_let_expression(&expr),
            Expression::BinOp(expr) => self.eval_binop(&expr),
            Expression::Literal(expr) => self.eval_literal(&expr),
            Expression::Assign(expr) => self.eval_assignment(&expr),
            Expression::LetRef(expr) => self.eval_let_reference(&expr),
            Expression::UnaryOp(expr) => self.eval_unaryop(&expr),
            Expression::Grouping(expr) => self.expression(&expr.expr),
            Expression::Logic(expr) => self.eval_logic_expression(&expr),
            Expression::Call(expr) => self.eval_call(&expr),
            Expression::Function(expr) => self.eval_function(&expr),
            Expression::Block(expr) => self.eval_block(&expr),
            Expression::If(expr) => self.eval_if_conditional(&expr),
            Expression::GetIndex(expr) => self.eval_index(&expr),
            Expression::SetIndex(expr) => self.eval_set_index(&expr),
            Expression::Return(expr) => self.eval_return(&expr),
            Expression::ImplicitReturn(expr) => self.eval_implicit_return(&expr),
            Expression::DataStruct(expr) => self.eval_data_struct(&expr),
            Expression::DataStructInstance(expr) => self.eval_data_struct_instance(&expr),
            Expression::GetProperty(expr) => self.eval_get_property(&expr),
            Expression::SetProperty(expr) => self.eval_set_property(&expr),
            Expression::SelfExpr(expr) => self.eval_self_expr(&expr),
            Expression::LoopExpr(expr) => self.eval_loop_expr(&expr),
            Expression::BreakExpr(_) => Ok(Value::Break),
            Expression::ContinueExpr(_) => Ok(Value::Continue),
            Expression::WhileExpr(_) => todo!(),
            Expression::ForExpr(_) => todo!(),
        }
    }

    fn eval_let_expression(&mut self, expr: &LetExpr) -> InterpreterResult {
        let name = expr.name.value.to_string();
        let val = match &expr.value {
            Some(expr) => self.expression(expr)?,
            None => Value::Unit,
        };

        self.env.borrow_mut().define(name, val.clone());
        Ok(val)
    }

    fn eval_loop_expr(&mut self, loop_expr: &LoopExpr) -> InterpreterResult {
        // TODO fix
        // let env = Rc::new(RefCell::new(Environment::with_outer(Rc::clone(&self.env))));
        // let mut out = Value::Unit;
        // if let Some(iter) = &loop_expr.iterator {
        //     let iter = self.expression(iter)?;

        //     // condition is a let expr
        //     self.expression(&loop_expr.condition)?;
        //     let let_expr = loop_expr.condition.to_let();

        //     let values = match iter {
        //         Value::Array(arr) => arr.borrow().values.clone(),
        //         Value::Range(start, end) => (start..end).map(|i| Value::Int(i)).collect(),
        //         _ => panic!("Can only iterate over arrays"),
        //     };

        //     for x in values {
        //         env.borrow_mut()
        //             .assign(let_expr.name.value.to_string(), x.clone());
        //         eval_loop_body!(self, env.clone(), out, expression, &*loop_expr.body);
        //     }
        // } else {
        //     while (self.expression(&loop_expr.condition)?).is_truthy() {
        //         eval_loop_body!(self, env.clone(), out, expression, &*loop_expr.body);
        //     }
        // }

        // Ok(out)
        todo!()
    }

    fn eval_self_expr(&mut self, self_expr: &SelfExpr) -> InterpreterResult {
        match self.env.borrow().get(&self_expr.name) {
            Some(v) => Ok(v),
            None => panic!("Unknown variable 'self'"),
        }
    }

    fn eval_set_property(&mut self, set_property: &SetProperty) -> InterpreterResult {
        let obj = self.expression(&set_property.object)?;

        match obj {
            Value::DataClassInstance(instance) => {
                let value = self.expression(&set_property.value)?;

                instance
                    .borrow_mut()
                    .set(set_property.name.value.to_string(), value.clone());

                Ok(value.clone())
            }
            _ => panic!("Can only set properties on data instances got {}", obj),
        }
    }

    fn eval_get_property(&mut self, get_property: &GetProperty) -> InterpreterResult {
        let obj = self.expression(&get_property.object)?;

        match obj {
            Value::DataClassInstance(instance) => {
                let mut val = if get_property.is_callable {
                    instance
                        .borrow()
                        .get_method(get_property.name.value.to_string())
                } else {
                    instance.borrow().get(get_property.name.value.to_string())
                };

                // if property is a function, bind self keyword to function closure
                if let Value::Function(ref mut fun) = val {
                    fun.closure.borrow_mut().define(
                        "self".to_string(),
                        Value::DataClassInstance(instance.clone()),
                    );

                    // decrement function arity by 1 since self is supplied by the interpreter
                    if fun.arity > 0 {
                        fun.arity = fun.arity - 1;
                    }
                }

                Ok(val.clone())
            }
            Value::DataClass(d) => {
                let fun = d.get(get_property.name.value.to_string());
                Ok(Value::Function(fun))
            }
            Value::Null => panic!("Cannot access {} on null", get_property.name),
            _ => panic!("Property not found on {}", obj),
        }
    }

    fn eval_data_struct_instance(
        &mut self,
        data_struct_instance: &ast::DataStructInstance,
    ) -> InterpreterResult {
        let data_struct_identifier = data_struct_instance.name.clone();
        let data_struct = match self.env.borrow().get(&data_struct_identifier.value) {
            Some(val) => match val {
                Value::DataClass(class) => class.clone(),
                _ => panic!("Cannot only instantiate data struct got {}", val),
            },
            None => panic!("Unknown data struct {}", data_struct_identifier),
        };

        let mut eval_fields = HashMap::new();

        //  check if the instance fields are actually in the Data struct e.g
        //  data Person { name };
        //  Person { name: "test", age: 2 } // Error unknown field age
        for field in &data_struct_instance.fields {
            let matches = data_struct
                .fields
                .iter()
                .any(|e| e.value == field.name.value);
            if matches {
                let val = self.expression(&field.value)?;
                eval_fields.insert(field.name.value.to_string(), val);
            } else {
                panic!(
                    "Unknown field '{}' provided to data struct '{}'",
                    field.name, data_struct_identifier
                )
            }
        }

        // TODO require all fields to be filled in
        // fill in the missing fields with null value
        for field in &data_struct.fields {
            if !eval_fields.contains_key(&field.value) {
                eval_fields.insert(field.value.to_string(), Value::Null);
            }
        }

        Ok(Value::data_struct_instance(
            data_struct_identifier,
            data_struct.clone(),
            eval_fields,
            data_struct.fields.clone(),
        ))
    }

    fn eval_data_struct(&mut self, data_struct: &DataStruct) -> InterpreterResult {
        let mut static_methods = HashMap::new();
        let mut instance_methods = HashMap::new();

        for method in &data_struct.methods {
            match method {
                Expression::Function(fun) => match self.expression(method)? {
                    Value::Function(value_fun) => {
                        if fun.is_static {
                            static_methods
                                .insert(fun.name.as_ref().unwrap().to_string(), value_fun);
                        } else {
                            instance_methods
                                .insert(fun.name.as_ref().unwrap().to_string(), value_fun);
                        }
                    }
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            }
        }

        let value = Value::data_struct(
            data_struct.name.to_owned(),
            data_struct.fields.clone(),
            static_methods,
            instance_methods,
        );

        self.env
            .borrow_mut()
            .define(data_struct.name.value.to_string(), value.clone());

        Ok(value)
    }

    fn eval_return(&mut self, ret: &ReturnExpr) -> InterpreterResult {
        match &ret.value {
            Some(op) => Ok(Value::return_val(self.expression(op)?)),
            None => Ok(Value::return_val(Value::Unit)),
        }
    }
    fn eval_implicit_return(&mut self, ret: &ImplicitReturn) -> InterpreterResult {
        Ok(Value::implicit_return_val(self.expression(&ret.value)?))
    }

    fn eval_set_index(&mut self, set_index: &SetIndex) -> InterpreterResult {
        let lhs = self.expression(&set_index.lhs)?;
        let index = self.expression(&set_index.index)?;
        let value = self.expression(&set_index.value)?;

        match (&lhs, &index, &value) {
            (Value::Array(arr), Value::Int(i), _) => {
                let idx = *i as usize;

                if arr.borrow().values.len() < idx {
                    panic!(
                        "Index out of bound array len: {} index was: {}",
                        arr.borrow().values.len(),
                        idx
                    );
                }
                arr.borrow_mut().values[idx] = value.clone();
                Ok(value)
            }
            _ => panic!("Cannot set index into a value of {}", lhs.to_type()),
        }
    }

    fn eval_index(&mut self, index: &GetIndex) -> InterpreterResult {
        let lhs = self.expression(&index.lhs)?;
        let idx = self.expression(&index.index)?;

        match (&lhs, &idx) {
            (Value::Array(arr), Value::Int(i)) => match arr.borrow().values.get(*i as usize) {
                Some(val) => Ok(val.clone()),
                None => Ok(Value::Null),
            },
            _ => panic!("Cannot index into a value of {}", lhs.to_type()),
        }
    }

    fn eval_call(&mut self, call: &Call) -> InterpreterResult {
        let callee = self.expression(&call.callee)?;
        let mut args: Vec<Value> = vec![];
        unreachable!("TODO FIX NAMED ARGUMENTS@!!!");
        for arg in &call.arguments {

            // let val = self.expression(arg)?;
            // args.push(val);
        }

        match callee {
            Value::Function(fun) => {
                let mut env = Environment::with_outer(fun.closure.clone());
                let mut params = vec![];

                // TODO mark data struct functions as methods
                if fun.params.len() > 0 {
                    if fun.params[0].is_self() {
                        params = fun.params[1..].to_vec();
                    } else {
                        params = fun.params;
                    }
                }

                if args.len() != fun.arity.into() {
                    panic!("Expected {} arguments got {}", fun.arity, args.len())
                }

                for (arg, param) in args.iter().zip(params.iter()) {
                    if !param.is_self() {
                        env.define(param.value.to_string(), arg.clone());
                    }
                }

                let env = Rc::new(RefCell::new(env));

                eval_with_new_env_in_scope!(self, env, eval_function_expressions, &fun.body)
            }
            Value::NativeFunction(v) => {
                if args.len() != v.arity.into() {
                    panic!("Expected {} arguments got {}", v.arity, args.len())
                }

                v.builtin.call(args)
            }
            _ => panic!("You can only call functions and methods"),
        }
    }

    fn eval_function(&mut self, function: &Function) -> InterpreterResult {
        let fun = Value::function(
            function.name.clone(),
            function.params.clone(),
            function.params.len().try_into().unwrap(),
            *function.body.clone(),
            self.env.clone(),
        );

        Ok(fun)
    }

    fn eval_logic_expression(&mut self, logic: &Logic) -> InterpreterResult {
        let left = self.expression(&logic.lhs)?;
        let right = self.expression(&logic.rhs)?;
        let res = match (&left, &logic.op, &right) {
            // integer
            (Value::Int(n1), LogicOperation::Equal(_), Value::Int(n2)) => Value::Bool(n1 == n2),
            (Value::Int(n1), LogicOperation::NotEqual(_), Value::Int(n2)) => Value::Bool(n1 != n2),
            (Value::Int(n1), LogicOperation::LessThan(_), Value::Int(n2)) => Value::Bool(n1 < n2),
            (Value::Int(n1), LogicOperation::LessThanEqual(_), Value::Int(n2)) => {
                Value::Bool(n1 <= n2)
            }
            (Value::Int(n1), LogicOperation::GreaterThan(_), Value::Int(n2)) => {
                Value::Bool(n1 > n2)
            }
            (Value::Int(n1), LogicOperation::GreaterThanEqual(_), Value::Int(n2)) => {
                Value::Bool(n1 >= n2)
            }

            // float
            (Value::Float(n1), LogicOperation::Equal(_), Value::Float(n2)) => Value::Bool(n1 == n2),
            (Value::Float(n1), LogicOperation::NotEqual(_), Value::Float(n2)) => {
                Value::Bool(n1 != n2)
            }
            (Value::Float(n1), LogicOperation::LessThan(_), Value::Float(n2)) => {
                Value::Bool(n1 < n2)
            }
            (Value::Float(n1), LogicOperation::LessThanEqual(_), Value::Float(n2)) => {
                Value::Bool(n1 <= n2)
            }
            (Value::Float(n1), LogicOperation::GreaterThan(_), Value::Float(n2)) => {
                Value::Bool(n1 > n2)
            }
            (Value::Float(n1), LogicOperation::GreaterThanEqual(_), Value::Float(n2)) => {
                Value::Bool(n1 >= n2)
            }

            // bool
            (Value::Bool(b1), LogicOperation::And(_), Value::Bool(b2)) => Value::Bool(*b1 && *b2),
            (Value::Bool(b1), LogicOperation::Or(_), Value::Bool(b2)) => Value::Bool(*b1 || *b2),

            // string
            (Value::String(s1), LogicOperation::Equal(_), Value::String(s2)) => {
                Value::Bool(s1 == s2)
            }
            (Value::String(s1), LogicOperation::NotEqual(_), Value::String(s2)) => {
                Value::Bool(s1 != s2)
            }

            (Value::Null, LogicOperation::NotEqual(_), v) => Value::Bool(left != *v),
            (v, LogicOperation::NotEqual(_), Value::Null) => Value::Bool(*v != right),

            (Value::Null, LogicOperation::Equal(_), v) => Value::Bool(left == *v),
            (v, LogicOperation::Equal(_), Value::Null) => Value::Bool(*v == right),
            _ => panic!(
                "Invalid logic operation {} on {} and {}",
                logic.op, left, right
            ),
        };

        Ok(res)
    }

    fn eval_block(&mut self, block: &Block) -> InterpreterResult {
        let env = Rc::new(RefCell::new(Environment::with_outer(self.env.clone())));
        eval_with_new_env_in_scope!(self, env, eval_expressions, &block.exprs)
    }

    fn eval_if_conditional(&mut self, if_condition: &IfConditional) -> InterpreterResult {
        let cond = self.expression(&if_condition.condition)?;
        let env = Rc::new(RefCell::new(Environment::with_outer(self.env.clone())));
        let val = Value::Null;

        if cond.is_truthy() {
            return eval_with_new_env_in_scope!(self, env, expression, &*if_condition.then);
        } else if let Some(el) = &if_condition.not_then {
            return eval_with_new_env_in_scope!(self, env, expression, &*el);
        }

        Ok(val)
    }

    fn eval_let_reference(&mut self, let_ref: &ast::LetRef) -> InterpreterResult {
        let ident = let_ref.name.value.to_string();
        match self.env.borrow().get(&ident) {
            Some(var) => Ok(var.clone()),
            None => panic!("undefined variable {}", ident),
        }
    }

    fn eval_literal(&mut self, lit: &Literal) -> InterpreterResult {
        Ok(match &lit.value {
            LiteralValue::Int(v) => Value::Int(*v),
            LiteralValue::Float(v) => Value::Float(*v),
            LiteralValue::Bool(v) => Value::Bool(*v),
            LiteralValue::Tuple(_) => todo!(),
            LiteralValue::String(v) => Value::String(v.to_string()),
            LiteralValue::Array(v) => {
                let mut res = vec![];
                for e in v {
                    res.push(self.expression(e)?);
                }
                Value::array(res)
            }
            LiteralValue::Null => Value::Null,
        })
    }

    fn eval_assignment(&mut self, assign: &Assign) -> InterpreterResult {
        let name = assign.name.value.clone();
        let val = self.expression(&assign.rhs)?;
        match self.env.borrow_mut().assign(name.to_string(), val) {
            Some(val) => Ok(val),
            None => panic!("Assignment to unknown variable: {}", name),
        }
    }

    fn eval_unaryop(&mut self, unaryop: &UnaryOp) -> InterpreterResult {
        let left = &unaryop.op;
        let right = self.expression(&unaryop.rhs)?;

        let res = match (left, right) {
            (UnaryOperation::Minus(_), Value::Int(n1)) => Value::Int(-n1),
            (UnaryOperation::Plus(_), Value::Int(n1)) => Value::Int(n1),
            (UnaryOperation::Minus(_), Value::Float(n1)) => Value::Float(-n1),
            (UnaryOperation::Plus(_), Value::Float(n1)) => Value::Float(n1),
            (UnaryOperation::Not(_), Value::Bool(b1)) => Value::Bool(!b1),
            _ => panic!("invalid unary operation"),
        };

        Ok(res)
    }

    fn eval_binop(&mut self, binop: &BinOp) -> InterpreterResult {
        let left = self.expression(&binop.left)?;
        let right = self.expression(&binop.right)?;

        let res = match (&left, &binop.op, &right) {
            // integer binop
            (Value::Int(n1), BinaryOperation::Add(_), Value::Int(n2)) => Value::Int(n1 + n2),
            (Value::Int(n1), BinaryOperation::Subtract(_), Value::Int(n2)) => Value::Int(n1 - n2),
            (Value::Int(n1), BinaryOperation::Multiply(_), Value::Int(n2)) => Value::Int(n1 * n2),
            (Value::Int(n1), BinaryOperation::Divide(_), Value::Int(n2)) => Value::Int(n1 / n2),

            // float binop
            (Value::Float(n1), BinaryOperation::Add(_), Value::Float(n2)) => Value::Float(n1 + n2),
            (Value::Float(n1), BinaryOperation::Subtract(_), Value::Float(n2)) => {
                Value::Float(n1 - n2)
            }
            (Value::Float(n1), BinaryOperation::Multiply(_), Value::Float(n2)) => {
                Value::Float(n1 * n2)
            }
            (Value::Float(n1), BinaryOperation::Divide(_), Value::Float(n2)) => {
                Value::Float(n1 / n2)
            }
            (Value::String(s1), BinaryOperation::ConcatInterpolation, Value::String(s2)) => {
                Value::String(format!("{}{}", s1, s2))
            }
            (_, BinaryOperation::ConcatInterpolation, _) => {
                Value::String(format!("{}{}", left, right))
            }
            (v1, _, v2) => panic!("Invalid binary operation on {} and {}", v1, v2),
        };

        Ok(res)
    }
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;

    use crate::{DataStruct, FunctionValue, Interpreter, Value};
    use ast::Identifier;

    use span_util::Span;
    use type_core::Type;

    pub fn run(src: &str, expected: Value) {
        let exprs = parser::parse_panic(src);

        // TODO run typechecker before interpreter
        let mut interpreter = Interpreter::new();
        match interpreter.run(exprs) {
            Ok(val) => assert_eq!(val, expected),
            Err(err) => println!("{:#?}", err),
        };
    }

    fn ident(name: &str) -> Identifier {
        Identifier::new(name.to_string(), Span::fake())
    }

    fn ident_string(name: &str) -> Identifier {
        Identifier::with_value_type(name.into(), Some(Type::string()), Span::fake())
    }

    fn ident_int(name: &str) -> Identifier {
        Identifier::with_value_type(name.into(), Some(Type::int()), Span::fake())
    }

    fn data_struct(
        name: &str,
        fields: Vec<Identifier>,
        static_methods: HashMap<String, FunctionValue>,
        instance_methods: HashMap<String, FunctionValue>,
    ) -> DataStruct {
        DataStruct {
            name: Identifier::new(name.to_string(), Span::fake()),
            fields,
            static_methods,
            instance_methods,
        }
    }

    #[test]
    fn eval_implicit_returns() {
        run(
            r#"
        let speed = "quick";
        "The $(speed) $({
            let animal = "fox";
            let color = "brown";
            "$(color) $(animal)"
        }) jumps over the lazy dog";
        "#,
            Value::String("The quick brown fox jumps over the lazy dog".to_string()),
        );

        run(
            r#"
            let main = fn(val: bool){
                if val {
                    1
                } else {
                    92
                }
            };
            [main(true), main(false)];
            "#,
            Value::array(vec![Value::Int(1), Value::Int(92)]),
        );

        run(
            r#"
            let main = fn(val: bool) {
                if val {
                    return 99;
                };
                234;
            };
            [main(true), main(false)];
            "#,
            Value::array(vec![Value::Int(99), Value::Unit]),
        );

        run(
            r#"
            let main = fn(val: bool){
                if val {
                    99;
                };
                234;
            };
            [main(true), main(false)];
            "#,
            Value::array(vec![Value::Unit, Value::Unit]),
        );
    }

    #[test]
    fn eval_string_interpolation() {
        run(
            "
            let name = \"John\";
            \"Hello $(name), how was your day?\";
            ",
            Value::String("Hello John, how was your day?".to_string()),
        );

        run(
            r#"
            let john = "John";
            let jane = "Jane";
            
            "Hello $(if true { jane } else { john }), how was your day?";
            "#,
            Value::String("Hello Jane, how was your day?".to_string()),
        );

        run(
            r#"

            "Hello $({
                let first_name = "John";
                let last_name = "Doe";

                "$(first_name) $(last_name)"
            }), how was your day?";
            "#,
            Value::String("Hello John Doe, how was your day?".to_string()),
        );
    }

    #[test]
    fn eval_loop() {
        run(
            "
        let i = 0;
        loop i < 10 {
            i += 1;
        };
        i;
        ",
            Value::Int(10),
        );

        run(
            "
        let i = 0;
        loop i < 10 {
            if i == 5 {
                break;
            };
            i += 1;
        };
        i;
        ",
            Value::Int(5),
        );

        run(
            "
        let names = [2,5,7,8];
        let x = 0;
        loop name in names {
            if name == 7 {
                continue;
            };
            x += 1;
        };
        x;
        ",
            Value::Int(3),
        );

        run(
            "
        let x = 0;
        loop name in range(1, 10) {
            x += 1;
        };
        x;
        ",
            Value::Int(9),
        );

        run(
            "
        let x = 0;
        loop name in range(0, 10) {
            x += 1;
        };
        x;
        ",
            Value::Int(10),
        );
    }

    #[test]
    fn eval_let_expression() {
        run("let name = 123;", Value::Int(123));
        run("let name: string; name;", Value::Unit);
        run("let name = 123; name;", Value::Int(123));
    }

    #[test]
    fn eval_literals() {
        run("let x = true; x;", Value::Bool(true));
        run("let x = false; x;", Value::Bool(false));
        run("let x = 123; x;", Value::Int(123));
        run("let x = 12.45; x;", Value::Float(12.45));
        run("let x = null; x;", Value::Null);
        run(
            r#"let x = "test string"; x;"#,
            Value::String("test string".to_string()),
        );

        run(
            r#"let x = ["test", 1, 1.5, true, false, null]; x;"#,
            Value::array(vec![
                Value::String("test".to_string()),
                Value::Int(1),
                Value::Float(1.5),
                Value::Bool(true),
                Value::Bool(false),
                Value::Null,
            ]),
        );
    }

    #[test]
    pub fn eval_binop() {
        run("let x = 1 + 2 * 4 / 2 - 2; x;", Value::Int(3));
        run("let x = 1 + 2 * 4; x;", Value::Int(9));
        run("let x = 6 * 4 * 2 * 6; x;", Value::Int(288));

        run("let x = 2.5 * 4.5 / 0.2; x;", Value::Float(56.25));
        run("let x = 298.234 + 99.12 - 22.0; x;", Value::Float(375.354));
        run("-2 + -4;", Value::Int(-6));
        run("-2.0 + -4.0;", Value::Float(-6.0));
    }

    #[test]
    pub fn eval_grouping() {
        run("let x = (1 + 2) * 4; x;", Value::Int(12));
        run("let x = 3 * 3 / (2 + 4); x;", Value::Int(1));
        run("let x = (1 + 2) * 4; x;", Value::Int(12));
    }

    #[test]
    pub fn eval_unary() {
        run("-12;", Value::Int(-12));
        run("+12;", Value::Int(12));
        run("-12.23;", Value::Float(-12.23));
        run("+12.23;", Value::Float(12.23));
        run("!true;", Value::Bool(false));
        run("!false;", Value::Bool(true));
    }

    #[test]
    pub fn eval_assignment() {
        run("let x: int; x = 1; x;", Value::Int(1));
        run("let x = 2; x += 1; x;", Value::Int(3));
        run("let x = 2; x *= 3; x;", Value::Int(6));
        run("let x = 2; x /= 2; x;", Value::Int(1));
        run("let x = 2; x -= 1; x;", Value::Int(1));
    }

    #[test]
    pub fn eval_logic_expression() {
        run("1 == 1;", Value::Bool(true));
        run("1 != 1;", Value::Bool(false));
        run("1 == 2;", Value::Bool(false));
        run(r#" "test" == "test";"#, Value::Bool(true));
        run(r#" "test" != "test";"#, Value::Bool(false));
        run(r#" "test1" == "test";"#, Value::Bool(false));
        run("true && false;", Value::Bool(false));
        run("true && true;", Value::Bool(true));
        run("false && false;", Value::Bool(false));
        run("true || false;", Value::Bool(true));
        run("false || true;", Value::Bool(true));
        run("false || false;", Value::Bool(false));
        run("null == null;", Value::Bool(true));
        run("null != null;", Value::Bool(false));
        run("null == true;", Value::Bool(false));
        run("true == null;", Value::Bool(false));
        run("1 != null;", Value::Bool(true));
        run(r#"null != "test";"#, Value::Bool(true));
    }

    #[test]
    pub fn eval_if_conditional() {
        run("if true { 1 };", Value::Int(1));
        run("if false { 1 } else { 2 };", Value::Int(2));
        run("let x = if false { 1 } else { 2 }; x;", Value::Int(2));
        run(
            "
        let x = 1;
        if false { x = 22 } else { x = 99 };
        x;
        ",
            Value::Int(99),
        );
        run(
            "
        let x = 1;
        if true { let x = 22; } else { x = 99 };
        x;
        ",
            Value::Int(1),
        );
    }

    #[test]
    pub fn eval_block_expressions() {
        run("{ 1 };", Value::Int(1));
        run("{ 2 };", Value::Int(2));
        run("{ 3 };", Value::Int(3));
        run("let x = { 1; 2; 3; 4; 5; 3345 }; x;", Value::Int(3345));
    }

    #[test]
    pub fn eval_function() {
        run("fn (){ 1 }();", Value::Int(1));
        run("let f = fn (){1}; f();", Value::Int(1));
        run("let f = fn (x: int){x}; f(123);", Value::Int(123));
    }

    #[test]
    pub fn eval_return() {
        run(
            "
            let test = fn() {
                let x = 23;
                let y = 22;
                y;
            };
            test();
            ",
            Value::Unit,
        );
        run(
            "fn() {
                if true {
                    return 233;
                };
            123;
        }();",
            Value::Int(233),
        );
        run(
            "fn() {
                if false {
                    return 233;
                };
            123
        }();",
            Value::Int(123),
        );
        run(
            "fn() {
                if true {
                    return 233;
                } else {
                    return 123;
                };
        }();",
            Value::Int(233),
        );
        run(
            "
                let noop = fn () {
                    if (1 < 2) {
                        return 1;
                    };
                    return;
                }; noop();
        ",
            Value::Int(1),
        );

        run(
            "
                let noop = fn () {
                    if (1 < 2) {
                        return;
                    };
                    return 1;
                }; noop();
        ",
            Value::Unit,
        );

        run(
            "{
                1;
                2;
                6;
                let x = 1;
                22
             };",
            Value::Int(22),
        );
    }

    #[test]
    pub fn eval_scopes() {
        run(
            "
        let x = 23;
            let y = {
                let x = 123;
            };
        x;
        ",
            Value::Int(23),
        );
        run(
            "
        let x = 23;
            let y = {
                 x = 123;
            };
        x;
        ",
            Value::Int(123),
        );

        run(
            "
        let x = 23;
            let y = {
                 x = 123;
                let x = 123;
            };
        y;
        ",
            Value::Unit,
        );
    }

    #[test]
    pub fn eval_native_function() {
        run("println(1);", Value::Null);
        run("range(1, 22);", Value::Range(1, 22));
        run(r#"len("this is a string");"#, Value::Int(16));
        run(
            " 
        let arr = [0];
        let arr2 = clone(arr);
        arr2[0] = 123;
        arr[0];
        ",
            Value::Int(0),
        );
    }

    #[test]
    pub fn eval_array_access() {
        run("let x = [123]; x[0];", Value::Int(123));
        run("let x = [123]; x[10];", Value::Null);
        run("let x = [1,2,3, 4]; x[1+2];", Value::Int(4));
        run("[[123], 2][0][0];", Value::Int(123));
        run("[[123], 2][0];", Value::array(vec![Value::Int(123)]));
        run(r#"["test"][0];"#, Value::String("test".to_string()));
    }

    #[test]
    pub fn eval_set_array_index() {
        run("let x = [123]; x[0] = 1; x[0];", Value::Int(1));
        run(
            "
        let x = [123]; 
        x[0] = 1;
        x[0] += 2;
        x[0] -= 1;
        x[0] *= 2;
        x[0] /= 1;
        x[0];",
            Value::Int(4),
        );
    }

    #[test]
    pub fn eval_data_struct() {
        run(
            "data Person {};",
            Value::data_struct(ident("Person"), vec![], HashMap::new(), HashMap::new()),
        );

        run(
            "data Person {
                first_name: string,
                last_name: string,
                age: int,
            };",
            Value::data_struct(
                ident("Person"),
                vec![
                    ident_string("first_name"),
                    ident_string("last_name"),
                    ident_int("age"),
                ],
                HashMap::new(),
                HashMap::new(),
            ),
        );
    }

    #[test]
    pub fn eval_data_struct_instance() {
        let mut map = HashMap::new();
        map.insert(
            String::from("first_name"),
            Value::String(String::from("John")),
        );
        map.insert(
            String::from("last_name"),
            Value::String(String::from("Doe")),
        );
        map.insert(String::from("age"), Value::Int(40));

        let field_identifiers = vec![
            ident_string("first_name"),
            ident_string("last_name"),
            ident_int("age"),
        ];
        run(
            r#"data Person {
                first_name: string,
                last_name: string,
                age: int,
            };
            let person = Person { first_name: "John", last_name: "Doe", age: 40 };
            person;
            "#,
            Value::data_struct_instance(
                ident("Person"),
                data_struct(
                    "Person",
                    field_identifiers.clone(),
                    HashMap::new(),
                    HashMap::new(),
                ),
                map.clone(),
                field_identifiers.clone(),
            ),
        );

        map.insert(String::from("age"), Value::Null);
        run(
            r#"data Person {
                first_name: string,
                last_name: string,
                age: int,
            };
            let person = Person { first_name: "John", last_name: "Doe" };
            person;
            "#,
            Value::data_struct_instance(
                ident("Person"),
                data_struct(
                    "Person",
                    field_identifiers.clone(),
                    HashMap::new(),
                    HashMap::new(),
                ),
                map.clone(),
                field_identifiers.clone(),
            ),
        );

        map.insert(String::from("first_name"), Value::Null);
        map.insert(String::from("last_name"), Value::Null);
        map.insert(String::from("age"), Value::Null);
        run(
            r#"data Person {
                first_name: string,
                last_name: string,
                age: int,
            };
            let person = Person {};
            person;
            "#,
            Value::data_struct_instance(
                ident("Person"),
                data_struct(
                    "Person",
                    field_identifiers.clone(),
                    HashMap::new(),
                    HashMap::new(),
                ),
                map.clone(),
                field_identifiers.clone(),
            ),
        );
    }

    #[test]
    pub fn eval_property_access() {
        run(
            r#"data Person {
                first_name: string,
                last_name: string,
                age: int,
            };
            let person = Person { first_name: "John", last_name: "Doe", age: 40 };
            person.first_name;
            "#,
            Value::String("John".to_string()),
        );
    }

    #[test]
    pub fn eval_property_set() {
        run(
            r#"data Person {
                first_name: string,
                last_name: string,
                age: int,
            };
            let person = Person { first_name: "John", last_name: "Doe", age: 40 };
            person.age = 22;
            person.age;
            "#,
            Value::Int(22),
        );

        run(
            r#"data Person {
                first_name: string,
                last_name: string,
                age: int,
            };
            let person = Person { first_name: "John", last_name: "Doe", age: 40 };
            person.age += 1;
            person.age;
            "#,
            Value::Int(41),
        );

        run(
            r#"data Person {
                first_name: string,
                last_name: string,
                age: int,
            };
            let person = Person { first_name: "John", last_name: "Doe", age: 40 };
            person.age += 10;
            person.age -= 10;
            person.age *= 2;
            person.age /= 2;
            person.age;
            "#,
            Value::Int(40),
        );
    }

    #[test]
    pub fn eval_shorthand_data_struct_instantiate() {
        run(
            r#"
            data Person {
                first_name: string,
                last_name: string,
                age: int,
            };
            let first_name = "John";
            let last_name = "Doe";
            let age = 40;
            let person = Person { first_name, last_name, age };
            person.age;
            "#,
            Value::Int(40),
        );
    }

    #[test]
    pub fn eval_data_struct_static_method() {
        run(
            "data Person {
                id: int
            } :: {
                fn new {
                    Person { id: 1 }
                }
            };
            let person = Person.new();
            person.id;
            ",
            Value::Int(1),
        );
    }

    #[test]
    pub fn eval_data_struct_instance_method() {
        run(
            "data Person {
                id: int
            } :: {
                fn new {
                    Person { id: 22 }
                }

                fn get_id(self) {
                    self.id
                }
            };
            let person = Person.new();
            person.get_id();
            ",
            Value::Int(22),
        );

        // should allow a method with the same name as a data struct field to be called
        run(
            "data Person {
                id: int
            } :: {
                fn new {
                    Person { id: 22 }
                }

                fn id(self) {
                    self.id
                }
            };
            let person = Person.new();
            person.id();
            ",
            Value::Int(22),
        );
    }

    #[test]
    pub fn eval_data_struct_instance_set_method() {
        run(
            "data Person {
                id: int
            } :: {
                fn new {
                    Person { id: 22 }
                }

                fn set_id(self, new_id: int) {
                    self.id = new_id;
                }

                fn get_id(self) {
                    self.id
                }
            };
            let person = Person.new();
            person.set_id(98324);
            person.get_id();
            ",
            Value::Int(98324),
        );
    }
}
