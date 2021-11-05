use std::{cell::RefCell, collections::HashMap, convert::TryInto, fmt::Debug, rc::Rc};

use crate::{builtin::get_builtins, environment::Environment, Value};
use ast::{
    Assign, BinOp, BinaryOperation, Block, Call, DataClass, Expression, Function, GetProperty,
    IfConditional, Index, Let, Literal, Logic, LogicOperation, LoopExpr, Program, ReturnExpr,
    SelfExpr, SetIndex, SetProperty, Statement, UnaryOp, UnaryOperation,
};

#[derive(Debug)]
pub enum RuntimeError {}

#[derive(Debug)]
pub struct Interpreter {
    env: Rc<RefCell<Environment>>,
}

type InterpreterResult = Result<Value, RuntimeError>;

// macro used for swapping the parent environment with the new supplied environmet
// this also makes sure this code isnt duplicated when evaluating if, block and function calls
macro_rules! eval_with_new_env_in_scope {
    ($self:ident, $env:expr, $fun:ident, $stmt:expr) => {{
        let parent_env = $self.env.clone();
        $self.env = $env;
        let res = $self.$fun($stmt);
        $self.env = parent_env;
        res
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

    pub fn run(&mut self, stmts: Program) -> InterpreterResult {
        self.eval(&stmts)
    }

    fn eval(&mut self, stmts: &Program) -> InterpreterResult {
        let res = self.eval_statements(stmts)?;
        match res {
            Value::ReturnVal(val) => Ok(*val),
            _ => Ok(res),
        }
    }

    fn eval_statements(&mut self, stmts: &Program) -> InterpreterResult {
        let mut res = Value::Unit;
        for stmt in stmts.iter() {
            res = self.execute_statement(stmt)?;
            match res {
                Value::ReturnVal(_) | Value::Break | Value::Continue => return Ok(res),
                _ => {}
            }
        }
        Ok(res)
    }

    fn execute_statement(&mut self, stmt: &Statement) -> InterpreterResult {
        match stmt {
            Statement::Let(r#let) => self.let_statement(r#let),
            Statement::Expr(expr) => self.expression(expr),
        }
    }

    fn let_statement(&mut self, stmt: &Let) -> InterpreterResult {
        let name = stmt.id.to_string();
        let val = match &stmt.value {
            Some(expr) => self.expression(expr)?,
            None => Value::Null,
        };

        self.env.borrow_mut().define(name, val);

        Ok(Value::Unit)
    }

    fn expression(&mut self, expression: &Expression) -> InterpreterResult {
        match expression {
            Expression::BinOp(expr) => self.eval_binop(expr),
            Expression::Literal(expr) => self.eval_literal(expr),
            Expression::Assign(expr) => self.eval_assignment(expr),
            Expression::LetRef(expr) => self.eval_let_reference(expr),
            Expression::UnaryOp(expr) => self.eval_unaryop(expr),
            Expression::Grouping(expr) => self.expression(&expr.expr),
            Expression::Logic(expr) => self.eval_logic_expression(expr),
            Expression::Call(expr) => self.eval_call(expr),
            Expression::Function(expr) => self.eval_function(expr),
            Expression::Block(expr) => self.eval_block(expr),
            Expression::If(expr) => self.eval_if_conditional(expr),
            Expression::Index(expr) => self.eval_index(expr),
            Expression::SetIndex(expr) => self.eval_set_index(expr),
            Expression::Return(expr) => self.eval_return(expr),
            Expression::DataClass(expr) => self.eval_data_class(expr),
            Expression::DataClassInstance(expr) => self.eval_data_class_instance(expr),
            Expression::GetProperty(expr) => self.eval_get_property(expr),
            Expression::SetProperty(expr) => self.eval_set_property(expr),
            Expression::SelfExpr(expr) => self.eval_self_expr(expr),
            Expression::LoopExpr(expr) => self.eval_loop_expr(expr),
            Expression::BreakExpr(_) => Ok(Value::Break),
            Expression::ContinueExpr(_) => Ok(Value::Continue),
        }
    }

    fn eval_loop_expr(&mut self, loop_expr: &LoopExpr) -> InterpreterResult {
        let mut outval = Value::Unit;
        while (self.expression(&loop_expr.condition)?).is_truthy() {
            let val = self.eval_statements(&loop_expr.body)?;
            match val {
                Value::Break | Value::ReturnVal(_) => {
                    if let Value::ReturnVal(_) = val {
                        outval = val;
                    }
                    break;
                }
                Value::Continue => {
                    continue;
                }
                _ => {}
            }
        }
        Ok(outval)
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
            _ => panic!(
                "Can only set properties on data class instances got {}",
                obj
            ),
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
                match val {
                    Value::Function(ref mut fun) => {
                        fun.closure.borrow_mut().define(
                            "self".to_string(),
                            Value::DataClassInstance(instance.clone()),
                        );
                        if fun.arity > 0 {
                            fun.arity = fun.arity - 1;
                        }
                    }
                    _ => {}
                };
                Ok(val.clone())
            }
            Value::DataClass(d) => {
                let fun = d.get(get_property.name.value.to_string());
                Ok(Value::Function(fun))
            }
            Value::Null => panic!("Cannot access {} on null", get_property.name),
            _ => panic!(
                "Can only access properties on data class instances got {}",
                obj
            ),
        }
    }

    fn eval_data_class_instance(
        &mut self,
        data_class_instance: &ast::DataClassInstance,
    ) -> InterpreterResult {
        let data_class_identifier = data_class_instance.name.clone();
        let data_class = match self.env.borrow().get(&data_class_identifier.value) {
            Some(val) => match val {
                Value::DataClass(class) => class.clone(),
                _ => panic!("Cannot only instantiate data classes got {}", val),
            },
            None => panic!("Unkown data class {}", data_class_identifier),
        };

        let mut eval_fields = HashMap::new();

        //  check if the instance fields are actually in the Data class e.g
        //  data Person { name };
        //  Person { name: "test", age: 2 } // Error unknown field age
        for field in &data_class_instance.fields {
            if data_class.fields.contains(&field.name) {
                let val = self.expression(&field.value)?;
                eval_fields.insert(field.name.value.to_string(), val);
            } else {
                panic!(
                    "Unknown field {} provided to data class {}",
                    field.name, data_class_identifier
                )
            }
        }

        // fill in the missing fields with null value
        for field in &data_class.fields {
            if !eval_fields.contains_key(&field.value) {
                eval_fields.insert(field.value.to_string(), Value::Null);
            }
        }

        Ok(Value::data_class_instance(
            data_class_identifier,
            data_class.clone(),
            eval_fields,
            data_class.fields.clone(),
        ))
    }

    fn eval_data_class(&mut self, data_class: &DataClass) -> InterpreterResult {
        let mut static_methods = HashMap::new();
        let mut instance_methods = HashMap::new();

        for method in &data_class.methods {
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

        let value = Value::data_class(
            data_class.name.to_owned(),
            data_class.fields.clone(),
            static_methods,
            instance_methods,
        );

        self.env
            .borrow_mut()
            .define(data_class.name.value.to_string(), value.clone());

        Ok(value)
    }

    fn eval_return(&mut self, ret: &ReturnExpr) -> InterpreterResult {
        match &*ret.value {
            Some(op) => Ok(Value::return_val(self.expression(op)?)),
            None => Ok(Value::return_val(Value::Null)),
        }
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
                // match &*set_index.lhs {
                //     Expression::LetRef(let_ref) => {
                //         array[idx] = value.clone();

                //         self.env
                //             .borrow_mut()
                //             .assign(let_ref.name.0.to_string(), Value::Array(newarr));
                //     }
                //     _ => {}
                // }

                Ok(value)
            }
            _ => panic!("Invalid set index on {}", lhs),
        }
    }

    fn eval_index(&mut self, index: &Index) -> InterpreterResult {
        let lhs = self.expression(&index.lhs)?;
        let idx = self.expression(&index.index)?;

        match (&lhs, &idx) {
            (Value::Array(arr), Value::Int(i)) => match arr.borrow().values.get(*i as usize) {
                Some(val) => Ok(val.clone()),
                None => Ok(Value::Null),
            },
            _ => panic!("Invalid index access on {}", lhs),
        }
    }

    fn eval_call(&mut self, call: &Call) -> InterpreterResult {
        let callee = self.expression(&call.callee)?;
        let mut args = vec![];

        for arg in &call.arguments {
            let val = self.expression(arg)?;
            args.push(val);
        }

        match callee {
            Value::Function(fun) => {
                let mut env = Environment::with_outer(fun.closure.clone());
                let mut params = vec![];

                // TODO mark value functions as methods
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
                eval_with_new_env_in_scope!(self, env, eval, &fun.body)
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
            function.body.clone(),
            self.env.clone(),
        );

        Ok(fun)
    }

    fn eval_logic_expression(&mut self, logic: &Logic) -> InterpreterResult {
        let left = self.expression(&logic.lhs)?;
        let right = self.expression(&logic.rhs)?;
        let res = match (&left, &logic.op, &right) {
            // integer
            (Value::Int(n1), LogicOperation::Equal, Value::Int(n2)) => Value::Bool(n1 == n2),
            (Value::Int(n1), LogicOperation::NotEqual, Value::Int(n2)) => Value::Bool(n1 != n2),
            (Value::Int(n1), LogicOperation::LessThan, Value::Int(n2)) => Value::Bool(n1 < n2),
            (Value::Int(n1), LogicOperation::LessThanEqual, Value::Int(n2)) => {
                Value::Bool(n1 <= n2)
            }
            (Value::Int(n1), LogicOperation::GreaterThan, Value::Int(n2)) => Value::Bool(n1 > n2),
            (Value::Int(n1), LogicOperation::GreaterThanEqual, Value::Int(n2)) => {
                Value::Bool(n1 >= n2)
            }

            // float
            (Value::Float(n1), LogicOperation::Equal, Value::Float(n2)) => Value::Bool(n1 == n2),
            (Value::Float(n1), LogicOperation::NotEqual, Value::Float(n2)) => Value::Bool(n1 != n2),
            (Value::Float(n1), LogicOperation::LessThan, Value::Float(n2)) => Value::Bool(n1 < n2),
            (Value::Float(n1), LogicOperation::LessThanEqual, Value::Float(n2)) => {
                Value::Bool(n1 <= n2)
            }
            (Value::Float(n1), LogicOperation::GreaterThan, Value::Float(n2)) => {
                Value::Bool(n1 > n2)
            }
            (Value::Float(n1), LogicOperation::GreaterThanEqual, Value::Float(n2)) => {
                Value::Bool(n1 >= n2)
            }

            // bool
            (Value::Bool(b1), LogicOperation::And, Value::Bool(b2)) => Value::Bool(*b1 && *b2),
            (Value::Bool(b1), LogicOperation::Or, Value::Bool(b2)) => Value::Bool(*b1 || *b2),

            // string
            (Value::String(s1), LogicOperation::Equal, Value::String(s2)) => Value::Bool(s1 == s2),
            (Value::String(s1), LogicOperation::NotEqual, Value::String(s2)) => {
                Value::Bool(s1 != s2)
            }

            _ => panic!(
                "Invalid logic operation {} on {} and {}",
                logic.op, left, right
            ),
        };

        Ok(res)
    }

    fn eval_block(&mut self, block: &Block) -> InterpreterResult {
        let env = Rc::new(RefCell::new(Environment::with_outer(self.env.clone())));
        eval_with_new_env_in_scope!(self, env, eval_statements, &block.stmts)
    }

    fn eval_if_conditional(&mut self, ifcond: &IfConditional) -> InterpreterResult {
        let cond = self.expression(&ifcond.condition)?;
        let env = Rc::new(RefCell::new(Environment::with_outer(self.env.clone())));
        let val = Value::Null;

        if cond.is_truthy() {
            return eval_with_new_env_in_scope!(self, env, eval_statements, &ifcond.then);
        } else if let Some(el) = &ifcond.not_then {
            return eval_with_new_env_in_scope!(self, env, eval_statements, el);
        }

        Ok(val)
    }

    fn eval_let_reference(&mut self, letref: &ast::LetRef) -> InterpreterResult {
        let ident = letref.name.value.to_string();
        match self.env.borrow().get(&ident) {
            Some(var) => Ok(var.clone()),
            None => panic!("undefined variable {}", ident),
        }
    }

    fn eval_literal(&mut self, lit: &Literal) -> InterpreterResult {
        Ok(match lit {
            Literal::Int(v) => Value::Int(*v),
            Literal::Float(v) => Value::Float(*v),
            Literal::Bool(v) => Value::Bool(*v),
            Literal::String(v) => Value::String(v.to_string()),
            Literal::Array(v) => {
                let mut res = vec![];
                for e in v {
                    res.push(self.expression(e)?);
                }
                Value::array(res)
            }
            Literal::Null => Value::Null,
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
            (UnaryOperation::Minus, Value::Int(n1)) => Value::Int(-n1),
            (UnaryOperation::Plus, Value::Int(n1)) => Value::Int(n1),
            (UnaryOperation::Minus, Value::Float(n1)) => Value::Float(-n1),
            (UnaryOperation::Plus, Value::Float(n1)) => Value::Float(n1),
            (UnaryOperation::Not, Value::Bool(b1)) => Value::Bool(!b1),
            _ => panic!("invalid unary operation"),
        };

        Ok(res)
    }

    fn eval_binop(&mut self, binop: &BinOp) -> InterpreterResult {
        let left = self.expression(&binop.left)?;
        let right = self.expression(&binop.right)?;

        let res = match (left, &binop.op, right) {
            // integer binop
            (Value::Int(n1), BinaryOperation::Add, Value::Int(n2)) => Value::Int(n1 + n2),
            (Value::Int(n1), BinaryOperation::Substract, Value::Int(n2)) => Value::Int(n1 - n2),
            (Value::Int(n1), BinaryOperation::Multiply, Value::Int(n2)) => Value::Int(n1 * n2),
            (Value::Int(n1), BinaryOperation::Divide, Value::Int(n2)) => Value::Int(n1 / n2),

            // float binop
            (Value::Float(n1), BinaryOperation::Add, Value::Float(n2)) => Value::Float(n1 + n2),
            (Value::Float(n1), BinaryOperation::Substract, Value::Float(n2)) => {
                Value::Float(n1 - n2)
            }
            (Value::Float(n1), BinaryOperation::Multiply, Value::Float(n2)) => {
                Value::Float(n1 * n2)
            }
            (Value::Float(n1), BinaryOperation::Divide, Value::Float(n2)) => Value::Float(n1 / n2),

            (v1, _, v2) => panic!("Invalid binary operation on {} and {}", v1, v2),
        };

        Ok(res)
    }
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;

    use crate::{DataClass, FunctionValue, Interpreter, Value};
    use ast::Identifier;
    use parser::Parser;
    use scanner::Scanner;
    use spanner::SpanManager;

    pub fn run(src: (&str, Value)) {
        let mut manager = SpanManager::default();
        let mut maker = manager.add_source(src.0.to_string());
        let scanner = Scanner::new(src.0.to_string(), &mut maker);
        let mut parser = Parser::new(scanner);

        let (stmts, errors) = parser.parse();

        if errors.len() > 0 {
            errors
                .into_iter()
                .for_each(|mut e| println!("{}", e.into_spanned_error().print(&manager)));
            panic!("parser errors")
        }

        let mut interpreter = Interpreter::new();

        match interpreter.run(stmts) {
            Ok(val) => assert_eq!(val, src.1),
            Err(err) => println!("{:#?}", err),
        };
    }

    fn ident(name: &str) -> Identifier {
        Identifier::new(name.to_string())
    }

    fn data_class(
        name: &str,
        fields: Vec<Identifier>,
        static_methods: HashMap<String, FunctionValue>,
        instance_methods: HashMap<String, FunctionValue>,
    ) -> DataClass {
        DataClass {
            name: Identifier::new(name.to_string()),
            fields,
            static_methods,
            instance_methods,
        }
    }

    #[test]
    fn eval_loop() {
        run((
            "
        let i = 0;
        loop i < 10 {
            i += 1;
        };
        i;
        ",
            Value::Int(10),
        ));

        run((
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
        ));
    }

    #[test]
    fn let_statement() {
        run(("let name = 123;", Value::Unit));
        run(("let name; name;", Value::Null));
        run(("let name = 123; name;", Value::Int(123)));
    }

    #[test]
    fn literals() {
        run(("let x = true; x;", Value::Bool(true)));
        run(("let x = false; x;", Value::Bool(false)));
        run(("let x = 123; x;", Value::Int(123)));
        run(("let x = 12.45; x;", Value::Float(12.45)));
        run(("let x = null; x;", Value::Null));
        run((
            r#"let x = "test string"; x;"#,
            Value::String("test string".to_string()),
        ));

        run((
            r#"let x = ["test", 1, 1.5, true, false, null]; x;"#,
            Value::array(vec![
                Value::String("test".to_string()),
                Value::Int(1),
                Value::Float(1.5),
                Value::Bool(true),
                Value::Bool(false),
                Value::Null,
            ]),
        ));
    }

    #[test]
    pub fn eval_binop() {
        run(("let x = 1 + 2 * 4 / 2 - 2; x;", Value::Int(3)));
        run(("let x = 1 + 2 * 4; x;", Value::Int(9)));
        run(("let x = 6 * 4 * 2 * 6; x;", Value::Int(288)));

        run(("let x = 2.5 * 4.5 / 0.2; x;", Value::Float(56.25)));
        run(("let x = 298.234 + 99.12 - 22.0; x;", Value::Float(375.354)));
        run(("-2 + -4;", Value::Int(-6)));
        run(("-2.0 + -4.0;", Value::Float(-6.0)));
    }

    #[test]
    pub fn eval_grouping() {
        run(("let x = (1 + 2) * 4; x;", Value::Int(12)));
        run(("let x = 3 * 3 / (2 + 4); x;", Value::Int(1)));
        run(("let x = (1 + 2) * 4; x;", Value::Int(12)));
    }

    #[test]
    pub fn eval_unary() {
        run(("-12;", Value::Int(-12)));
        run(("+12;", Value::Int(12)));
        run(("-12.23;", Value::Float(-12.23)));
        run(("+12.23;", Value::Float(12.23)));
        run(("!true;", Value::Bool(false)));
        run(("!false;", Value::Bool(true)));
    }

    #[test]
    pub fn assignment() {
        run(("let x; x = 1; x;", Value::Int(1)));
        run(("let x = 2; x += 1; x;", Value::Int(3)));
        run(("let x = 2; x *= 3; x;", Value::Int(6)));
        run(("let x = 2; x /= 2; x;", Value::Int(1)));
        run(("let x = 2; x -= 1; x;", Value::Int(1)));
    }

    #[test]
    pub fn logic_expression() {
        run(("1 == 1;", Value::Bool(true)));
        run(("1 != 1;", Value::Bool(false)));
        run(("1 == 2;", Value::Bool(false)));
        run((r#" "test" == "test";"#, Value::Bool(true)));
        run((r#" "test" != "test";"#, Value::Bool(false)));
        run((r#" "test1" == "test";"#, Value::Bool(false)));
        run(("true && false;", Value::Bool(false)));
        run(("true && true;", Value::Bool(true)));
        run(("false && false;", Value::Bool(false)));
        run(("true || false;", Value::Bool(true)));
        run(("false || true;", Value::Bool(true)));
        run(("false || false;", Value::Bool(false)));
    }

    #[test]
    pub fn if_condiditional() {
        run(("if true { 1; };", Value::Int(1)));
        run(("if false { 1; } else { 2; };", Value::Int(2)));
        run(("let x = if false { 1; } else { 2; }; x;", Value::Int(2)));
        run((
            "
        let x = 1;
        if false { x = 22; } else { x = 99; }; 
        x;
        ",
            Value::Int(99),
        ));
        run((
            "
        let x = 1;
        if true { let x = 22; } else { x = 99; }; 
        x;
        ",
            Value::Int(1),
        ));
    }

    #[test]
    pub fn block_expressions() {
        run(("{ 1; };", Value::Int(1)));
        run(("{ 2; };", Value::Int(2)));
        run(("{ 3; };", Value::Int(3)));
        run(("let x = { 1;2;3;4;5;3345; }; x;", Value::Int(3345)));
    }

    #[test]
    pub fn eval_function() {
        run(("fun (){1;}();", Value::Int(1)));
        run(("let fn = fun (){1;}; fn();", Value::Int(1)));
        run(("let fn = fun (x){x;}; fn(123);", Value::Int(123)));
    }

    #[test]
    pub fn eval_return() {
        run((
            "fun() {
                if true {
                    return 233;
                };
            123;
        }();",
            Value::Int(233),
        ));
        run((
            "fun() {
                if false {
                    return 233;
                };
            123;
        }();",
            Value::Int(123),
        ));
        run((
            "fun() {
                if true {
                    return 233;
                } else {
                    return 123;
                };
        }();",
            Value::Int(233),
        ));
        run((
            "
                let noop = fun () {
                    if (1 < 2) {
                        return 1;
                    };
                    return;
                }; noop();
        ",
            Value::Int(1),
        ));

        run((
            "
                let noop = fun () {
                    if (1 < 2) {
                        return;
                    };
                    return 1;
                }; noop();
        ",
            Value::Null,
        ));

        run((
            "{
                1;
                2;
                return 22;
                6;
                let x = 1;
             };",
            Value::Int(22),
        ));
    }

    #[test]
    pub fn eval_scopes() {
        run((
            "
        let x = 23;
            let y = {
                let x = 123;
            };
        x;
        ",
            Value::Int(23),
        ));
        run((
            "
        let x = 23;
            let y = {
                 x = 123;
            };
        x;
        ",
            Value::Int(123),
        ));

        run((
            "
        let x = 23;
            let y = {
                 x = 123;
                let x = 123;
            };
        y;
        ",
            Value::Unit,
        ));
    }

    #[test]
    pub fn eval_native_function() {
        run(("println(1);", Value::Null));
        run((r#"len("this is a string");"#, Value::Int(16)));
        run((
            " 
        let arr = [0];
        let arr2 = clone(arr);
        arr2[0] = 123;
        arr[0];
        ",
            Value::Int(0),
        ));
    }

    #[test]
    pub fn eval_array_access() {
        run(("let x = [123]; x[0];", Value::Int(123)));
        run(("let x = [123]; x[10];", Value::Null));
        run(("let x = [1,2,3, 4]; x[1+2];", Value::Int(4)));
        run(("[[123], 2][0][0];", Value::Int(123)));
        run(("[[123], 2][0];", Value::array(vec![Value::Int(123)])));
        run((r#"["test"][0];"#, Value::String("test".to_string())));
    }

    #[test]
    pub fn eval_set_array_index() {
        run(("let x = [123]; x[0] = 1; x[0];", Value::Int(1)));
        run((
            "
        let x = [123]; 
        x[0] = 1;
        x[0] += 2;
        x[0] -= 1;
        x[0] *= 2;
        x[0] /= 1;
        x[0];",
            Value::Int(4),
        ));
    }

    #[test]
    pub fn eval_data_class() {
        run((
            "data Person {};",
            Value::data_class(ident("Person"), vec![], HashMap::new(), HashMap::new()),
        ));

        run((
            "data Person {
                first_name,
                last_name,
                age,
            };",
            Value::data_class(
                ident("Person"),
                vec![ident("first_name"), ident("last_name"), ident("age")],
                HashMap::new(),
                HashMap::new(),
            ),
        ));
    }

    #[test]
    pub fn eval_data_class_instance() {
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

        let field_idents = vec![ident("first_name"), ident("last_name"), ident("age")];
        run((
            r#"data Person {
                first_name,
                last_name,
                age,
            };
            let person = Person { first_name: "John", last_name: "Doe", age: 40 };
            person;
            "#,
            Value::data_class_instance(
                ident("Person"),
                data_class(
                    "Person",
                    field_idents.clone(),
                    HashMap::new(),
                    HashMap::new(),
                ),
                map.clone(),
                field_idents.clone(),
            ),
        ));

        map.insert(String::from("age"), Value::Null);
        run((
            r#"data Person {
                first_name,
                last_name,
                age,
            };
            let person = Person { first_name: "John", last_name: "Doe" };
            person;
            "#,
            Value::data_class_instance(
                ident("Person"),
                data_class(
                    "Person",
                    field_idents.clone(),
                    HashMap::new(),
                    HashMap::new(),
                ),
                map.clone(),
                field_idents.clone(),
            ),
        ));

        map.insert(String::from("first_name"), Value::Null);
        map.insert(String::from("last_name"), Value::Null);
        map.insert(String::from("age"), Value::Null);
        run((
            r#"data Person {
                first_name,
                last_name,
                age,
            };
            let person = Person {};
            person;
            "#,
            Value::data_class_instance(
                ident("Person"),
                data_class(
                    "Person",
                    field_idents.clone(),
                    HashMap::new(),
                    HashMap::new(),
                ),
                map.clone(),
                field_idents.clone(),
            ),
        ));
    }

    #[test]
    pub fn eval_property_access() {
        run((
            r#"data Person {
                first_name,
                last_name,
                age,
            };
            let person = Person { first_name: "John", last_name: "Doe", age: 40 };
            person.first_name;
            "#,
            Value::String("John".to_string()),
        ));
    }

    #[test]
    pub fn eval_property_set() {
        run((
            r#"data Person {
                first_name,
                last_name,
                age,
            };
            let person = Person { first_name: "John", last_name: "Doe", age: 40 };
            person.age = 22;
            person.age;
            "#,
            Value::Int(22),
        ));

        run((
            r#"data Person {
                first_name,
                last_name,
                age,
            };
            let person = Person { first_name: "John", last_name: "Doe", age: 40 };
            person.age += 1;
            person.age;
            "#,
            Value::Int(41),
        ));

        run((
            r#"data Person {
                first_name,
                last_name,
                age,
            };
            let person = Person { first_name: "John", last_name: "Doe", age: 40 };
            person.age += 10;
            person.age -= 10;
            person.age *= 2;
            person.age /= 2;
            person.age;
            "#,
            Value::Int(40),
        ));
    }

    #[test]
    pub fn eval_shorthand_data_class_instantiate() {
        run((
            r#"
            data Person {
                first_name,
                last_name,
                age,
            };
            let first_name = "John";
            let last_name = "Doe";
            let age = 40;
            let person = Person { first_name, last_name, age };
            person.age;
            "#,
            Value::Int(40),
        ));
    }

    #[test]
    pub fn eval_data_class_static_method() {
        run((
            "data Person {
                id
            } :: {
                fun new {
                    Person { id: 1 };
                }
            };
            let person = Person.new();
            person.id;
            ",
            Value::Int(1),
        ));
    }

    #[test]
    pub fn eval_data_class_instance_method() {
        run((
            "data Person {
                id
            } :: {
                fun new {
                    Person { id: 22 };
                }

                fun get_id(self) {
                    self.id;
                }
            };
            let person = Person.new();
            person.get_id();
            ",
            Value::Int(22),
        ));

        // should allow a method with the same name as a data class field to be called
        run((
            "data Person {
                id
            } :: {
                fun new {
                    Person { id: 22 };
                }

                fun id(self) {
                    self.id;
                }
            };
            let person = Person.new();
            person.id();
            ",
            Value::Int(22),
        ));
    }

    #[test]
    pub fn eval_data_class_instance_set_method() {
        run((
            "data Person {
                id
            } :: {
                fun new {
                    Person { id: 22 };
                }

                fun set_id(self, new_id) {
                    self.id = new_id;
                }

                fun get_id(self) {
                    self.id;
                }
            };
            let person = Person.new();
            person.set_id(98324);
            person.get_id();
            ",
            Value::Int(98324),
        ));
    }
}
