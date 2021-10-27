use std::{collections::HashMap, fmt::Debug};

use crate::Value;
use ast::{
    Assign, BinOp, BinaryOperation, Block, Call, Expression, Function, IfConditional, Let, Literal,
    Logic, LogicOperation, Program, Statement, UnaryOp, UnaryOperation,
};

pub enum RuntimeError {}

#[derive(Debug)]
pub struct Interpreter {
    env: HashMap<String, Value>,
}

type InterpreterResult = Result<Value, RuntimeError>;

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {
            env: Default::default(),
        }
    }

    pub fn run(&mut self, stmts: Program) -> Result<Value, RuntimeError> {
        let mut res = Value::Empty;
        for stmt in stmts.iter() {
            match self.execute_statement(stmt) {
                Ok(v) => res = v,
                Err(_) => todo!(),
            }
        }

        Ok(res)
    }

    fn execute_block(&mut self, stmts: &Program) -> InterpreterResult {
        let mut res = Value::Null;
        for stmt in stmts.iter() {
            res = self.execute_statement(stmt)?;
        }
        Ok(res)
    }

    fn execute_statement(&mut self, stmt: &Statement) -> Result<Value, RuntimeError> {
        match stmt {
            Statement::Let(r#let) => self.let_statement(r#let),
            Statement::Expr(expr) => self.expression(expr),
        }
    }

    fn let_statement(&mut self, stmt: &Let) -> Result<Value, RuntimeError> {
        let name = stmt.id.to_string();
        let val = match &stmt.value {
            Some(expr) => self.expression(expr)?,
            None => Value::Null,
        };

        self.env.insert(name, val);

        Ok(Value::Empty)
    }

    fn expression(&mut self, expression: &Expression) -> Result<Value, RuntimeError> {
        match expression {
            Expression::BinOp(expr) => self.eval_binop(expr),
            Expression::Literal(lit) => self.eval_literal(lit),
            Expression::Assign(expr) => self.eval_assignment(expr),
            Expression::LetRef(letref) => self.eval_let_reference(letref),
            Expression::UnaryOp(expr) => self.eval_unaryop(expr),
            Expression::Grouping(expr) => self.expression(&expr.expr),
            Expression::Logic(expr) => self.eval_logic_expression(expr),
            Expression::Call(expr) => self.eval_call(expr),
            Expression::Function(expr) => self.eval_function(expr),
            Expression::Block(expr) => self.eval_block(expr),
            Expression::If(expr) => self.eval_if_conditional(expr),
        }
    }

    fn eval_call(&mut self, call: &Call) -> InterpreterResult {
        let callee = self.expression(&call.callee)?;
        if let Value::Function(fun) = callee {
            let mut args = vec![];
            for arg in &call.arguments {
                let val = self.expression(arg)?;
                args.push(val);
            }

            if args.len() != fun.params.len() {
                panic!("Expected {} arguments got {}", fun.params.len(), args.len())
            }

            let res = self.execute_block(&fun.body)?;
            return Ok(res);
        }
        panic!("You can only call functions");
    }

    fn eval_function(&mut self, function: &Function) -> InterpreterResult {
        let fun = Value::function(
            function.name.clone(),
            function.params.clone(),
            function.body.clone(),
        );

        if let Some(name) = &function.name {
            self.env.insert(name.to_string(), fun.clone());
        };

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
        return self.execute_block(&block.stmts);
    }

    fn eval_if_conditional(&mut self, ifcond: &IfConditional) -> InterpreterResult {
        let cond = self.expression(&ifcond.condition)?;
        if cond.is_truthy() {
            return self.execute_block(&ifcond.then);
        }
        if let Some(el) = &ifcond.not_then {
            return self.execute_block(el);
        }

        Ok(Value::Null)
    }

    fn eval_let_reference(&mut self, letref: &ast::LetRef) -> InterpreterResult {
        let ident = letref.name.0.to_string();
        match self.env.get(&ident) {
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
                Value::Array(res)
            }
            Literal::Null => Value::Null,
        })
    }

    fn eval_assignment(&mut self, assign: &Assign) -> InterpreterResult {
        let name = assign.name.0.clone();
        match self.env.get(&name) {
            Some(_) => {
                let val = self.expression(&assign.rhs)?;
                self.env.insert(name, val.clone());
                Ok(val)
            }
            None => panic!("Assignment to unknown variable"),
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
    use crate::{Interpreter, Value};
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
            Err(err) => todo!(),
        };
    }

    #[test]
    pub fn let_statement() {
        run(("let name = 123;", Value::Empty));
        run(("let name; name;", Value::Null));
        run(("let name = 123; name;", Value::Int(123)));
    }

    #[test]
    pub fn literals() {
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
            Value::Array(vec![
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
}
