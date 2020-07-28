#[cfg(test)]
mod tests;

use crate::callable::{populate_natives, Function, LoxCallable};
use crate::expr::{BinOp, Expr, LitExpr, LogOp, Param, UnOp, Visitor as ExprVisitor};
use crate::location::Loc;
use crate::stmt::{Stmt, Visitor as StmtVisitor};
use crate::value::Value;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub struct Interpreter {
    env: Env,
    pub globals: Env,
    locals: HashMap<(String, Loc), usize>,
}

#[derive(Debug)]
pub struct Environ {
    values: HashMap<String, Value>,
    enclosing: Option<Env>,
}

pub type Env = Rc<RefCell<Environ>>;

#[derive(Debug, PartialEq, Fail)]
pub enum RuntimeError {
    UnsupportedOperand(Loc, String, String),
    UnsupportedOperands(Loc, String, String, String),
    DivisionByZero(Loc),
    UndefinedVariable(Loc, String),
    NotACallable(Loc, String),
    MismatchingArity(Loc, usize, usize),
}

#[derive(Debug)]
pub enum RuntimeInterrupt {
    Error(RuntimeError),
    Return(Value),
    Break,
}

pub type ValueRes = Result<Value, RuntimeError>;
type ExecuteRes = Result<(), RuntimeInterrupt>;

impl Interpreter {
    pub fn new() -> Self {
        let globals = Environ::new().into();
        let mut inter = Interpreter {
            env: Rc::clone(&globals),
            globals,
            locals: HashMap::new(),
        };

        populate_natives(&mut inter);
        inter
    }

    pub fn interpret(&mut self, stmts: &[Stmt]) -> Result<(), RuntimeError> {
        for stmt in stmts {
            self.execute(stmt).map_err(RuntimeInterrupt::expect_error)?
        }

        Ok(())
    }

    pub fn evaluate(&mut self, expr: &Expr) -> ValueRes {
        expr.accept(self)
    }

    fn execute(&mut self, stmt: &Stmt) -> ExecuteRes {
        stmt.accept(self)
    }

    pub fn execute_block(&mut self, stmts: &[Stmt], env: Env) -> ExecuteRes {
        let prev = Rc::clone(&self.env);
        self.env = env;

        for stmt in stmts {
            match self.execute(stmt) {
                Ok(()) => (),
                Err(err) => {
                    self.env = prev;
                    return Err(err);
                }
            }
        }

        self.env = prev;
        Ok(())
    }

    pub fn resolve(&mut self, name: &str, loc: Loc, depth: usize) {
        self.locals.insert((String::from(name), loc), depth);
    }

    fn get_local_distance(&self, name: &str, loc: Loc) -> Option<usize> {
        self.locals.get(&(name.to_string(), loc)).cloned()
    }

    fn look_up_variable(&self, name: &str, loc: Loc) -> Result<Value, RuntimeError> {
        if let Some(distance) = self.get_local_distance(name, loc) {
            Ok(self.env.borrow().get_at(distance, name))
        } else {
            self.globals.borrow().get(name, loc)
        }
    }
}

impl Environ {
    pub fn new() -> Self {
        Environ {
            values: HashMap::new(),
            enclosing: None,
        }
    }

    pub fn with_enclosing(enclosing: &Env) -> Env {
        let env = Environ {
            values: HashMap::new(),
            enclosing: Some(Rc::clone(enclosing)),
        };

        env.into()
    }

    pub fn define(&mut self, name: &str, val: Value) {
        self.values.insert(String::from(name), val);
    }

    pub fn get(&self, name: &str, loc: Loc) -> Result<Value, RuntimeError> {
        if let Some(val) = self.values.get(name) {
            Ok(val.clone())
        } else if let Some(ref env) = self.enclosing {
            env.borrow().get(name, loc)
        } else {
            Err(RuntimeError::undefined_variable(loc, name))
        }
    }

    fn ancestor(&self, distance: usize) -> &Environ {
        let mut env: *const Environ = self;

        for _ in 0..distance {
            if let Some(enclosing) = unsafe { (*env).enclosing.as_ref() } {
                env = enclosing.as_ptr();
            } else {
                panic!("Ancestor not found at distance {}", distance);
            }
        }

        unsafe { &*env }
    }

    fn get_at(&self, distance: usize, name: &str) -> Value {
        if let Some(val) = self.ancestor(distance).values.get(name) {
            val.clone()
        } else {
            panic!("Variable '{}' not found at distance {}", name, distance)
        }
    }

    pub fn assign(&mut self, name: &str, val: Value, loc: Loc) -> Result<(), RuntimeError> {
        if self.values.contains_key(name) {
            self.values.insert(String::from(name), val);

            Ok(())
        } else if let Some(ref env) = self.enclosing {
            env.borrow_mut().assign(name, val, loc)
        } else {
            Err(RuntimeError::undefined_variable(loc, &name))
        }
    }

    fn ancestor_mut(&mut self, distance: usize) -> &mut Environ {
        let mut env: *mut Environ = self;

        for _ in 0..distance {
            if let Some(enclosing) = unsafe { (*env).enclosing.as_ref() } {
                env = enclosing.as_ptr();
            } else {
                panic!("Ancestor not found at distance {}", distance);
            }
        }

        unsafe { &mut *env }
    }

    fn assign_at(&mut self, distance: usize, name: &str, val: Value) {
        self.ancestor_mut(distance)
            .values
            .insert(String::from(name), val);
    }
}

impl Into<Env> for Environ {
    fn into(self) -> Env {
        Rc::new(RefCell::new(self))
    }
}

impl ExprVisitor<Value> for Interpreter {
    type Error = RuntimeError;

    fn visit_literal_expr(&mut self, literal: &LitExpr, _loc: Loc) -> ValueRes {
        Ok(literal.into())
    }

    fn visit_function_expr(&mut self, params: &[Param], body: &[Stmt], _loc: Loc) -> ValueRes {
        let params: Vec<_> = params.iter().map(|p| p.kind.clone()).collect();
        let function = Function::new_anon(params, body, &self.env);
        Ok(function.into())
    }

    fn visit_grouping_expr(&mut self, expr: &Expr, _loc: Loc) -> ValueRes {
        self.evaluate(expr)
    }

    fn visit_unary_expr(&mut self, op: &UnOp, expr: &Expr, loc: Loc) -> ValueRes {
        let val = self.evaluate(expr)?;

        Ok(match op {
            UnOp::Negate => val.negate(loc)?,
            UnOp::Not => val.not(),
        })
    }

    fn visit_binary_expr(&mut self, left: &Expr, op: &BinOp, right: &Expr, loc: Loc) -> ValueRes {
        let left_val = self.evaluate(left)?;
        let right_val = self.evaluate(right)?;

        Ok(match op {
            BinOp::Add => left_val.add(right_val, loc)?,
            BinOp::Sub => left_val.sub(right_val, loc)?,
            BinOp::Mul => left_val.mul(right_val, loc)?,
            BinOp::Div => left_val.div(right_val, loc)?,
            BinOp::Rem => left_val.rem(right_val, loc)?,
            BinOp::Equal => left_val.equal(&right_val),
            BinOp::NotEqual => left_val.not_eq(&right_val),
            BinOp::Greater => left_val.greater(&right_val, loc)?,
            BinOp::GreaterEqual => left_val.greater_eq(&right_val, loc)?,
            BinOp::Less => left_val.less(&right_val, loc)?,
            BinOp::LessEqual => left_val.less_eq(&right_val, loc)?,
        })
    }

    fn visit_logical_expr(&mut self, left: &Expr, op: &LogOp, right: &Expr, _loc: Loc) -> ValueRes {
        let left_val = self.evaluate(left)?;

        Ok(match op {
            LogOp::And if !left_val.is_truthy() => left_val,
            LogOp::Or if left_val.is_truthy() => left_val,
            _ => self.evaluate(right)?,
        })
    }

    fn visit_comma_expr(&mut self, left: &Expr, right: &Expr, _loc: Loc) -> ValueRes {
        let _ = self.evaluate(left)?;
        self.evaluate(right)
    }

    fn visit_cond_expr(&mut self, cond: &Expr, left: &Expr, right: &Expr, _loc: Loc) -> ValueRes {
        let cond_val = self.evaluate(cond)?;
        if cond_val.is_truthy() {
            self.evaluate(left)
        } else {
            self.evaluate(right)
        }
    }

    fn visit_variable_expr(&mut self, name: &str, loc: Loc) -> ValueRes {
        self.look_up_variable(name, loc)
    }

    fn visit_assign_expr(&mut self, name: &str, expr: &Expr, loc: Loc) -> ValueRes {
        let val = self.evaluate(expr)?;
        let cloned_val = val.clone();

        if let Some(distance) = self.get_local_distance(name, loc) {
            self.env.borrow_mut().assign_at(distance, name, val);
        } else {
            self.globals.borrow_mut().assign(name, val, loc)?;
        }

        Ok(cloned_val)
    }

    fn visit_call_expr(&mut self, callee: &Expr, args: &[Expr], loc: Loc) -> ValueRes {
        let callee = self.evaluate(callee)?;
        let args: Vec<_> = args
            .iter()
            .map(|a| self.evaluate(a))
            .collect::<Result<_, _>>()?;

        if let Value::Callable(callable) = callee {
            if args.len() == callable.arity() {
                callable.call(self, args)
            } else {
                Err(RuntimeError::mismatching_arity(
                    loc,
                    callable.arity(),
                    args.len(),
                ))
            }
        } else {
            Err(RuntimeError::not_a_callable(loc, callee))
        }
    }
}

impl StmtVisitor<()> for Interpreter {
    type Error = RuntimeInterrupt;

    fn visit_expression_stmt(&mut self, expr: &Expr, _loc: Loc) -> ExecuteRes {
        self.evaluate(expr)?;
        Ok(())
    }

    fn visit_if_stmt(
        &mut self,
        cond: &Expr,
        then_branch: &Stmt,
        else_branch: &Option<Box<Stmt>>,
        _loc: Loc,
    ) -> ExecuteRes {
        let cond_val = self.evaluate(cond)?;
        if cond_val.is_truthy() {
            self.execute(then_branch)
        } else if let Some(else_stmt) = else_branch {
            self.execute(else_stmt)
        } else {
            Ok(())
        }
    }

    fn visit_print_stmt(&mut self, expr: &Expr, _loc: Loc) -> ExecuteRes {
        let val = self.evaluate(expr)?;
        println!("{}", val);
        Ok(())
    }

    fn visit_while_stmt(&mut self, cond: &Expr, body: &Stmt, _loc: Loc) -> ExecuteRes {
        while self.evaluate(cond)?.is_truthy() {
            match self.execute(body) {
                Ok(()) => (),
                Err(RuntimeInterrupt::Break) => break,
                Err(interrupt) => {
                    return Err(interrupt);
                }
            }
        }

        Ok(())
    }

    fn visit_var_stmt(&mut self, name: &str, init: &Option<Expr>, _loc: Loc) -> ExecuteRes {
        let init_val = if let Some(expr) = init {
            self.evaluate(expr)?
        } else {
            Value::Nil
        };

        self.env.borrow_mut().define(name, init_val);
        Ok(())
    }

    fn visit_block_stmt(&mut self, stmts: &[Stmt], _loc: Loc) -> ExecuteRes {
        self.execute_block(stmts, Environ::with_enclosing(&self.env))
    }

    fn visit_function_stmt(
        &mut self,
        name: &str,
        params: &[Param],
        body: &[Stmt],
        _loc: Loc,
    ) -> ExecuteRes {
        let params: Vec<_> = params.iter().map(|p| p.kind.clone()).collect();
        let function = Function::new(name, params, body, &self.env);
        self.env.borrow_mut().define(name, function.into());
        Ok(())
    }

    fn visit_return_stmt(&mut self, ret: &Option<Expr>, _loc: Loc) -> ExecuteRes {
        let ret_val = if let Some(expr) = ret {
            self.evaluate(expr)?
        } else {
            Value::Nil
        };

        Err(RuntimeInterrupt::Return(ret_val))
    }

    fn visit_break_stmt(&mut self, _loc: Loc) -> ExecuteRes {
        Err(RuntimeInterrupt::Break)
    }
}

macro_rules! arithmethic_operation {
    ($op:tt, $lhs:ident, $rhs:ident, $loc:ident) => {
        use Value::*;
        return match ($lhs, $rhs) {
            (Integer(left), Integer(right)) => Ok(Integer(left $op right)),
            (Integer(left), Float(right)) => Ok(Float((left as f64) $op right)),
            (Float(left), Integer(right)) => Ok(Float(left $op (right as f64))),
            (Float(left), Float(right)) => Ok(Float(left $op right)),
            (left, right) => Err(RuntimeError::unsupported_operands(
                $loc,
                stringify!($op),
                &left,
                &right,
            )),
        }
    };
}

macro_rules! comparison_operation {
    ($op:tt, $lhs:ident, $rhs:ident, $loc:ident) => {
        use Value::*;
        return match ($lhs, $rhs) {
            (Integer(left), Integer(right)) => Ok(Boolean(left $op right)),
            (Integer(left), Float(right)) => Ok(Boolean((*left as f64) $op *right)),
            (Float(left), Integer(right)) => Ok(Boolean(*left $op (*right as f64))),
            (Float(left), Float(right)) => Ok(Boolean(left $op right)),
            (left, right) => Err(RuntimeError::unsupported_operands(
                $loc,
                stringify!($op),
                left,
                right,
            )),
        }
    };
}

impl Value {
    fn not(self) -> Self {
        Value::Boolean(!self.is_truthy())
    }

    fn negate(self, loc: Loc) -> ValueRes {
        match self {
            Value::Integer(int) => Ok(Value::Integer(-int)),
            Value::Float(float) => Ok(Value::Float(-float)),
            value => Err(RuntimeError::unsupported_operand(loc, "-", value)),
        }
    }

    fn add(self, rhs: Value, loc: Loc) -> ValueRes {
        use Value::*;
        match (self, rhs) {
            (Integer(left), Integer(right)) => Ok(Integer(left + right)),
            (Integer(left), Float(right)) => Ok(Float((left as f64) + right)),
            (Float(left), Integer(right)) => Ok(Float(left + (right as f64))),
            (Float(left), Float(right)) => Ok(Float(left + right)),
            (Str(left), Str(right)) => Ok(Str(format!("{}{}", left, right))),
            (left, right) => Err(RuntimeError::unsupported_operands(loc, "+", &left, &right)),
        }
    }

    fn sub(self, rhs: Value, loc: Loc) -> ValueRes {
        arithmethic_operation!(-, self, rhs, loc);
    }

    fn mul(self, rhs: Value, loc: Loc) -> ValueRes {
        arithmethic_operation!(*, self, rhs, loc);
    }

    fn div(self, rhs: Value, loc: Loc) -> ValueRes {
        if rhs.number() == Some(0.0) {
            return Err(RuntimeError::DivisionByZero(loc));
        }

        arithmethic_operation!(/, self, rhs, loc);
    }

    fn rem(self, rhs: Value, loc: Loc) -> ValueRes {
        if rhs.number() == Some(0.0) {
            return Err(RuntimeError::DivisionByZero(loc));
        }

        arithmethic_operation!(%, self, rhs, loc);
    }

    fn equal(&self, rhs: &Value) -> Value {
        use std::f64::EPSILON;
        use Value::*;
        let eq = |l: f64, r: f64| (l - r).abs() <= EPSILON * 64.0;

        Boolean(match (self, rhs) {
            (Integer(left), Integer(right)) => left == right,
            (Integer(left), Float(right)) => eq(*left as f64, *right),
            (Float(left), Integer(right)) => eq(*left, *right as f64),
            (Float(left), Float(right)) => eq(*left, *right),
            (Str(left), Str(right)) => left == right,
            (Boolean(left), Boolean(right)) => left == right,
            (Nil, Nil) => true,
            (Callable(left), Callable(right)) => left == right,
            (_, _) => false,
        })
    }

    fn not_eq(&self, rhs: &Value) -> Value {
        self.equal(rhs).not()
    }

    fn greater(&self, rhs: &Value, loc: Loc) -> ValueRes {
        comparison_operation!(>, self, rhs, loc);
    }

    fn greater_eq(&self, rhs: &Value, loc: Loc) -> ValueRes {
        comparison_operation!(>=, self, rhs, loc);
    }

    fn less(&self, rhs: &Value, loc: Loc) -> ValueRes {
        comparison_operation!(<, self, rhs, loc);
    }

    fn less_eq(&self, rhs: &Value, loc: Loc) -> ValueRes {
        comparison_operation!(<=, self, rhs, loc);
    }
}

impl RuntimeError {
    fn unsupported_operand(loc: Loc, op: &str, val: Value) -> Self {
        Self::UnsupportedOperand(loc, String::from(op), String::from(val.get_type()))
    }

    fn unsupported_operands(loc: Loc, op: &str, left: &Value, right: &Value) -> Self {
        Self::UnsupportedOperands(
            loc,
            String::from(op),
            String::from(left.get_type()),
            String::from(right.get_type()),
        )
    }

    fn undefined_variable(loc: Loc, name: &str) -> Self {
        Self::UndefinedVariable(loc, String::from(name))
    }

    fn not_a_callable(loc: Loc, callee: Value) -> Self {
        Self::NotACallable(loc, String::from(callee.get_type()))
    }

    fn mismatching_arity(loc: Loc, expected: usize, got: usize) -> Self {
        Self::MismatchingArity(loc, expected, got)
    }
}

impl RuntimeInterrupt {
    pub fn expect_error(self) -> RuntimeError {
        use RuntimeInterrupt::*;
        match self {
            Error(error) => error,
            interrupt => panic!(
                "Expected interrupt to be an error at this point. Got: {:?}",
                interrupt
            ),
        }
    }
}

impl From<RuntimeError> for RuntimeInterrupt {
    fn from(error: RuntimeError) -> Self {
        Self::Error(error)
    }
}
