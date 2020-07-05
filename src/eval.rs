#[cfg(test)]
mod tests;

use crate::expr::{BinOp, Expr, LitExpr, UnOp, Visitor as ExprVisitor};
use crate::location::Loc;
use crate::stmt::{Stmt, Visitor as StmtVisitor};
use crate::value::Value;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub struct Interpreter {
    env: Env,
}

struct Environ {
    values: HashMap<String, Value>,
    enclosing: Option<Env>,
}

type Env = Rc<RefCell<Environ>>;

#[derive(Debug, PartialEq, Fail)]
pub enum RuntimeError {
    #[fail(display = "[{}] Unsupported operand for {}: '{}'", _0, _1, 2)]
    UnsupportedOperand(Loc, String, String),
    #[fail(
        display = "[{}] Unsupported operands for {}: '{}' and '{}'",
        _0, _1, 2, _3
    )]
    UnsupportedOperands(Loc, String, String, String),
    #[fail(display = "[{}] Division or modulo by zero", _0)]
    DivisionByZero(Loc),
    #[fail(display = "[{}] Undefined variable '{}'", _0, _1)]
    UndefinedVariable(Loc, String),
}

type ValueRes = Result<Value, RuntimeError>;
type ExecuteRes = Result<(), RuntimeError>;

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            env: Environ::new().into(),
        }
    }

    pub fn interpret(&mut self, stmts: &[Stmt]) -> ExecuteRes {
        for stmt in stmts {
            self.execute(stmt)?;
        }

        Ok(())
    }

    pub fn evaluate(&mut self, expr: &Expr) -> ValueRes {
        expr.accept(self)
    }

    pub fn execute(&mut self, stmt: &Stmt) -> ExecuteRes {
        stmt.accept(self)
    }

    fn execute_block(&mut self, stmts: &[Stmt], env: Env) -> ExecuteRes {
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
        self.env.borrow().get(&name, loc)
    }

    fn visit_assign_expr(&mut self, name: &str, expr: &Expr, loc: Loc) -> ValueRes {
        let val = self.evaluate(expr)?;
        let cloned_val = val.clone();
        self.env.borrow_mut().assign(name, val, loc)?;
        Ok(cloned_val)
    }
}

impl StmtVisitor<()> for Interpreter {
    type Error = RuntimeError;

    fn visit_expression_stmt(&mut self, expr: &Expr, _loc: Loc) -> ExecuteRes {
        self.evaluate(expr)?;
        Ok(())
    }

    fn visit_print_stmt(&mut self, expr: &Expr, _loc: Loc) -> ExecuteRes {
        let val = self.evaluate(expr)?;
        println!("{}", val);
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
}
