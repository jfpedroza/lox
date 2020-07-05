#[cfg(test)]
mod tests;

use crate::expr::{BinOp, Expr, ExprKind, UnOp};
use crate::location::Loc;
use crate::stmt::{Stmt, StmtKind};
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

trait Evaluable<Res> {
    type Error;

    fn evaluate(self, inter: &mut Interpreter) -> Result<Res, Self::Error>;
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            env: Environ::new().to_rc(),
        }
    }

    pub fn interpret(&mut self, stmts: Vec<Stmt>) -> ExecuteRes {
        for stmt in stmts {
            self.execute(stmt)?;
        }

        Ok(())
    }

    pub fn evaluate<T: Into<Expr>>(&mut self, expr: T) -> ValueRes {
        expr.into().evaluate(self)
    }

    pub fn execute<T: Into<Stmt>>(&mut self, stmt: T) -> ExecuteRes {
        stmt.into().evaluate(self)
    }

    fn execute_block(&mut self, stmts: Vec<Stmt>, env: Env) -> ExecuteRes {
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

        env.to_rc()
    }

    pub fn to_rc(self) -> Env {
        Rc::new(RefCell::new(self))
    }

    pub fn define(&mut self, name: String, val: Value) {
        self.values.insert(name, val);
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

    pub fn assign(&mut self, name: String, val: Value, loc: Loc) -> Result<(), RuntimeError> {
        if self.values.contains_key(&name) {
            self.values.insert(name, val);

            Ok(())
        } else if let Some(ref env) = self.enclosing {
            env.borrow_mut().assign(name, val, loc)
        } else {
            Err(RuntimeError::undefined_variable(loc, &name))
        }
    }
}

impl Evaluable<Value> for Expr {
    type Error = RuntimeError;

    fn evaluate(self, inter: &mut Interpreter) -> ValueRes {
        use ExprKind::*;
        Ok(match self.kind {
            Literal(literal) => literal.into(),
            Grouping(expr) => inter.evaluate(expr)?,
            Unary(op, expr) => {
                let val = inter.evaluate(expr)?;

                match op {
                    UnOp::Negate => val.negate(self.loc)?,
                    UnOp::Not => val.not(),
                }
            }
            Binary(left, op, right) => {
                let left_val = inter.evaluate(left)?;
                let right_val = inter.evaluate(right)?;

                match op {
                    BinOp::Add => left_val.add(right_val, self.loc)?,
                    BinOp::Sub => left_val.sub(right_val, self.loc)?,
                    BinOp::Mul => left_val.mul(right_val, self.loc)?,
                    BinOp::Div => left_val.div(right_val, self.loc)?,
                    BinOp::Rem => left_val.rem(right_val, self.loc)?,
                    BinOp::Equal => left_val.equal(&right_val),
                    BinOp::NotEqual => left_val.not_eq(&right_val),
                    BinOp::Greater => left_val.greater(&right_val, self.loc)?,
                    BinOp::GreaterEqual => left_val.greater_eq(&right_val, self.loc)?,
                    BinOp::Less => left_val.less(&right_val, self.loc)?,
                    BinOp::LessEqual => left_val.less_eq(&right_val, self.loc)?,
                }
            }
            Comma(left, right) => {
                let _ = inter.evaluate(left)?;
                inter.evaluate(right)?
            }
            Conditional(cond, left, right) => {
                let cond_val = inter.evaluate(cond)?;
                if cond_val.is_truthy() {
                    inter.evaluate(left)?
                } else {
                    inter.evaluate(right)?
                }
            }
            Variable(name) => inter.env.borrow().get(&name, self.loc)?,
            Assign(name, expr) => {
                let val = inter.evaluate(expr)?;
                let cloned_val = val.clone();
                inter.env.borrow_mut().assign(name, val, self.loc)?;
                cloned_val
            }
        })
    }
}

impl Evaluable<()> for Stmt {
    type Error = RuntimeError;

    fn evaluate(self, inter: &mut Interpreter) -> Result<(), Self::Error> {
        use StmtKind::*;
        Ok(match self.kind {
            Expression(expr) => {
                inter.evaluate(expr)?;
            }
            Print(expr) => {
                let val = inter.evaluate(expr)?;
                println!("{}", val);
            }
            Var(name, init) => {
                let init_val = if let Some(expr) = init {
                    inter.evaluate(expr)?
                } else {
                    Value::Nil
                };

                inter.env.borrow_mut().define(name, init_val);
            }
            Block(stmts) => {
                inter.execute_block(stmts, Environ::with_enclosing(&inter.env))?;
            }
        })
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
