use crate::expr::{BinaryOp, Expr, ExprKind, UnaryOp};
use crate::location::Location;
use crate::value::Value;

pub struct Interpreter;

#[derive(Debug, PartialEq, Fail)]
pub enum RuntimeError {
    #[fail(display = "[{}] Unsupported operand for {}: '{}'", _0, _1, 2)]
    UnsupportedOperand(Location, String, String),
    #[fail(
        display = "[{}] Unsupported operands for {}: '{}' and '{}'",
        _0, _1, 2, _3
    )]
    UnsupportedOperands(Location, String, String, String),
    #[fail(display = "[{}] Division or modulo by zero", _0)]
    DivisionByZero(Location),
}

type ValueRes = Result<Value, RuntimeError>;

trait Evaluable<Res> {
    type Error;

    fn evaluate(&self, inter: &mut Interpreter) -> Result<Res, Self::Error>;
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter
    }

    pub fn interpret(&mut self, expr: &Expr) -> Result<(), RuntimeError> {
        let val = expr.evaluate(self)?;

        println!("{}", val);
        Ok(())
    }
}

impl Evaluable<Value> for Expr {
    type Error = RuntimeError;

    fn evaluate(&self, inter: &mut Interpreter) -> Result<Value, Self::Error> {
        use ExprKind::*;
        Ok(match &self.kind {
            Literal(literal) => Value::from_literal(literal),
            Grouping(expr) => expr.evaluate(inter)?,
            Unary(op, expr) => {
                let val = expr.evaluate(inter)?;

                match op {
                    UnaryOp::Negate => val.negate(self.loc)?,
                    UnaryOp::Not => val.not(),
                }
            }
            Binary(left, op, right) => {
                let left_val = left.evaluate(inter)?;
                let right_val = right.evaluate(inter)?;

                match op {
                    BinaryOp::Add => left_val.add(right_val, self.loc)?,
                    BinaryOp::Sub => left_val.sub(right_val, self.loc)?,
                    BinaryOp::Mul => left_val.mul(right_val, self.loc)?,
                    BinaryOp::Div => left_val.div(right_val, self.loc)?,
                    BinaryOp::Rem => left_val.rem(right_val, self.loc)?,
                    BinaryOp::Equal => left_val.equal(right_val),
                    BinaryOp::NotEqual => left_val.not_eq(right_val),
                    BinaryOp::Greater => left_val.greater(right_val, self.loc)?,
                    BinaryOp::GreaterEqual => left_val.greater_eq(right_val, self.loc)?,
                    BinaryOp::Less => left_val.less(right_val, self.loc)?,
                    BinaryOp::LessEqual => left_val.less_eq(right_val, self.loc)?,
                }
            }
            Comma(left, right) => {
                let _ = left.evaluate(inter)?;
                right.evaluate(inter)?
            }
            Conditional(cond, left, right) => {
                let cond_val = cond.evaluate(inter)?;
                if cond_val.is_truthy() {
                    left.evaluate(inter)?
                } else {
                    right.evaluate(inter)?
                }
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
                left,
                right,
            )),
        }
    };
}

macro_rules! comparison_operation {
    ($op:tt, $lhs:ident, $rhs:ident, $loc:ident) => {
        use Value::*;
        return match ($lhs, $rhs) {
            (Integer(left), Integer(right)) => Ok(Boolean(left $op right)),
            (Integer(left), Float(right)) => Ok(Boolean((left as f64) $op right)),
            (Float(left), Integer(right)) => Ok(Boolean(left $op (right as f64))),
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

    fn negate(self, loc: Location) -> ValueRes {
        match self {
            Value::Integer(int) => Ok(Value::Integer(-int)),
            Value::Float(float) => Ok(Value::Float(-float)),
            value => Err(RuntimeError::unsupported_operand(loc, "-", value)),
        }
    }

    fn add(self, rhs: Value, loc: Location) -> ValueRes {
        use Value::*;
        match (self, rhs) {
            (Integer(left), Integer(right)) => Ok(Integer(left + right)),
            (Integer(left), Float(right)) => Ok(Float((left as f64) + right)),
            (Float(left), Integer(right)) => Ok(Float(left + (right as f64))),
            (Float(left), Float(right)) => Ok(Float(left + right)),
            (Str(left), Str(right)) => Ok(Str(format!("{}{}", left, right))),
            (left, right) => Err(RuntimeError::unsupported_operands(loc, "+", left, right)),
        }
    }

    fn sub(self, rhs: Value, loc: Location) -> ValueRes {
        arithmethic_operation!(-, self, rhs, loc);
    }

    fn mul(self, rhs: Value, loc: Location) -> ValueRes {
        arithmethic_operation!(*, self, rhs, loc);
    }

    fn div(self, rhs: Value, loc: Location) -> ValueRes {
        if rhs.number() == Some(0.0) {
            return Err(RuntimeError::DivisionByZero(loc));
        }

        arithmethic_operation!(/, self, rhs, loc);
    }

    fn rem(self, rhs: Value, loc: Location) -> ValueRes {
        if rhs.number() == Some(0.0) {
            return Err(RuntimeError::DivisionByZero(loc));
        }

        arithmethic_operation!(%, self, rhs, loc);
    }

    fn equal(self, rhs: Value) -> Value {
        use Value::*;
        Boolean(match (self, rhs) {
            (Integer(left), Integer(right)) => left == right,
            (Integer(left), Float(right)) => (left as f64) == right,
            (Float(left), Integer(right)) => left == (right as f64),
            (Float(left), Float(right)) => left == right,
            (Str(left), Str(right)) => left == right,
            (Boolean(left), Boolean(right)) => left == right,
            (Nil, Nil) => true,
            (_, _) => false,
        })
    }

    fn not_eq(self, rhs: Value) -> Value {
        self.equal(rhs).not()
    }

    fn greater(self, rhs: Value, loc: Location) -> ValueRes {
        comparison_operation!(>, self, rhs, loc);
    }

    fn greater_eq(self, rhs: Value, loc: Location) -> ValueRes {
        comparison_operation!(>=, self, rhs, loc);
    }

    fn less(self, rhs: Value, loc: Location) -> ValueRes {
        comparison_operation!(<, self, rhs, loc);
    }

    fn less_eq(self, rhs: Value, loc: Location) -> ValueRes {
        comparison_operation!(<=, self, rhs, loc);
    }
}

impl RuntimeError {
    fn unsupported_operand(loc: Location, op: &str, val: Value) -> Self {
        Self::UnsupportedOperand(loc, String::from(op), String::from(val.get_type()))
    }

    fn unsupported_operands(loc: Location, op: &str, left: Value, right: Value) -> Self {
        Self::UnsupportedOperands(
            loc,
            String::from(op),
            String::from(left.get_type()),
            String::from(right.get_type()),
        )
    }
}
