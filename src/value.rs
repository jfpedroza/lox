use crate::expr::LitExpr;
use std::fmt::{Display, Formatter, Result as FmtResult};

#[derive(PartialEq, Clone)]
pub enum Value {
    Integer(i64),
    Float(f64),
    Str(String),
    Boolean(bool),
    Nil,
}

pub mod types {
    pub const INT: &str = "int";
    pub const FLOAT: &str = "float";
    pub const STRING: &str = "string";
    pub const BOOL: &str = "bool";
    pub const NIL: &str = "nil";
}

impl Value {
    pub fn from_literal(literal: &LitExpr) -> Self {
        use LitExpr::*;
        match literal {
            Integer(int) => Value::Integer(*int),
            Float(float) => Value::Float(*float),
            Str(string) => Value::Str(string.clone()),
            Boolean(boolean) => Value::Boolean(*boolean),
            Nil => Value::Nil,
        }
    }

    pub fn is_truthy(&self) -> bool {
        use Value::*;
        match self {
            Nil | Boolean(false) => false,
            _ => true,
        }
    }

    pub fn number(&self) -> Option<f64> {
        match self {
            Value::Integer(int) => Some(*int as f64),
            Value::Float(float) => Some(*float),
            _ => None,
        }
    }

    pub fn get_type(&self) -> &'static str {
        use Value::*;
        match self {
            Integer(_) => types::INT,
            Float(_) => types::FLOAT,
            Str(_) => types::STRING,
            Boolean(_) => types::BOOL,
            Nil => types::NIL,
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        use Value::*;
        match self {
            Integer(int) => write!(f, "{}", int),
            Float(float) => write!(f, "{}", float),
            Str(string) => write!(f, "{}", string),
            Boolean(boolean) => write!(f, "{}", boolean),
            Nil => write!(f, "nil"),
        }
    }
}
