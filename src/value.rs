use crate::callable::{Callable, Class, ClassInstance, InstanceRc, LoxCallable};
use crate::expr::LitExpr;
use std::cell::RefCell;
use std::fmt::{Display, Formatter, Result as FmtResult};
use std::rc::Rc;

#[derive(Clone, Debug)]
pub enum Value {
    Integer(i64),
    Float(f64),
    Str(String),
    Boolean(bool),
    Nil,
    Callable(Callable),
    Instance(InstanceRc),
}

pub mod types {
    pub const INT: &str = "int";
    pub const FLOAT: &str = "float";
    pub const STRING: &str = "string";
    pub const BOOL: &str = "bool";
    pub const NIL: &str = "nil";
    pub const INSTANCE: &str = "instance";
}

impl Value {
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
            Callable(callable) => callable.get_type(),
            Instance(_) => types::INSTANCE,
        }
    }

    pub fn into_instance(self) -> Option<InstanceRc> {
        use Value::*;
        match self {
            Instance(instance) => Some(instance),
            Callable(callable) => callable.into_instance(),
            _ => None,
        }
    }

    pub fn into_class(self) -> Option<Rc<Class>> {
        match self {
            Value::Callable(Callable::Class(class)) => Some(class),
            _ => None,
        }
    }
}

impl From<&LitExpr> for Value {
    fn from(literal: &LitExpr) -> Self {
        use LitExpr::*;
        match literal {
            Integer(int) => Value::Integer(*int),
            Float(float) => Value::Float(*float),
            Str(string) => Value::Str(string.clone()),
            Boolean(boolean) => Value::Boolean(*boolean),
            Nil => Value::Nil,
        }
    }
}

impl From<i64> for Value {
    fn from(input: i64) -> Self {
        Value::Integer(input)
    }
}

impl From<f64> for Value {
    fn from(input: f64) -> Self {
        Value::Float(input)
    }
}

impl From<String> for Value {
    fn from(input: String) -> Self {
        Value::Str(input)
    }
}

impl From<&str> for Value {
    fn from(input: &str) -> Self {
        Value::Str(String::from(input))
    }
}

impl From<bool> for Value {
    fn from(input: bool) -> Self {
        Value::Boolean(input)
    }
}

impl<T: LoxCallable> From<T> for Value {
    fn from(input: T) -> Self {
        Value::Callable(input.into())
    }
}

impl From<ClassInstance> for Value {
    fn from(input: ClassInstance) -> Self {
        Value::Instance(Rc::new(RefCell::new(input)))
    }
}

impl<T: Into<Value>> From<Option<T>> for Value {
    fn from(input: Option<T>) -> Self {
        match input {
            Some(input) => input.into(),
            None => Value::Nil,
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
            Callable(callable) => callable.fmt(f),
            Instance(instance) => instance.borrow().fmt(f),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        use Value::*;
        match (self, other) {
            (Integer(left), Integer(right)) => left == right,
            (Float(left), Float(right)) => left == right,
            (Str(left), Str(right)) => left == right,
            (Boolean(left), Boolean(right)) => left == right,
            (Nil, Nil) => true,
            (Callable(left), Callable(right)) => left == right,
            (Instance(left), Instance(right)) => Rc::ptr_eq(left, right),
            (_, _) => false,
        }
    }
}
