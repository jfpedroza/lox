use crate::value::Value;
use std::cell::RefCell;
use std::fmt::{Display, Formatter, Result as FmtResult};
use std::rc::Rc;
use std::slice::Iter;

#[derive(Debug)]
pub struct Array(Vec<Value>);

pub type ArrayRc = Rc<RefCell<Array>>;

impl Array {
    pub fn new(elements: Vec<Value>) -> Self {
        Self(elements)
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn iter(&self) -> Iter<Value> {
        self.0.iter()
    }
}

impl Display for Array {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        let len = self.0.len();
        write!(f, "[")?;
        for (i, el) in self.0.iter().enumerate() {
            if i < len - 1 {
                write!(f, "{}, ", el)?;
            } else {
                write!(f, "{}", el)?;
            }
        }

        write!(f, "]")
    }
}

pub mod functions {
    use crate::eval::{Interpreter, RuntimeError, ValueRes};
    use crate::location::Loc;
    use crate::value::{types::ARRAY, Value};

    pub fn array_len(_inter: &mut Interpreter, mut args: Vec<Value>, loc: Loc) -> ValueRes {
        let arg = args.pop().unwrap();
        if let Value::Array(array) = arg {
            let len = array.borrow().len();
            Ok(Value::Integer(len as i64))
        } else {
            Err(RuntimeError::expected_type(loc, ARRAY, arg))
        }
    }
}
