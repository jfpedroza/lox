use crate::eval::{RuntimeError, ValueRes};
use crate::location::Loc;
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

    pub fn push(&mut self, val: Value) {
        self.0.push(val);
    }

    pub fn get(&self, index: i64, loc: Loc) -> ValueRes {
        let len = self.len();
        if index >= 0 && (index as usize) < len {
            Ok(self.0[index as usize].clone())
        } else {
            Err(RuntimeError::IndexOutOfBounds(loc, index, len))
        }
    }

    pub fn set(&mut self, index: i64, val: Value, loc: Loc) -> Result<(), RuntimeError> {
        let len = self.len();
        if index >= 0 && (index as usize) < len {
            self.0[index as usize] = val;
            Ok(())
        } else {
            Err(RuntimeError::IndexOutOfBounds(loc, index, len))
        }
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
    use super::*;
    use crate::eval::Interpreter;
    use crate::value::types::{ARRAY, INT};

    fn validate_array(val: Value, loc: Loc) -> Result<ArrayRc, RuntimeError> {
        if let Value::Array(array) = val {
            Ok(array)
        } else {
            Err(RuntimeError::expected_type(loc, ARRAY, val))
        }
    }

    pub fn array_len(_inter: &mut Interpreter, mut args: Vec<Value>, loc: Loc) -> ValueRes {
        let arg = args.pop().unwrap();
        let array = validate_array(arg, loc)?;
        let len = array.borrow().len();
        Ok(Value::Integer(len as i64))
    }

    pub fn array_push(_inter: &mut Interpreter, mut args: Vec<Value>, loc: Loc) -> ValueRes {
        let val = args.pop().unwrap();
        let arg = args.pop().unwrap();
        let array = validate_array(arg, loc)?;
        array.borrow_mut().push(val);
        Ok(Value::Nil)
    }

    pub fn array_get(_inter: &mut Interpreter, mut args: Vec<Value>, loc: Loc) -> ValueRes {
        let index = args.pop().unwrap();
        let arg = args.pop().unwrap();
        let array = validate_array(arg, loc)?;
        if let Value::Integer(index) = index {
            array.borrow().get(index, loc)
        } else {
            Err(RuntimeError::expected_type(loc, INT, index))
        }
    }

    pub fn array_set(_inter: &mut Interpreter, mut args: Vec<Value>, loc: Loc) -> ValueRes {
        let val = args.pop().unwrap();
        let index = args.pop().unwrap();
        let arg = args.pop().unwrap();
        let array = validate_array(arg, loc)?;
        if let Value::Integer(index) = index {
            array.borrow_mut().set(index, val.clone(), loc)?;
            Ok(val)
        } else {
            Err(RuntimeError::expected_type(loc, INT, index))
        }
    }
}
