use crate::array::{Array, ArrayRc};
use crate::eval::{RuntimeError, ValueRes};
use crate::location::Loc;
use crate::value::Value;

#[derive(Clone, Debug)]
pub enum Scriptable {
    Array(ArrayRc),
}

pub trait LoxScriptable {
    fn subscript_get(&self, index: Value, loc: Loc) -> ValueRes;
    fn subscript_set(&mut self, index: Value, val: Value, loc: Loc) -> ValueRes;
}

impl LoxScriptable for Scriptable {
    fn subscript_get(&self, index: Value, loc: Loc) -> ValueRes {
        match self {
            Scriptable::Array(array) => array.borrow().subscript_get(index, loc),
        }
    }

    fn subscript_set(&mut self, index: Value, val: Value, loc: Loc) -> ValueRes {
        match self {
            Scriptable::Array(array) => array.borrow_mut().subscript_set(index, val, loc),
        }
    }
}

impl LoxScriptable for Array {
    fn subscript_get(&self, index: Value, loc: Loc) -> ValueRes {
        if let Value::Integer(index) = index {
            self.get(index, loc)
        } else {
            Err(RuntimeError::array_index_not_int(loc, index))
        }
    }

    fn subscript_set(&mut self, index: Value, val: Value, loc: Loc) -> ValueRes {
        if let Value::Integer(index) = index {
            self.set(index, val.clone(), loc)?;
            Ok(val)
        } else {
            Err(RuntimeError::array_index_not_int(loc, index))
        }
    }
}
