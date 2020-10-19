use crate::callable::{Callable, LoxCallable};
use crate::class::{Class, InstanceRc, LoxClass, MethodMap, NativeMethod, NativeMethodFn};
use crate::constants::ARRAY_CLASS;
use crate::eval::{Interpreter, RuntimeError, ValueRes};
use crate::location::Loc;
use crate::value::{types::INT, Value};
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Display, Formatter, Result as FmtResult};
use std::rc::Rc;
use std::slice::Iter;

#[derive(Debug)]
pub struct Array {
    class: Rc<ArrayClass>,
    data: Vec<Value>,
}

pub type ArrayRc = Rc<RefCell<Array>>;

impl Array {
    pub fn new(inter: &Interpreter, data: Vec<Value>) -> Self {
        let class = Rc::clone(&inter.natives.array_class);
        Self { class, data }
    }

    pub fn class(&self) -> Rc<ArrayClass> {
        Rc::clone(&self.class)
    }

    pub fn class_name(&self) -> &'static str {
        ARRAY_CLASS
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }

    pub fn iter(&self) -> Iter<Value> {
        self.data.iter()
    }

    pub fn push(&mut self, val: Value) {
        self.data.push(val);
    }

    pub fn pop(&mut self) -> ValueRes {
        Ok(self.data.pop().unwrap())
    }

    pub fn get(&self, index: i64, loc: Loc) -> ValueRes {
        let len = self.len();
        if index >= 0 && (index as usize) < len {
            Ok(self.data[index as usize].clone())
        } else {
            Err(RuntimeError::IndexOutOfBounds(loc, index, len))
        }
    }

    pub fn set(&mut self, index: i64, val: Value, loc: Loc) -> Result<(), RuntimeError> {
        let len = self.len();
        if index >= 0 && (index as usize) < len {
            self.data[index as usize] = val;
            Ok(())
        } else {
            Err(RuntimeError::IndexOutOfBounds(loc, index, len))
        }
    }
}

impl Display for Array {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        let len = self.data.len();
        write!(f, "[")?;
        for (i, el) in self.data.iter().enumerate() {
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

#[derive(Debug)]
pub struct ArrayClass {
    methods: MethodMap,
    getters: MethodMap,
    metainstance: InstanceRc,
}

impl ArrayClass {
    pub fn new() -> Self {
        let mut static_methods = MethodMap::new();
        Self::add_new_smethod(&mut static_methods);
        let metainstance = Class::create_metainstance(ARRAY_CLASS, None, static_methods);

        let mut class = Self {
            methods: HashMap::new(),
            getters: HashMap::new(),
            metainstance,
        };

        class.add_length_getter();
        class.add_push_method();
        class.add_pop_method();
        class.add_get_method();
        class.add_set_method();
        class
    }

    fn add_method(&mut self, name: &'static str, arity: usize, fun: NativeMethodFn) {
        Class::add_native_method(&mut self.methods, NativeMethod { name, arity, fun })
    }

    fn add_getter(&mut self, name: &'static str, arity: usize, fun: NativeMethodFn) {
        Class::add_native_method(&mut self.getters, NativeMethod { name, arity, fun })
    }

    fn add_length_getter(&mut self) {
        self.add_getter("length", 0, |_inter, _args, instance, _loc| {
            let array = instance.array().unwrap();
            let len = array.borrow().len();
            Ok(Value::Integer(len as i64))
        })
    }

    fn add_push_method(&mut self) {
        self.add_method("push", 1, |_inter, mut args: Vec<Value>, instance, _loc| {
            let array = instance.array().unwrap();
            let val = args.pop().unwrap();
            array.borrow_mut().push(val);
            Ok(Value::Nil)
        })
    }

    fn add_pop_method(&mut self) {
        self.add_method("pop", 0, |_inter, _args, instance, _loc| {
            let array = instance.array().unwrap();
            let val = array.borrow_mut().pop()?;
            Ok(val)
        })
    }

    fn add_get_method(&mut self) {
        self.add_method("get", 1, |_inter, mut args: Vec<Value>, instance, loc| {
            let array = instance.array().unwrap();
            let index = args.pop().unwrap();
            if let Value::Integer(index) = index {
                array.borrow().get(index, loc)
            } else {
                Err(RuntimeError::expected_type(loc, INT, index))
            }
        })
    }

    fn add_set_method(&mut self) {
        self.add_method("set", 2, |_inter, mut args: Vec<Value>, instance, loc| {
            let array = instance.array().unwrap();
            let val = args.pop().unwrap();
            let index = args.pop().unwrap();
            if let Value::Integer(index) = index {
                array.borrow_mut().set(index, val.clone(), loc)?;
                Ok(val)
            } else {
                Err(RuntimeError::expected_type(loc, INT, index))
            }
        })
    }

    fn add_new_smethod(static_methods: &mut MethodMap) {
        Class::add_native_method(
            static_methods,
            NativeMethod {
                name: "new",
                arity: 0,
                fun: |inter, _args, _instance, _loc| Ok(Array::new(inter, vec![]).into()),
            },
        )
    }
}

impl LoxClass for Rc<ArrayClass> {
    fn name(&self) -> &str {
        ARRAY_CLASS
    }

    fn superclass(&self) -> Option<&Class> {
        None
    }

    fn methods(&self) -> &MethodMap {
        &self.methods
    }

    fn getters(&self) -> &MethodMap {
        &self.getters
    }

    fn metainstance(&self) -> Option<InstanceRc> {
        Some(Rc::clone(&self.metainstance))
    }
}

impl LoxCallable for Rc<ArrayClass> {
    fn arity(&self) -> usize {
        2
    }

    fn call(&self, inter: &mut Interpreter, mut args: Vec<Value>, loc: Loc) -> ValueRes {
        let val = args.pop().unwrap();
        let size = args.pop().unwrap();

        match size {
            Value::Integer(size) if size >= 0 => {
                Ok(Array::new(inter, vec![val; size as usize]).into())
            }
            Value::Integer(_) => Err(RuntimeError::generic(
                loc,
                "Negative value given for the size",
            )),
            size => Err(RuntimeError::expected_type(loc, INT, size)),
        }
    }
}

impl From<Rc<ArrayClass>> for Callable {
    fn from(class: Rc<ArrayClass>) -> Self {
        Callable::Class(Class::Array(class))
    }
}

impl From<Rc<ArrayClass>> for Class {
    fn from(class: Rc<ArrayClass>) -> Self {
        Class::Array(class)
    }
}

impl Display for ArrayClass {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "<class {}>", ARRAY_CLASS)
    }
}
