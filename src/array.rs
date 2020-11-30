use crate::callable::{Callable, LoxCallable};
use crate::class::{Class, InstanceRc, LoxClass, MethodMap, NativeMethod};
use crate::constants::ARRAY_CLASS;
use crate::eval::{Interpreter, RuntimeError, ValueRes};
use crate::location::Loc;
use crate::value::{types::INT, Value};
use std::cell::RefCell;
use std::fmt::{Display, Formatter, Result as FmtResult};
use std::rc::Rc;
use std::slice::Iter;
use uuid::Uuid;

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

#[derive(Debug)]
pub struct ArrayClass {
    uuid: Uuid,
    methods: MethodMap,
    getters: MethodMap,
    metainstance: InstanceRc,
}

impl ArrayClass {
    pub fn new() -> Self {
        let uuid = Uuid::new_v4();
        let mut static_methods = MethodMap::new();
        Self::add_new_smethod(&mut static_methods);

        let mut getters = MethodMap::new();
        Self::add_length_getter(&mut getters);
        let mut methods = MethodMap::new();
        Self::add_push_method(&mut methods);
        Self::add_pop_method(&mut methods);
        Self::add_get_method(&mut methods);
        Self::add_set_method(&mut methods);

        let metainstance =
            Class::create_metainstance(ARRAY_CLASS, uuid, None, static_methods, &methods, &getters);

        Self {
            uuid,
            methods,
            getters,
            metainstance,
        }
    }

    fn add_length_getter(getters: &mut MethodMap) {
        Class::add_native_method(
            getters,
            NativeMethod::new("length", 0, |_inter, _args, instance, _loc| {
                let array = instance.array().unwrap();
                let len = array.borrow().len();
                Ok(Value::Integer(len as i64))
            }),
        )
    }

    fn add_push_method(methods: &mut MethodMap) {
        Class::add_native_method(
            methods,
            NativeMethod::new("push", 1, |_inter, mut args: Vec<Value>, instance, _loc| {
                let array = instance.array().unwrap();
                let val = args.pop().unwrap();
                array.borrow_mut().push(val);
                Ok(Value::Nil)
            }),
        );
    }

    fn add_pop_method(methods: &mut MethodMap) {
        Class::add_native_method(
            methods,
            NativeMethod::new("pop", 0, |_inter, _args, instance, _loc| {
                let array = instance.array().unwrap();
                let val = array.borrow_mut().pop()?;
                Ok(val)
            }),
        )
    }

    fn add_get_method(methods: &mut MethodMap) {
        Class::add_native_method(
            methods,
            NativeMethod::new("get", 1, |_inter, mut args: Vec<Value>, instance, loc| {
                let array = instance.array().unwrap();
                let index = args.pop().unwrap();
                if let Value::Integer(index) = index {
                    array.borrow().get(index, loc)
                } else {
                    Err(RuntimeError::expected_type(loc, INT, index))
                }
            }),
        )
    }

    fn add_set_method(methods: &mut MethodMap) {
        Class::add_native_method(
            methods,
            NativeMethod::new("set", 2, |_inter, mut args: Vec<Value>, instance, loc| {
                let array = instance.array().unwrap();
                let val = args.pop().unwrap();
                let index = args.pop().unwrap();
                if let Value::Integer(index) = index {
                    array.borrow_mut().set(index, val.clone(), loc)?;
                    Ok(val)
                } else {
                    Err(RuntimeError::expected_type(loc, INT, index))
                }
            }),
        )
    }

    fn add_new_smethod(static_methods: &mut MethodMap) {
        Class::add_native_method(
            static_methods,
            NativeMethod::new("new", 0, |inter, _args, _instance, _loc| {
                Ok(Array::new(inter, vec![]).into())
            }),
        )
    }
}

impl LoxClass for Rc<ArrayClass> {
    fn name(&self) -> &str {
        ARRAY_CLASS
    }

    fn uuid(&self) -> Uuid {
        self.uuid
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
