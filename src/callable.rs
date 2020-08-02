use crate::eval::{Env, Environ, GlobalEnviron, Interpreter, RuntimeInterrupt, ValueRes};
use crate::stmt::Stmt;
use crate::value::Value;
use std::collections::HashMap;
use std::fmt::{Display, Formatter, Result as FmtResult};
use std::rc::Rc;
use std::time::{SystemTime, UNIX_EPOCH};

#[derive(Clone, Debug)]
pub enum Callable {
    Native(NativeFunction),
    Function(Rc<Function>),
    Class(Rc<Class>),
}

#[derive(PartialEq, Copy, Clone, Debug)]
pub enum NativeFunction {
    Clock,
    Str,
}

#[derive(Debug)]
pub struct Function {
    pub name: Option<String>,
    params: Vec<String>,
    body: Vec<Stmt>,
    closure: Option<Env>,
}

#[derive(Debug)]
pub struct Class {
    name: String,
}

#[derive(Debug)]
pub struct ClassInstance {
    class: Rc<Class>,
    fields: HashMap<String, Value>,
}

pub mod types {
    pub const NATIVE: &str = "native_fn";
    pub const FUNCTION: &str = "function";
    pub const CLASS: &str = "class";
}

pub trait LoxCallable: Into<Callable> {
    fn arity(&self) -> usize;
    fn call(&self, inter: &mut Interpreter, args: Vec<Value>) -> ValueRes;
}

impl Callable {
    pub fn get_type(&self) -> &'static str {
        use Callable::*;
        match self {
            Native(_) => types::NATIVE,
            Function(_) => types::FUNCTION,
            Class(_) => types::CLASS,
        }
    }
}

impl Display for Callable {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        use Callable::*;
        match self {
            Native(function) => Display::fmt(function, f),
            Function(function) => Display::fmt(function, f),
            Class(class) => Display::fmt(class, f),
        }
    }
}

impl LoxCallable for Callable {
    fn arity(&self) -> usize {
        use Callable::*;
        match self {
            Native(function) => function.arity(),
            Function(function) => function.arity(),
            Class(class) => class.arity(),
        }
    }

    fn call(&self, inter: &mut Interpreter, args: Vec<Value>) -> ValueRes {
        use Callable::*;
        match self {
            Native(function) => function.call(inter, args),
            Function(function) => function.call(inter, args),
            Class(class) => class.call(inter, args),
        }
    }
}

impl PartialEq for Callable {
    fn eq(&self, other: &Self) -> bool {
        use Callable::*;
        match (self, other) {
            (Native(left), Native(right)) => left == right,
            (Function(left), Function(right)) => Rc::ptr_eq(left, right),
            (Class(left), Class(right)) => Rc::ptr_eq(left, right),
            (_, _) => false,
        }
    }
}

impl NativeFunction {
    fn name(&self) -> &'static str {
        use NativeFunction::*;
        match self {
            Clock => "clock",
            Str => "str",
        }
    }
}

impl LoxCallable for NativeFunction {
    fn arity(&self) -> usize {
        use NativeFunction::*;
        match self {
            Clock => 0,
            Str => 1,
        }
    }

    fn call(&self, inter: &mut Interpreter, args: Vec<Value>) -> ValueRes {
        use NativeFunction::*;
        match self {
            Clock => clock(inter, args),
            Str => val_to_str(inter, args),
        }
    }
}

impl Display for NativeFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "<native fn>")
    }
}

impl From<NativeFunction> for Callable {
    fn from(native_fn: NativeFunction) -> Self {
        Callable::Native(native_fn)
    }
}

impl Function {
    pub fn new(name: &str, params: Vec<String>, body: &[Stmt], closure: &Option<Env>) -> Self {
        Self {
            name: Some(String::from(name)),
            params,
            body: body.to_vec(),
            closure: closure.as_ref().map(Rc::clone),
        }
    }

    pub fn new_anon(params: Vec<String>, body: &[Stmt], closure: &Option<Env>) -> Self {
        Self {
            name: None,
            params,
            body: body.to_vec(),
            closure: closure.as_ref().map(Rc::clone),
        }
    }
}

impl LoxCallable for Function {
    fn arity(&self) -> usize {
        self.params.len()
    }

    fn call(&self, inter: &mut Interpreter, args: Vec<Value>) -> ValueRes {
        let env = Environ::with_enclosing(&self.closure);
        for arg in args.into_iter() {
            env.borrow_mut().define(arg);
        }

        match inter.execute_block(&self.body, env) {
            Ok(()) => Ok(Value::Nil),
            Err(RuntimeInterrupt::Return(ret)) => Ok(ret),
            Err(error) => Err(error.expect_error()),
        }
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match self.name {
            Some(ref name) => write!(f, "<fn {}>", name),
            None => write!(f, "<anonymous fn>"),
        }
    }
}

impl From<Function> for Callable {
    fn from(function: Function) -> Self {
        Callable::Function(Rc::new(function))
    }
}

impl Class {
    pub fn new(name: &str) -> Self {
        Self {
            name: String::from(name),
        }
    }
}

impl LoxCallable for Rc<Class> {
    fn arity(&self) -> usize {
        0
    }

    fn call(&self, _inter: &mut Interpreter, _args: Vec<Value>) -> ValueRes {
        let instance = ClassInstance::new(self);
        Ok(instance.into())
    }
}

impl Display for Class {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "<class {}>", self.name)
    }
}

impl From<Class> for Callable {
    fn from(class: Class) -> Self {
        Callable::Class(Rc::new(class))
    }
}

impl From<Rc<Class>> for Callable {
    fn from(class: Rc<Class>) -> Self {
        Callable::Class(class)
    }
}

impl ClassInstance {
    pub fn new(class: &Rc<Class>) -> Self {
        Self {
            class: Rc::clone(class),
            fields: HashMap::new(),
        }
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        self.fields.get(name).cloned()
    }
}

impl Display for ClassInstance {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "<instance of {}>", self.class.name)
    }
}

pub fn populate_natives(inter: &mut Interpreter) {
    let mut globals = inter.globals.borrow_mut();
    define_native(&mut globals, NativeFunction::Clock);
    define_native(&mut globals, NativeFunction::Str);
}

fn define_native(globals: &mut GlobalEnviron, function: NativeFunction) {
    globals.define(function.name(), function.into());
}

fn clock(_inter: &mut Interpreter, _args: Vec<Value>) -> ValueRes {
    let start = SystemTime::now();
    let since_the_epoch = start
        .duration_since(UNIX_EPOCH)
        .expect("Time went backwards");
    let in_ms =
        since_the_epoch.as_secs() as i64 * 1000 + since_the_epoch.subsec_nanos() as i64 / 1_000_000;
    Ok(in_ms.into())
}

fn val_to_str(_inter: &mut Interpreter, args: Vec<Value>) -> ValueRes {
    let value = args.first().unwrap();
    Ok(value.to_string().into())
}
