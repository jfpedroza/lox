use crate::eval::{Environ, Interpreter, RuntimeInterrupt, ValueRes};
use crate::stmt::Stmt;
use crate::value::Value;
use std::fmt::{Display, Formatter, Result as FmtResult};
use std::rc::Rc;
use std::time::{SystemTime, UNIX_EPOCH};

#[derive(PartialEq, Clone, Debug)]
pub enum Callable {
    Native(NativeFunction),
    Function(Function),
}

#[derive(PartialEq, Copy, Clone, Debug)]
pub enum NativeFunction {
    Clock,
    Str,
}

#[derive(PartialEq, Clone, Debug)]
pub struct Function {
    name: Rc<Option<String>>,
    params: Rc<Vec<String>>,
    body: Rc<Vec<Stmt>>,
}

pub mod types {
    pub const NATIVE: &str = "native_fn";
    pub const FUNCTION: &str = "function";
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
        }
    }
}

impl Display for Callable {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        use Callable::*;
        match self {
            Native(function) => Display::fmt(function, f),
            Function(function) => Display::fmt(function, f),
        }
    }
}

impl LoxCallable for Callable {
    fn arity(&self) -> usize {
        use Callable::*;
        match self {
            Native(function) => function.arity(),
            Function(function) => function.arity(),
        }
    }

    fn call(&self, inter: &mut Interpreter, args: Vec<Value>) -> ValueRes {
        use Callable::*;
        match self {
            Native(function) => function.call(inter, args),
            Function(function) => function.call(inter, args),
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
    pub fn new(name: &str, params: &[String], body: &[Stmt]) -> Self {
        Self {
            name: Rc::new(Some(String::from(name))),
            params: Rc::new(params.to_vec()),
            body: Rc::new(body.to_vec()),
        }
    }
}

impl LoxCallable for Function {
    fn arity(&self) -> usize {
        self.params.len()
    }

    fn call(&self, inter: &mut Interpreter, args: Vec<Value>) -> ValueRes {
        let env = Environ::with_enclosing(&inter.globals);
        for (i, arg) in args.into_iter().enumerate() {
            env.borrow_mut().define(&self.params[i], arg);
        }

        inter
            .execute_block(&self.body, env)
            .map_err(RuntimeInterrupt::expect_error)?;
        Ok(Value::Nil)
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match self.name.as_ref() {
            Some(name) => write!(f, "<fn {}>", name),
            None => write!(f, "<anonymous fn>"),
        }
    }
}

impl From<Function> for Callable {
    fn from(function: Function) -> Self {
        Callable::Function(function)
    }
}

pub fn populate_natives(inter: &mut Interpreter) {
    let mut globals = inter.globals.borrow_mut();
    define_native(&mut globals, NativeFunction::Clock);
    define_native(&mut globals, NativeFunction::Str);
}

fn define_native(globals: &mut Environ, function: NativeFunction) {
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
