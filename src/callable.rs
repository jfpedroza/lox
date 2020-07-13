use crate::eval::{Environ, Interpreter, ValueRes};
use crate::value::Value;
use std::fmt::{Display, Formatter, Result as FmtResult};
use std::time::{SystemTime, UNIX_EPOCH};

#[derive(PartialEq, Clone, Debug)]
pub enum Callable {
    Native(NativeFunction),
}

#[derive(PartialEq, Copy, Clone, Debug)]
pub enum NativeFunction {
    Clock,
    Str,
}

pub mod types {
    pub const NATIVE: &str = "native_fn";
}

pub trait LoxCallable {
    fn arity(&self) -> usize;
    fn call(&self, inter: &mut Interpreter, args: Vec<Value>) -> ValueRes;
}

impl Callable {
    pub fn get_type(&self) -> &'static str {
        use Callable::*;
        match self {
            Native(_) => types::NATIVE,
        }
    }
}

impl Display for Callable {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        use Callable::*;
        match self {
            Native(function) => Display::fmt(function, f),
        }
    }
}

impl LoxCallable for Callable {
    fn arity(&self) -> usize {
        use Callable::*;
        match self {
            Native(function) => function.arity(),
        }
    }

    fn call(&self, inter: &mut Interpreter, args: Vec<Value>) -> ValueRes {
        use Callable::*;
        match self {
            Native(function) => function.call(inter, args),
        }
    }
}

impl From<NativeFunction> for Callable {
    fn from(native_fn: NativeFunction) -> Self {
        Callable::Native(native_fn)
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

pub fn populate_natives(inter: &mut Interpreter) {
    let mut globals = inter.globals.borrow_mut();
    define_native(&mut globals, NativeFunction::Clock);
    define_native(&mut globals, NativeFunction::Str);
}

fn define_native(globals: &mut Environ, function: NativeFunction) {
    globals.define(function.name(), Value::Callable(function.into()));
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
