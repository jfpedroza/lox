use crate::class::{BoundMethod, Class, InstanceRc, LoxClass};
use crate::eval::{Env, Environ, GlobalEnviron, Interpreter, RuntimeInterrupt, ValueRes};
use crate::location::Loc;
use crate::stmt::Stmt;
use crate::value::Value;
use std::fmt::{Debug, Display, Formatter, Result as FmtResult};
use std::rc::Rc;
use std::time::{SystemTime, UNIX_EPOCH};

#[derive(Clone, Debug)]
pub enum Callable {
    Native(NativeFunction),
    Function(Rc<Function>),
    BoundMethod(BoundMethod),
    Class(Class),
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
    pub closure: Option<Env>,
    is_init: bool,
}

pub mod types {
    pub const NATIVE: &str = "native_fn";
    pub const FUNCTION: &str = "function";
    pub const CLASS: &str = "class";
}

pub trait LoxCallable: Into<Callable> {
    fn arity(&self) -> usize;
    fn call(&self, inter: &mut Interpreter, args: Vec<Value>, loc: Loc) -> ValueRes;
}

impl Callable {
    pub fn get_type(&self) -> &'static str {
        use Callable::*;
        match self {
            Native(_) => types::NATIVE,
            Function(_) => types::FUNCTION,
            BoundMethod(_) => types::FUNCTION,
            Class(_) => types::CLASS,
        }
    }

    pub fn into_instance(self) -> Option<InstanceRc> {
        match self {
            Callable::Class(class) => class.metainstance(),
            _ => None,
        }
    }
}

impl Display for Callable {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        use Callable::*;
        match self {
            Native(function) => Display::fmt(function, f),
            Function(function) => Display::fmt(function, f),
            BoundMethod(method) => Display::fmt(method, f),
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
            BoundMethod(method) => method.arity(),
            Class(class) => class.arity(),
        }
    }

    fn call(&self, inter: &mut Interpreter, args: Vec<Value>, loc: Loc) -> ValueRes {
        use Callable::*;
        match self {
            Native(function) => function.call(inter, args, loc),
            Function(function) => function.call(inter, args, loc),
            BoundMethod(method) => method.call(inter, args, loc),
            Class(class) => class.call(inter, args, loc),
        }
    }
}

impl PartialEq for Callable {
    fn eq(&self, other: &Self) -> bool {
        use Callable::*;
        match (self, other) {
            (Native(left), Native(right)) => left == right,
            (Function(left), Function(right)) => Rc::ptr_eq(left, right),
            (BoundMethod(left), BoundMethod(right)) => left == right,
            (Class(left), Class(right)) => left == right,
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

    fn call(&self, inter: &mut Interpreter, args: Vec<Value>, loc: Loc) -> ValueRes {
        use NativeFunction::*;
        match self {
            Clock => clock(inter, args, loc),
            Str => val_to_str(inter, args, loc),
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
    pub fn new(
        name: &str,
        params: Vec<String>,
        body: &[Stmt],
        closure: &Option<Env>,
        is_init: bool,
    ) -> Self {
        Self {
            name: Some(String::from(name)),
            params,
            body: body.to_vec(),
            closure: closure.as_ref().map(Rc::clone),
            is_init,
        }
    }

    pub fn new_anon(params: Vec<String>, body: &[Stmt], closure: &Option<Env>) -> Self {
        Self {
            name: None,
            params,
            body: body.to_vec(),
            closure: closure.as_ref().map(Rc::clone),
            is_init: false,
        }
    }

    pub fn call_with_closure(
        &self,
        inter: &mut Interpreter,
        args: Vec<Value>,
        _loc: Loc,
        closure: &Option<Env>,
    ) -> ValueRes {
        let env = Environ::with_enclosing(closure);
        for arg in args.into_iter() {
            env.borrow_mut().define(arg);
        }

        match inter.execute_block(&self.body, env) {
            Ok(()) => Ok(if self.is_init {
                self.get_this(closure)
            } else {
                Value::Nil
            }),
            Err(RuntimeInterrupt::Return(ret)) => Ok(if self.is_init {
                self.get_this(closure)
            } else {
                ret
            }),
            Err(error) => Err(error.expect_error()),
        }
    }

    fn get_this(&self, closure: &Option<Env>) -> Value {
        closure.as_ref().unwrap().borrow().get_at(0, 0)
    }
}

impl LoxCallable for Function {
    fn arity(&self) -> usize {
        self.params.len()
    }

    fn call(&self, inter: &mut Interpreter, args: Vec<Value>, loc: Loc) -> ValueRes {
        self.call_with_closure(inter, args, loc, &self.closure)
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

impl From<BoundMethod> for Callable {
    fn from(method: BoundMethod) -> Self {
        Callable::BoundMethod(method)
    }
}

pub fn define_native_functions(globals: &mut GlobalEnviron) {
    define_native(globals, NativeFunction::Clock);
    define_native(globals, NativeFunction::Str);
}

fn define_native(globals: &mut GlobalEnviron, function: NativeFunction) {
    globals.define(function.name(), function.into());
}

fn clock(_inter: &mut Interpreter, _args: Vec<Value>, _loc: Loc) -> ValueRes {
    let start = SystemTime::now();
    let since_the_epoch = start
        .duration_since(UNIX_EPOCH)
        .expect("Time went backwards");
    let in_ms =
        since_the_epoch.as_secs() as i64 * 1000 + since_the_epoch.subsec_nanos() as i64 / 1_000_000;
    Ok(in_ms.into())
}

fn val_to_str(_inter: &mut Interpreter, args: Vec<Value>, _loc: Loc) -> ValueRes {
    let value = args.first().unwrap();
    Ok(value.to_string().into())
}
