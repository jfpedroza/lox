use crate::constants::INIT_METHOD;
use crate::eval::{Env, Environ, GlobalEnviron, Interpreter, RuntimeInterrupt, ValueRes};
use crate::stmt::Stmt;
use crate::value::Value;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter, Result as FmtResult};
use std::rc::Rc;
use std::time::{SystemTime, UNIX_EPOCH};

#[derive(Clone, Debug)]
pub enum Callable {
    Native(NativeFunction),
    Function(Rc<Function>),
    BoundMethod(BoundMethod),
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
    is_init: bool,
}

pub struct NativeMethod {
    name: &'static str,
    arity: usize,
    fun: fn(&mut Interpreter, Vec<Value>, &mut ClassInstance) -> ValueRes,
}

#[derive(Clone, Debug)]
pub enum Method {
    Native(Rc<NativeMethod>),
    Function(Rc<Function>),
}

#[derive(Clone, Debug)]
pub struct BoundMethod {
    method: Method,
    instance: InstanceRc,
}

#[derive(Debug)]
pub struct Class {
    name: String,
    methods: HashMap<String, Method>,
    is_meta: bool,
    metainstance: Option<InstanceRc>,
}

#[derive(Debug)]
pub struct ClassInstance {
    class: Rc<Class>,
    fields: HashMap<String, Value>,
}

pub type InstanceRc = Rc<RefCell<ClassInstance>>;

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
            BoundMethod(_) => types::FUNCTION,
            Class(_) => types::CLASS,
        }
    }

    pub fn into_instance(self) -> Option<InstanceRc> {
        match self {
            Callable::Class(class) => class.metainstance.as_ref().map(Rc::clone),
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

    fn call(&self, inter: &mut Interpreter, args: Vec<Value>) -> ValueRes {
        use Callable::*;
        match self {
            Native(function) => function.call(inter, args),
            Function(function) => function.call(inter, args),
            BoundMethod(method) => method.call(inter, args),
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
            (BoundMethod(left), BoundMethod(right)) => left == right,
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

    fn call_with_closure(
        &self,
        inter: &mut Interpreter,
        args: Vec<Value>,
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

    fn call(&self, inter: &mut Interpreter, args: Vec<Value>) -> ValueRes {
        self.call_with_closure(inter, args, &self.closure)
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

impl Debug for NativeMethod {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        f.debug_struct("NativeMethod")
            .field("name", &String::from(self.name))
            .field("arity", &self.arity)
            .field("fun", &String::from("<native function>"))
            .finish()
    }
}

impl Method {
    pub fn arity(&self) -> usize {
        match self {
            Method::Native(native) => native.arity,
            Method::Function(function) => function.arity(),
        }
    }

    pub fn bind(method: Method, instance: &InstanceRc) -> Callable {
        Callable::BoundMethod(BoundMethod::new(method, instance))
    }
}

impl PartialEq for Method {
    fn eq(&self, other: &Self) -> bool {
        use Method::*;
        match (self, other) {
            (Native(left), Native(right)) => Rc::ptr_eq(left, right),
            (Function(left), Function(right)) => Rc::ptr_eq(left, right),
            (_, _) => false,
        }
    }
}

impl From<NativeMethod> for Method {
    fn from(native: NativeMethod) -> Self {
        Method::Native(Rc::new(native))
    }
}

impl From<Function> for Method {
    fn from(function: Function) -> Self {
        Method::Function(Rc::new(function))
    }
}

impl BoundMethod {
    pub fn new(method: Method, instance: &InstanceRc) -> Self {
        Self {
            method: method,
            instance: Rc::clone(instance),
        }
    }
}

impl LoxCallable for BoundMethod {
    fn arity(&self) -> usize {
        self.method.arity()
    }

    fn call(&self, inter: &mut Interpreter, args: Vec<Value>) -> ValueRes {
        match &self.method {
            Method::Native(native) => {
                let fun = native.fun;
                fun(inter, args, &mut self.instance.borrow_mut())
            }
            Method::Function(function) => {
                let env = Environ::with_enclosing(&function.closure);
                let this_val = Value::Instance(Rc::clone(&self.instance));
                env.borrow_mut().define(this_val);
                function.call_with_closure(inter, args, &Some(env))
            }
        }
    }
}

impl Display for BoundMethod {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        let method_name = match &self.method {
            Method::Native(native) => native.name,
            Method::Function(function) => function.name.as_ref().unwrap(),
        };

        let class_name = &self.instance.borrow().class.name;
        write!(f, "<method {} of class {}>", method_name, class_name)
    }
}

impl PartialEq for BoundMethod {
    fn eq(&self, other: &Self) -> bool {
        self.method == other.method && Rc::ptr_eq(&self.instance, &other.instance)
    }
}

impl From<BoundMethod> for Callable {
    fn from(method: BoundMethod) -> Self {
        Callable::BoundMethod(method)
    }
}

impl Class {
    pub fn new(
        name: &str,
        methods: HashMap<String, Method>,
        static_methods: HashMap<String, Method>,
    ) -> Self {
        let metaclass = Rc::new(Self::new_meta(name, static_methods));
        let metainstance = ClassInstance::new(&metaclass).into();

        Self {
            name: String::from(name),
            methods,
            is_meta: false,
            metainstance: Some(metainstance),
        }
    }

    fn new_meta(name: &str, methods: HashMap<String, Method>) -> Self {
        Self {
            name: String::from(name),
            methods,
            is_meta: true,
            metainstance: None,
        }
    }

    fn add_native_method(&mut self, method: NativeMethod) {
        let name = String::from(method.name);
        self.methods.insert(name, method.into());
    }

    fn find_method(&self, name: &str) -> Option<Method> {
        self.methods.get(name).cloned()
    }
}

impl LoxCallable for Rc<Class> {
    fn arity(&self) -> usize {
        self.find_method(INIT_METHOD)
            .map(|m| m.arity())
            .unwrap_or(0)
    }

    fn call(&self, inter: &mut Interpreter, args: Vec<Value>) -> ValueRes {
        let instance = ClassInstance::new(self).into();
        if let Some(method) = self.find_method(INIT_METHOD) {
            Method::bind(method, &instance).call(inter, args)?;
        }

        Ok(Value::Instance(instance))
    }
}

impl Display for Class {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        let class = if self.is_meta { "metaclass" } else { "class" };
        write!(f, "<{} {}>", class, self.name)
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

    pub fn get(instance: &InstanceRc, name: &str) -> Option<Value> {
        instance.borrow().fields.get(name).cloned().or_else(|| {
            instance
                .borrow()
                .class
                .find_method(name)
                .map(|m| Method::bind(m, instance).into())
        })
    }

    pub fn set(&mut self, name: &str, val: Value) {
        self.fields.insert(String::from(name), val);
    }
}

impl Display for ClassInstance {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "<instance of {}>", self.class.name)
    }
}

impl Into<InstanceRc> for ClassInstance {
    fn into(self) -> InstanceRc {
        Rc::new(RefCell::new(self))
    }
}

pub fn populate_globals(inter: &mut Interpreter) {
    let mut globals = inter.globals.borrow_mut();
    define_native(&mut globals, NativeFunction::Clock);
    define_native(&mut globals, NativeFunction::Str);
    define_class(&mut globals, make_map_class());
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

fn define_class(globals: &mut GlobalEnviron, class: Class) {
    let name = class.name.clone();
    globals.define(&name, Rc::new(class).into());
}

fn make_map_class() -> Class {
    let mut class = Class::new("Map", HashMap::new(), HashMap::new());

    class.add_native_method(NativeMethod {
        name: "count",
        arity: 0,
        fun: |_inter, _args, instance| Ok(Value::Integer(instance.fields.len() as i64)),
    });

    class
}
