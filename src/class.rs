use crate::callable::{Callable, Function, LoxCallable};
use crate::constants::INIT_METHOD;
use crate::eval::{Environ, GlobalEnviron, Interpreter, ValueRes};
use crate::location::Loc;
use crate::value::Value;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter, Result as FmtResult};
use std::rc::Rc;

#[derive(Debug)]
pub struct Class {
    name: String,
    superclass: Option<Rc<Class>>,
    methods: HashMap<String, Method>,
    getters: HashMap<String, Method>,
    is_meta: bool,
    pub metainstance: Option<InstanceRc>,
}

#[derive(Debug)]
pub struct ClassInstance {
    class: Rc<Class>,
    fields: HashMap<String, Value>,
}

pub type InstanceRc = Rc<RefCell<ClassInstance>>;

#[derive(Clone, Debug)]
pub enum Method {
    Native(Rc<NativeMethod>),
    Function(Rc<Function>),
}

pub struct NativeMethod {
    name: &'static str,
    arity: usize,
    fun: fn(&mut Interpreter, Vec<Value>, &mut ClassInstance, Loc) -> ValueRes,
}

#[derive(Clone, Debug)]
pub struct BoundMethod {
    method: Method,
    instance: InstanceRc,
}

impl Class {
    pub fn new(
        name: &str,
        superclass: Option<Rc<Class>>,
        methods: HashMap<String, Method>,
        getters: HashMap<String, Method>,
        static_methods: HashMap<String, Method>,
    ) -> Self {
        let supermetaclass = superclass.as_ref().and_then(|sc| sc.metaclass());
        let metaclass = Rc::new(Self::new_meta(name, supermetaclass, static_methods));
        let metainstance = ClassInstance::new(&metaclass).into();

        Self {
            name: String::from(name),
            superclass,
            methods,
            getters,
            is_meta: false,
            metainstance: Some(metainstance),
        }
    }

    fn new_meta(
        name: &str,
        superclass: Option<Rc<Class>>,
        methods: HashMap<String, Method>,
    ) -> Self {
        Self {
            name: String::from(name),
            superclass,
            methods,
            getters: HashMap::new(),
            is_meta: true,
            metainstance: None,
        }
    }

    fn metaclass(&self) -> Option<Rc<Class>> {
        self.metainstance
            .as_ref()
            .map(|inst| Rc::clone(&inst.borrow().class))
    }

    fn add_native_method(&mut self, method: NativeMethod) {
        let name = String::from(method.name);
        self.methods.insert(name, method.into());
    }

    fn find_method(&self, name: &str) -> Option<Method> {
        self.methods
            .get(name)
            .cloned()
            .or_else(|| self.superclass.as_ref().and_then(|sc| sc.find_method(name)))
    }

    fn find_getter(&self, name: &str) -> Option<Method> {
        self.getters
            .get(name)
            .cloned()
            .or_else(|| self.superclass.as_ref().and_then(|sc| sc.find_getter(name)))
    }

    pub fn get_and_bind(
        &self,
        instance: &InstanceRc,
        inter: &mut Interpreter,
        name: &str,
        loc: Loc,
    ) -> Option<ValueRes> {
        self.find_method(name)
            .map(|m| Method::bind(m, instance).into())
            .map(Ok)
            .or_else(|| {
                self.find_getter(name)
                    .map(|m| Method::bind(m, instance))
                    .map(|m| m.call(inter, Vec::new(), loc))
            })
    }
}

impl LoxCallable for Rc<Class> {
    fn arity(&self) -> usize {
        self.find_method(INIT_METHOD)
            .map(|m| m.arity())
            .unwrap_or(0)
    }

    fn call(&self, inter: &mut Interpreter, args: Vec<Value>, loc: Loc) -> ValueRes {
        let instance = ClassInstance::new(self).into();
        if let Some(method) = self.find_method(INIT_METHOD) {
            Method::bind(method, &instance).call(inter, args, loc)?;
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

impl ClassInstance {
    pub fn new(class: &Rc<Class>) -> Self {
        Self {
            class: Rc::clone(class),
            fields: HashMap::new(),
        }
    }

    pub fn get(
        inter: &mut Interpreter,
        instance: &InstanceRc,
        name: &str,
        loc: Loc,
    ) -> Option<ValueRes> {
        let field = { instance.borrow().fields.get(name).cloned().map(Ok) };

        field.or_else(|| {
            let class = Rc::clone(&instance.borrow().class);
            class.get_and_bind(instance, inter, name, loc)
        })
    }

    pub fn set(&mut self, name: &str, val: Value, _loc: Loc) {
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
            method,
            instance: Rc::clone(instance),
        }
    }
}

impl LoxCallable for BoundMethod {
    fn arity(&self) -> usize {
        self.method.arity()
    }

    fn call(&self, inter: &mut Interpreter, args: Vec<Value>, loc: Loc) -> ValueRes {
        match &self.method {
            Method::Native(native) => {
                let fun = native.fun;
                fun(inter, args, &mut self.instance.borrow_mut(), loc)
            }
            Method::Function(function) => {
                let env = Environ::with_enclosing(&function.closure);
                let this_val = Value::Instance(Rc::clone(&self.instance));
                env.borrow_mut().define(this_val);
                function.call_with_closure(inter, args, loc, &Some(env))
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

pub fn define_native_classes(globals: &mut GlobalEnviron) {
    define_class(globals, make_map_class());
}

fn define_class(globals: &mut GlobalEnviron, class: Class) {
    let name = class.name.clone();
    globals.define(&name, Rc::new(class).into());
}

fn make_map_class() -> Class {
    let mut class = Class::new("Map", None, HashMap::new(), HashMap::new(), HashMap::new());

    class.add_native_method(NativeMethod {
        name: "count",
        arity: 0,
        fun: |_inter, _args, instance, _loc| Ok(Value::Integer(instance.fields.len() as i64)),
    });

    class
}
