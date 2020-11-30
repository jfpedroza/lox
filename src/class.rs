use crate::array::{ArrayClass, ArrayRc};
use crate::callable::{Callable, Function, LoxCallable};
use crate::constants::INIT_METHOD;
use crate::eval::{Environ, GlobalEnviron, Interpreter, RuntimeError, ValueRes};
use crate::location::Loc;
use crate::value::Value;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter, Result as FmtResult};
use std::rc::Rc;
use uuid::Uuid;

#[derive(Clone, Debug)]
pub enum Class {
    Generic(Rc<GenericClass>),
    Array(Rc<ArrayClass>),
}

pub type MethodMap = HashMap<String, Method>;

#[derive(Debug)]
pub struct GenericClass {
    name: String,
    uuid: Uuid,
    superclass: Option<Class>,
    methods: MethodMap,
    getters: MethodMap,
    is_meta: bool,
    pub metainstance: Option<InstanceRc>,
}

pub trait LoxClass: LoxCallable {
    fn name(&self) -> &str;
    fn uuid(&self) -> Uuid;
    fn superclass(&self) -> Option<&Class>;
    fn methods(&self) -> &MethodMap;
    fn getters(&self) -> &MethodMap;
    fn metainstance(&self) -> Option<InstanceRc>;
}

#[derive(Debug)]
pub struct ClassInstance {
    kind: InstanceKind,
    fields: HashMap<String, Value>,
}

#[derive(Debug)]
enum InstanceKind {
    Generic(Rc<GenericClass>),
    Array(ArrayRc),
}

pub type InstanceRc = Rc<RefCell<ClassInstance>>;

#[derive(Clone, Debug)]
pub enum Method {
    Native(Rc<NativeMethod>),
    Function(Rc<Function>),
    StaticWrapper(Rc<StaticWrapper>),
}

pub struct NativeMethod {
    pub name: &'static str,
    pub arity: usize,
    pub fun: NativeMethodFn,
}

pub type NativeMethodFn = fn(&mut Interpreter, Vec<Value>, &mut ClassInstance, Loc) -> ValueRes;

#[derive(Debug)]
pub struct StaticWrapper {
    wrapped: Method,
    class_uuid: Uuid,
    class_name: String,
}

#[derive(Clone, Debug)]
pub struct BoundMethod {
    method: Method,
    instance: InstanceRc,
}

impl Class {
    fn metaclass(&self) -> Option<Class> {
        self.metainstance()
            .as_ref()
            .map(|inst| inst.borrow().class())
    }

    fn find_method(&self, name: &str) -> Option<Method> {
        match self {
            Class::Generic(class) => find_method(class, name),
            Class::Array(class) => find_method(class, name),
        }
    }

    fn find_getter(&self, name: &str) -> Option<Method> {
        match self {
            Class::Generic(class) => find_getter(class, name),
            Class::Array(class) => find_getter(class, name),
        }
    }

    pub fn get_and_bind(
        &self,
        instance: &InstanceRc,
        inter: &mut Interpreter,
        name: &str,
        loc: Loc,
    ) -> Option<ValueRes> {
        match self {
            Class::Generic(class) => get_and_bind(class, instance, inter, name, loc),
            Class::Array(class) => get_and_bind(class, instance, inter, name, loc),
        }
    }

    pub fn add_native_method(methods: &mut MethodMap, method: NativeMethod) {
        let name = String::from(method.name);
        methods.insert(name, method.into());
    }

    pub fn create_metainstance(
        name: &str,
        class_uuid: Uuid,
        superclass: Option<&Class>,
        mut static_methods: MethodMap,
        methods: &MethodMap,
        getters: &MethodMap,
    ) -> InstanceRc {
        let static_wrappers = methods
            .iter()
            .chain(getters.iter())
            .map(|entry| Self::create_static_method(entry, class_uuid, name));
        static_methods.extend(static_wrappers);

        let supermetaclass = superclass.and_then(|sc| sc.metaclass());
        let metaclass = Rc::new(GenericClass::new_meta(name, supermetaclass, static_methods));
        ClassInstance::new_generic(&metaclass).into()
    }

    fn create_static_method(
        (name, method): (&String, &Method),
        class_uuid: Uuid,
        class_name: &str,
    ) -> (String, Method) {
        let name = String::from(name);
        let wrapper = StaticWrapper::new(method, class_uuid, class_name);
        (name, wrapper.into())
    }
}

impl LoxClass for Class {
    fn name(&self) -> &str {
        match self {
            Class::Generic(class) => class.name(),
            Class::Array(class) => class.name(),
        }
    }

    fn uuid(&self) -> Uuid {
        match self {
            Class::Generic(class) => class.uuid(),
            Class::Array(class) => class.uuid(),
        }
    }

    fn superclass(&self) -> Option<&Class> {
        match self {
            Class::Generic(class) => class.superclass(),
            Class::Array(class) => class.superclass(),
        }
    }

    fn methods(&self) -> &MethodMap {
        match self {
            Class::Generic(class) => class.methods(),
            Class::Array(class) => class.methods(),
        }
    }

    fn getters(&self) -> &MethodMap {
        match self {
            Class::Generic(class) => class.getters(),
            Class::Array(class) => class.getters(),
        }
    }

    fn metainstance(&self) -> Option<InstanceRc> {
        match self {
            Class::Generic(class) => class.metainstance(),
            Class::Array(class) => class.metainstance(),
        }
    }
}

impl LoxCallable for Class {
    fn arity(&self) -> usize {
        match self {
            Class::Generic(class) => class.arity(),
            Class::Array(class) => class.arity(),
        }
    }

    fn call(&self, inter: &mut Interpreter, args: Vec<Value>, loc: Loc) -> ValueRes {
        match self {
            Class::Generic(class) => class.call(inter, args, loc),
            Class::Array(class) => class.call(inter, args, loc),
        }
    }
}

impl Display for Class {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match self {
            Class::Generic(class) => Display::fmt(class, f),
            Class::Array(class) => Display::fmt(class, f),
        }
    }
}

impl PartialEq for Class {
    fn eq(&self, other: &Self) -> bool {
        use Class::{Array, Generic};
        match (self, other) {
            (Generic(left), Generic(right)) => Rc::ptr_eq(left, right),
            (Array(left), Array(right)) => Rc::ptr_eq(left, right),
            (_, _) => false,
        }
    }
}

impl From<Class> for Callable {
    fn from(class: Class) -> Self {
        Callable::Class(class)
    }
}

fn find_method<T>(class: &Rc<T>, name: &str) -> Option<Method>
where
    Rc<T>: LoxClass,
{
    class
        .methods()
        .get(name)
        .cloned()
        .or_else(|| class.superclass().and_then(|sc| sc.find_method(name)))
}

fn find_getter<T>(class: &Rc<T>, name: &str) -> Option<Method>
where
    Rc<T>: LoxClass,
{
    class
        .getters()
        .get(name)
        .cloned()
        .or_else(|| class.superclass().and_then(|sc| sc.find_getter(name)))
}

fn get_and_bind<T>(
    class: &Rc<T>,
    instance: &InstanceRc,
    inter: &mut Interpreter,
    name: &str,
    loc: Loc,
) -> Option<ValueRes>
where
    Rc<T>: LoxClass,
{
    find_method(class, name)
        .map(|m| Method::bind(m, instance).into())
        .map(Ok)
        .or_else(|| {
            find_getter(class, name)
                .map(|m| Method::bind(m, instance))
                .map(|m| m.call(inter, Vec::new(), loc))
        })
}

impl GenericClass {
    pub fn new(
        name: &str,
        superclass: Option<Class>,
        methods: MethodMap,
        getters: MethodMap,
        static_methods: MethodMap,
    ) -> Self {
        let uuid = Uuid::new_v4();
        let metainstance = Class::create_metainstance(
            name,
            uuid,
            superclass.as_ref(),
            static_methods,
            &methods,
            &getters,
        );

        Self {
            name: String::from(name),
            uuid,
            superclass,
            methods,
            getters,
            is_meta: false,
            metainstance: Some(metainstance),
        }
    }

    fn new_meta(name: &str, superclass: Option<Class>, methods: MethodMap) -> Self {
        let uuid = Uuid::new_v4();
        Self {
            name: String::from(name),
            uuid,
            superclass,
            methods,
            getters: HashMap::new(),
            is_meta: true,
            metainstance: None,
        }
    }

    pub fn new_empty(name: &str, superclass: Option<Class>) -> Self {
        Self::new(
            name,
            superclass,
            HashMap::new(),
            HashMap::new(),
            HashMap::new(),
        )
    }
}

impl LoxClass for Rc<GenericClass> {
    fn name(&self) -> &str {
        &self.name
    }

    fn uuid(&self) -> Uuid {
        self.uuid
    }

    fn superclass(&self) -> Option<&Class> {
        self.superclass.as_ref()
    }

    fn methods(&self) -> &MethodMap {
        &self.methods
    }

    fn getters(&self) -> &MethodMap {
        &self.getters
    }

    fn metainstance(&self) -> Option<InstanceRc> {
        self.metainstance.as_ref().map(Rc::clone)
    }
}

impl LoxCallable for Rc<GenericClass> {
    fn arity(&self) -> usize {
        find_method(self, INIT_METHOD)
            .map(|m| m.arity())
            .unwrap_or(0)
    }

    fn call(&self, inter: &mut Interpreter, args: Vec<Value>, loc: Loc) -> ValueRes {
        let instance = ClassInstance::new_generic(self).into();
        if let Some(method) = find_method(self, INIT_METHOD) {
            Method::bind(method, &instance).call(inter, args, loc)?;
        }

        Ok(Value::Instance(instance))
    }
}

impl From<GenericClass> for Callable {
    fn from(class: GenericClass) -> Self {
        Callable::from(Rc::new(class))
    }
}

impl From<Rc<GenericClass>> for Callable {
    fn from(class: Rc<GenericClass>) -> Self {
        Callable::Class(Class::from(class))
    }
}

impl From<Rc<GenericClass>> for Class {
    fn from(class: Rc<GenericClass>) -> Self {
        Class::Generic(class)
    }
}

impl Display for GenericClass {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        let class = if self.is_meta { "metaclass" } else { "class" };
        write!(f, "<{} {}>", class, self.name)
    }
}

impl ClassInstance {
    pub fn new_generic(class: &Rc<GenericClass>) -> Self {
        Self {
            kind: InstanceKind::Generic(Rc::clone(class)),
            fields: HashMap::new(),
        }
    }

    pub fn from_array(array: ArrayRc) -> Self {
        Self {
            kind: InstanceKind::Array(array),
            fields: HashMap::new(),
        }
    }

    pub fn array(&self) -> Option<ArrayRc> {
        use InstanceKind::*;
        match &self.kind {
            Generic(_class) => None,
            Array(array) => Some(Rc::clone(&array)),
        }
    }

    pub fn class(&self) -> Class {
        use InstanceKind::*;
        match &self.kind {
            Generic(class) => Rc::clone(class).into(),
            Array(array) => array.borrow().class().into(),
        }
    }

    pub fn class_name(&self) -> &str {
        use InstanceKind::*;
        match &self.kind {
            Generic(class) => &class.name,
            Array(array) => array.borrow().class_name(),
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
            let class = instance.borrow().class();
            class.get_and_bind(instance, inter, name, loc)
        })
    }

    pub fn set(&mut self, name: &str, val: Value, _loc: Loc) {
        self.fields.insert(String::from(name), val);
    }
}

impl Display for ClassInstance {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "<instance of {}>", self.class_name())
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
            Method::StaticWrapper(wrapper) => wrapper.arity(),
        }
    }

    pub fn name(&self) -> &str {
        match self {
            Method::Native(native) => native.name,
            Method::Function(function) => function.name.as_ref().unwrap(),
            Method::StaticWrapper(wrapper) => wrapper.name(),
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

impl From<StaticWrapper> for Method {
    fn from(wrapper: StaticWrapper) -> Self {
        Method::StaticWrapper(Rc::new(wrapper))
    }
}

impl NativeMethod {
    pub fn new(name: &'static str, arity: usize, fun: NativeMethodFn) -> Self {
        Self { name, arity, fun }
    }
}

impl StaticWrapper {
    pub fn new(method: &Method, class_uuid: Uuid, class_name: &str) -> Self {
        Self {
            wrapped: method.clone(),
            class_uuid,
            class_name: String::from(class_name),
        }
    }

    pub fn arity(&self) -> usize {
        self.wrapped.arity() + 1
    }

    pub fn name(&self) -> &str {
        self.wrapped.name()
    }

    fn bind(&self, instance: &InstanceRc) -> Callable {
        Method::bind(self.wrapped.clone(), instance)
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

    fn call(&self, inter: &mut Interpreter, mut args: Vec<Value>, loc: Loc) -> ValueRes {
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
            Method::StaticWrapper(wrapper) => {
                let first_arg = args.remove(0);
                let val_type = first_arg.get_type();
                match first_arg.into_instance() {
                    Some(instance) if instance.borrow().class().uuid() == wrapper.class_uuid => {
                        let bound = wrapper.bind(&instance);
                        bound.call(inter, args, loc)
                    }
                    Some(_) | None => Err(RuntimeError::ExpectedType(
                        loc,
                        wrapper.class_name.clone(),
                        String::from(val_type),
                    )),
                }
            }
        }
    }
}

impl Display for BoundMethod {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        let method_name = self.method.name();
        let instance = self.instance.borrow();
        let class_name = instance.class_name();
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
    let name = String::from(class.name());
    globals.define(&name, class.into());
}

fn make_map_class() -> Class {
    let mut class = GenericClass::new_empty("Map", None);

    Class::add_native_method(
        &mut class.methods,
        NativeMethod {
            name: "count",
            arity: 0,
            fun: |_inter, _args, instance, _loc| Ok(Value::Integer(instance.fields.len() as i64)),
        },
    );

    Class::Generic(Rc::new(class))
}
