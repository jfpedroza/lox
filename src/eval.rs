#[cfg(test)]
mod tests;

use crate::callable::{populate_globals, Class, ClassInstance, Function, LoxCallable, Method};
use crate::constants::{INIT_METHOD, THIS_KEYWORD};
use crate::expr::{BinOp, Expr, LitExpr, LogOp, Param, UnOp, Visitor as ExprVisitor};
use crate::location::Loc;
use crate::stmt::{Stmt, StmtKind, Visitor as StmtVisitor};
use crate::value::Value;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub struct Interpreter {
    env: Option<Env>,
    pub globals: GlobalEnv,
    locals: HashMap<(String, Loc), ResolvedLocal>,
}

#[derive(Debug)]
pub struct GlobalEnviron {
    values: HashMap<String, Value>,
}

#[derive(Debug)]
pub struct Environ {
    values: Vec<Value>,
    enclosing: Option<Env>,
}

pub type Env = Rc<RefCell<Environ>>;
pub type GlobalEnv = Rc<RefCell<GlobalEnviron>>;

#[derive(Copy, Clone)]
struct ResolvedLocal {
    depth: usize,
    index: usize,
}

#[derive(Debug, PartialEq, Fail)]
pub enum RuntimeError {
    UnsupportedOperand(Loc, String, String),
    UnsupportedOperands(Loc, String, String, String),
    DivisionByZero(Loc),
    UndefinedVariable(Loc, String),
    NotACallable(Loc, String),
    MismatchingArity(Loc, usize, usize),
    NoProperties(Loc, String),
    UndefinedProperty(Loc, String),
    NoFields(Loc, String),
}

#[derive(Debug)]
pub enum RuntimeInterrupt {
    Error(RuntimeError),
    Return(Value),
    Break,
}

pub type ValueRes = Result<Value, RuntimeError>;
type ExecuteRes = Result<(), RuntimeInterrupt>;

impl Interpreter {
    pub fn new() -> Self {
        let globals = GlobalEnviron::new().into();
        let mut inter = Interpreter {
            env: None,
            globals,
            locals: HashMap::new(),
        };

        populate_globals(&mut inter);
        inter
    }

    pub fn interpret(&mut self, stmts: &[Stmt]) -> Result<(), RuntimeError> {
        for stmt in stmts {
            self.execute(stmt).map_err(RuntimeInterrupt::expect_error)?
        }

        Ok(())
    }

    pub fn evaluate(&mut self, expr: &Expr) -> ValueRes {
        expr.accept(self)
    }

    fn execute(&mut self, stmt: &Stmt) -> ExecuteRes {
        stmt.accept(self)
    }

    pub fn execute_block(&mut self, stmts: &[Stmt], env: Env) -> ExecuteRes {
        let prev = self.env.as_ref().map(Rc::clone);
        self.env = Some(env);

        for stmt in stmts {
            match self.execute(stmt) {
                Ok(()) => (),
                Err(err) => {
                    self.env = prev;
                    return Err(err);
                }
            }
        }

        self.env = prev;
        Ok(())
    }

    pub fn resolve(&mut self, name: &str, loc: Loc, depth: usize, index: usize) {
        self.locals
            .insert((String::from(name), loc), ResolvedLocal::new(depth, index));
    }

    fn get_resolved_local(&self, name: &str, loc: Loc) -> Option<ResolvedLocal> {
        self.locals.get(&(name.to_string(), loc)).cloned()
    }

    fn look_up_variable(&self, name: &str, loc: Loc) -> Result<Value, RuntimeError> {
        if let Some(ResolvedLocal { depth, index }) = self.get_resolved_local(name, loc) {
            Ok(self.local_env().borrow().get_at(depth, index))
        } else {
            self.globals.borrow().get(name, loc)
        }
    }

    fn local_env(&self) -> Env {
        let env = self
            .env
            .as_ref()
            .expect("Expected a local environment to be set");

        Rc::clone(env)
    }

    fn define(&mut self, name: &str, val: Value) {
        if let Some(env) = &self.env {
            env.borrow_mut().define(val);
        } else {
            self.globals.borrow_mut().define(name, val);
        }
    }

    fn assign(&mut self, name: &str, val: Value, loc: Loc) -> Result<(), RuntimeError> {
        if let Some(ResolvedLocal { depth, index }) = self.get_resolved_local(name, loc) {
            self.local_env().borrow_mut().assign_at(depth, index, val);
        } else {
            self.globals.borrow_mut().assign(name, val, loc)?;
        }

        Ok(())
    }

    fn stmt_to_method_entry(&self, stmt: &Stmt, is_static: bool) -> (String, Method) {
        match &stmt.kind {
            StmtKind::Function(name, params, body) => {
                let params: Vec<_> = params.iter().map(|p| p.kind.clone()).collect();
                let is_init = !is_static && name == INIT_METHOD;
                let method = Function::new(name, params, body, &self.env, is_init);
                (name.clone(), method.into())
            }
            _ => unreachable!(),
        }
    }
}

impl GlobalEnviron {
    pub fn new() -> Self {
        GlobalEnviron {
            values: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: &str, val: Value) {
        self.values.insert(String::from(name), val);
    }

    pub fn get(&self, name: &str, loc: Loc) -> Result<Value, RuntimeError> {
        if let Some(val) = self.values.get(name) {
            Ok(val.clone())
        } else {
            Err(RuntimeError::undefined_variable(loc, name))
        }
    }

    pub fn assign(&mut self, name: &str, val: Value, loc: Loc) -> Result<(), RuntimeError> {
        if self.values.contains_key(name) {
            self.values.insert(String::from(name), val);

            Ok(())
        } else {
            Err(RuntimeError::undefined_variable(loc, &name))
        }
    }
}

impl Environ {
    pub fn with_enclosing(enclosing: &Option<Env>) -> Env {
        let env = Environ {
            values: Vec::new(),
            enclosing: enclosing.as_ref().map(Rc::clone),
        };

        env.into()
    }

    pub fn define(&mut self, val: Value) {
        self.values.push(val);
    }

    fn ancestor(&self, distance: usize) -> &Environ {
        let mut env: *const Environ = self;

        for _ in 0..distance {
            if let Some(enclosing) = unsafe { (*env).enclosing.as_ref() } {
                env = enclosing.as_ptr();
            } else {
                panic!("Ancestor not found at distance {}", distance);
            }
        }

        unsafe { &*env }
    }

    pub fn get_at(&self, distance: usize, index: usize) -> Value {
        self.ancestor(distance).values[index].clone()
    }

    fn ancestor_mut(&mut self, distance: usize) -> &mut Environ {
        let mut env: *mut Environ = self;

        for _ in 0..distance {
            if let Some(enclosing) = unsafe { (*env).enclosing.as_ref() } {
                env = enclosing.as_ptr();
            } else {
                panic!("Ancestor not found at distance {}", distance);
            }
        }

        unsafe { &mut *env }
    }

    pub fn assign_at(&mut self, distance: usize, index: usize, val: Value) {
        self.ancestor_mut(distance).values[index] = val;
    }
}

impl Into<GlobalEnv> for GlobalEnviron {
    fn into(self) -> GlobalEnv {
        Rc::new(RefCell::new(self))
    }
}

impl Into<Env> for Environ {
    fn into(self) -> Env {
        Rc::new(RefCell::new(self))
    }
}

impl ResolvedLocal {
    pub fn new(depth: usize, index: usize) -> Self {
        Self { depth, index }
    }
}

impl ExprVisitor<Value> for Interpreter {
    type Error = RuntimeError;

    fn visit_literal_expr(&mut self, literal: &LitExpr, _loc: Loc) -> ValueRes {
        Ok(literal.into())
    }

    fn visit_function_expr(&mut self, params: &[Param], body: &[Stmt], _loc: Loc) -> ValueRes {
        let params: Vec<_> = params.iter().map(|p| p.kind.clone()).collect();
        let function = Function::new_anon(params, body, &self.env);
        Ok(function.into())
    }

    fn visit_grouping_expr(&mut self, expr: &Expr, _loc: Loc) -> ValueRes {
        self.evaluate(expr)
    }

    fn visit_unary_expr(&mut self, op: &UnOp, expr: &Expr, loc: Loc) -> ValueRes {
        let val = self.evaluate(expr)?;

        Ok(match op {
            UnOp::Negate => val.negate(loc)?,
            UnOp::Not => val.not(),
        })
    }

    fn visit_binary_expr(&mut self, left: &Expr, op: &BinOp, right: &Expr, loc: Loc) -> ValueRes {
        let left_val = self.evaluate(left)?;
        let right_val = self.evaluate(right)?;

        Ok(match op {
            BinOp::Add => left_val.add(right_val, loc)?,
            BinOp::Sub => left_val.sub(right_val, loc)?,
            BinOp::Mul => left_val.mul(right_val, loc)?,
            BinOp::Div => left_val.div(right_val, loc)?,
            BinOp::Rem => left_val.rem(right_val, loc)?,
            BinOp::Equal => left_val.equal(&right_val),
            BinOp::NotEqual => left_val.not_eq(&right_val),
            BinOp::Greater => left_val.greater(&right_val, loc)?,
            BinOp::GreaterEqual => left_val.greater_eq(&right_val, loc)?,
            BinOp::Less => left_val.less(&right_val, loc)?,
            BinOp::LessEqual => left_val.less_eq(&right_val, loc)?,
        })
    }

    fn visit_logical_expr(&mut self, left: &Expr, op: &LogOp, right: &Expr, _loc: Loc) -> ValueRes {
        let left_val = self.evaluate(left)?;

        Ok(match op {
            LogOp::And if !left_val.is_truthy() => left_val,
            LogOp::Or if left_val.is_truthy() => left_val,
            _ => self.evaluate(right)?,
        })
    }

    fn visit_comma_expr(&mut self, left: &Expr, right: &Expr, _loc: Loc) -> ValueRes {
        let _ = self.evaluate(left)?;
        self.evaluate(right)
    }

    fn visit_cond_expr(&mut self, cond: &Expr, left: &Expr, right: &Expr, _loc: Loc) -> ValueRes {
        let cond_val = self.evaluate(cond)?;
        if cond_val.is_truthy() {
            self.evaluate(left)
        } else {
            self.evaluate(right)
        }
    }

    fn visit_variable_expr(&mut self, name: &str, loc: Loc) -> ValueRes {
        self.look_up_variable(name, loc)
    }

    fn visit_assign_expr(&mut self, name: &str, expr: &Expr, loc: Loc) -> ValueRes {
        let val = self.evaluate(expr)?;
        self.assign(name, val.clone(), loc)?;

        Ok(val)
    }

    fn visit_call_expr(&mut self, callee: &Expr, args: &[Expr], loc: Loc) -> ValueRes {
        let callee = self.evaluate(callee)?;
        let args: Vec<_> = args
            .iter()
            .map(|a| self.evaluate(a))
            .collect::<Result<_, _>>()?;

        if let Value::Callable(callable) = callee {
            if args.len() == callable.arity() {
                callable.call(self, args)
            } else {
                Err(RuntimeError::mismatching_arity(
                    loc,
                    callable.arity(),
                    args.len(),
                ))
            }
        } else {
            Err(RuntimeError::not_a_callable(loc, callee))
        }
    }

    fn visit_get_expr(&mut self, obj: &Expr, name: &str, loc: Loc) -> ValueRes {
        let obj = self.evaluate(obj)?;
        let val_type = obj.get_type();
        if let Some(instance) = obj.into_instance() {
            ClassInstance::get(&instance, name)
                .ok_or_else(|| RuntimeError::undefined_property(loc, name))
        } else {
            Err(RuntimeError::no_properties(loc, val_type))
        }
    }

    fn visit_set_expr(&mut self, obj: &Expr, name: &str, expr: &Expr, loc: Loc) -> ValueRes {
        let obj = self.evaluate(obj)?;
        let val_type = obj.get_type();
        if let Some(instance) = obj.into_instance() {
            let val = self.evaluate(expr)?;
            instance.borrow_mut().set(name, val.clone());
            Ok(val)
        } else {
            Err(RuntimeError::no_fields(loc, val_type))
        }
    }

    fn visit_this_expr(&mut self, loc: Loc) -> ValueRes {
        self.look_up_variable(THIS_KEYWORD, loc)
    }
}

impl StmtVisitor<()> for Interpreter {
    type Error = RuntimeInterrupt;

    fn visit_expression_stmt(&mut self, expr: &Expr, _loc: Loc) -> ExecuteRes {
        self.evaluate(expr)?;
        Ok(())
    }

    fn visit_if_stmt(
        &mut self,
        cond: &Expr,
        then_branch: &Stmt,
        else_branch: &Option<Box<Stmt>>,
        _loc: Loc,
    ) -> ExecuteRes {
        let cond_val = self.evaluate(cond)?;
        if cond_val.is_truthy() {
            self.execute(then_branch)
        } else if let Some(else_stmt) = else_branch {
            self.execute(else_stmt)
        } else {
            Ok(())
        }
    }

    fn visit_print_stmt(&mut self, expr: &Expr, _loc: Loc) -> ExecuteRes {
        let val = self.evaluate(expr)?;
        println!("{}", val);
        Ok(())
    }

    fn visit_while_stmt(&mut self, cond: &Expr, body: &Stmt, _loc: Loc) -> ExecuteRes {
        while self.evaluate(cond)?.is_truthy() {
            match self.execute(body) {
                Ok(()) => (),
                Err(RuntimeInterrupt::Break) => break,
                Err(interrupt) => {
                    return Err(interrupt);
                }
            }
        }

        Ok(())
    }

    fn visit_var_stmt(&mut self, name: &str, init: &Option<Expr>, _loc: Loc) -> ExecuteRes {
        let init_val = if let Some(expr) = init {
            self.evaluate(expr)?
        } else {
            Value::Nil
        };

        self.define(name, init_val);
        Ok(())
    }

    fn visit_block_stmt(&mut self, stmts: &[Stmt], _loc: Loc) -> ExecuteRes {
        self.execute_block(stmts, Environ::with_enclosing(&self.env))
    }

    fn visit_function_stmt(
        &mut self,
        name: &str,
        params: &[Param],
        body: &[Stmt],
        _loc: Loc,
    ) -> ExecuteRes {
        let params: Vec<_> = params.iter().map(|p| p.kind.clone()).collect();
        let function = Function::new(name, params, body, &self.env, false);
        self.define(name, function.into());
        Ok(())
    }

    fn visit_return_stmt(&mut self, ret: &Option<Expr>, _loc: Loc) -> ExecuteRes {
        let ret_val = if let Some(expr) = ret {
            self.evaluate(expr)?
        } else {
            Value::Nil
        };

        Err(RuntimeInterrupt::Return(ret_val))
    }

    fn visit_class_stmt(
        &mut self,
        name: &str,
        methods: &[Stmt],
        static_methods: &[Stmt],
        loc: Loc,
    ) -> ExecuteRes {
        self.define(name, Value::Nil);

        let methods: HashMap<_, _> = methods
            .iter()
            .map(|stmt| self.stmt_to_method_entry(stmt, false))
            .collect();

        let static_methods: HashMap<_, _> = static_methods
            .iter()
            .map(|stmt| self.stmt_to_method_entry(stmt, true))
            .collect();

        let class = Class::new(name, methods, static_methods);
        self.assign(name, Value::Callable(class.into()), loc)?;
        Ok(())
    }

    fn visit_break_stmt(&mut self, _loc: Loc) -> ExecuteRes {
        Err(RuntimeInterrupt::Break)
    }
}

macro_rules! arithmethic_operation {
    ($op:tt, $lhs:ident, $rhs:ident, $loc:ident) => {
        use Value::*;
        return match ($lhs, $rhs) {
            (Integer(left), Integer(right)) => Ok(Integer(left $op right)),
            (Integer(left), Float(right)) => Ok(Float((left as f64) $op right)),
            (Float(left), Integer(right)) => Ok(Float(left $op (right as f64))),
            (Float(left), Float(right)) => Ok(Float(left $op right)),
            (left, right) => Err(RuntimeError::unsupported_operands(
                $loc,
                stringify!($op),
                &left,
                &right,
            )),
        }
    };
}

macro_rules! comparison_operation {
    ($op:tt, $lhs:ident, $rhs:ident, $loc:ident) => {
        use Value::*;
        return match ($lhs, $rhs) {
            (Integer(left), Integer(right)) => Ok(Boolean(left $op right)),
            (Integer(left), Float(right)) => Ok(Boolean((*left as f64) $op *right)),
            (Float(left), Integer(right)) => Ok(Boolean(*left $op (*right as f64))),
            (Float(left), Float(right)) => Ok(Boolean(left $op right)),
            (left, right) => Err(RuntimeError::unsupported_operands(
                $loc,
                stringify!($op),
                left,
                right,
            )),
        }
    };
}

impl Value {
    fn not(self) -> Self {
        Value::Boolean(!self.is_truthy())
    }

    fn negate(self, loc: Loc) -> ValueRes {
        match self {
            Value::Integer(int) => Ok(Value::Integer(-int)),
            Value::Float(float) => Ok(Value::Float(-float)),
            value => Err(RuntimeError::unsupported_operand(loc, "-", value)),
        }
    }

    fn add(self, rhs: Value, loc: Loc) -> ValueRes {
        use Value::*;
        match (self, rhs) {
            (Integer(left), Integer(right)) => Ok(Integer(left + right)),
            (Integer(left), Float(right)) => Ok(Float((left as f64) + right)),
            (Float(left), Integer(right)) => Ok(Float(left + (right as f64))),
            (Float(left), Float(right)) => Ok(Float(left + right)),
            (Str(left), Str(right)) => Ok(Str(format!("{}{}", left, right))),
            (left, right) => Err(RuntimeError::unsupported_operands(loc, "+", &left, &right)),
        }
    }

    fn sub(self, rhs: Value, loc: Loc) -> ValueRes {
        arithmethic_operation!(-, self, rhs, loc);
    }

    fn mul(self, rhs: Value, loc: Loc) -> ValueRes {
        arithmethic_operation!(*, self, rhs, loc);
    }

    fn div(self, rhs: Value, loc: Loc) -> ValueRes {
        if rhs.number() == Some(0.0) {
            return Err(RuntimeError::DivisionByZero(loc));
        }

        arithmethic_operation!(/, self, rhs, loc);
    }

    fn rem(self, rhs: Value, loc: Loc) -> ValueRes {
        if rhs.number() == Some(0.0) {
            return Err(RuntimeError::DivisionByZero(loc));
        }

        arithmethic_operation!(%, self, rhs, loc);
    }

    fn equal(&self, rhs: &Value) -> Value {
        use std::f64::EPSILON;
        use Value::*;
        let eq = |l: f64, r: f64| (l - r).abs() <= EPSILON * 64.0;

        Boolean(match (self, rhs) {
            (Integer(left), Integer(right)) => left == right,
            (Integer(left), Float(right)) => eq(*left as f64, *right),
            (Float(left), Integer(right)) => eq(*left, *right as f64),
            (Float(left), Float(right)) => eq(*left, *right),
            (Str(left), Str(right)) => left == right,
            (Boolean(left), Boolean(right)) => left == right,
            (Nil, Nil) => true,
            (Callable(left), Callable(right)) => left == right,
            (Instance(left), Instance(right)) => Rc::ptr_eq(left, right),
            (_, _) => false,
        })
    }

    fn not_eq(&self, rhs: &Value) -> Value {
        self.equal(rhs).not()
    }

    fn greater(&self, rhs: &Value, loc: Loc) -> ValueRes {
        comparison_operation!(>, self, rhs, loc);
    }

    fn greater_eq(&self, rhs: &Value, loc: Loc) -> ValueRes {
        comparison_operation!(>=, self, rhs, loc);
    }

    fn less(&self, rhs: &Value, loc: Loc) -> ValueRes {
        comparison_operation!(<, self, rhs, loc);
    }

    fn less_eq(&self, rhs: &Value, loc: Loc) -> ValueRes {
        comparison_operation!(<=, self, rhs, loc);
    }
}

impl RuntimeError {
    fn unsupported_operand(loc: Loc, op: &str, val: Value) -> Self {
        Self::UnsupportedOperand(loc, String::from(op), String::from(val.get_type()))
    }

    fn unsupported_operands(loc: Loc, op: &str, left: &Value, right: &Value) -> Self {
        Self::UnsupportedOperands(
            loc,
            String::from(op),
            String::from(left.get_type()),
            String::from(right.get_type()),
        )
    }

    fn undefined_variable(loc: Loc, name: &str) -> Self {
        Self::UndefinedVariable(loc, String::from(name))
    }

    fn not_a_callable(loc: Loc, callee: Value) -> Self {
        Self::NotACallable(loc, String::from(callee.get_type()))
    }

    fn mismatching_arity(loc: Loc, expected: usize, got: usize) -> Self {
        Self::MismatchingArity(loc, expected, got)
    }

    fn no_properties(loc: Loc, val_type: &str) -> Self {
        Self::NoProperties(loc, String::from(val_type))
    }

    fn undefined_property(loc: Loc, name: &str) -> Self {
        Self::UndefinedProperty(loc, String::from(name))
    }

    fn no_fields(loc: Loc, val_type: &str) -> Self {
        Self::NoFields(loc, String::from(val_type))
    }
}

impl RuntimeInterrupt {
    pub fn expect_error(self) -> RuntimeError {
        use RuntimeInterrupt::*;
        match self {
            Error(error) => error,
            interrupt => panic!(
                "Expected interrupt to be an error at this point. Got: {:?}",
                interrupt
            ),
        }
    }
}

impl From<RuntimeError> for RuntimeInterrupt {
    fn from(error: RuntimeError) -> Self {
        Self::Error(error)
    }
}
