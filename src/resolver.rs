#[cfg(test)]
mod tests;

use crate::constants::{INIT_METHOD, SUPER_KEYWORD, THIS_KEYWORD};
use crate::error::Warning;
use crate::eval::Interpreter;
use crate::expr::{BinOp, Expr, ExprKind, LitExpr, LogOp, Param, UnOp, Visitor as ExprVisitor};
use crate::location::Loc;
use crate::stmt::{FunctionKind, Stmt, StmtKind, Visitor as StmtVisitor};
use std::collections::HashMap;

pub struct Resolver<'a> {
    inter: &'a mut Interpreter,
    scopes: Vec<HashMap<String, ResolvedVar>>,
    current_fun: FunctionType,
    current_class: ClassType,
    in_loop: bool,
    errors: Vec<ResolutionError>,
    pub warnings: Vec<Warning>,
}

#[derive(Debug)]
struct ResolvedVar {
    loc: Loc,
    index: usize,
    defined: bool,
    used: bool,
}

#[derive(Copy, Clone, PartialEq, Eq)]
enum FunctionType {
    None,
    Function,
    Method,
    Initializer,
    StaticMethod,
}

#[derive(Copy, Clone, PartialEq, Eq)]
enum ClassType {
    None,
    Class,
    Subclass,
}

#[derive(Debug, PartialEq, Fail)]
pub enum ResolutionError {
    VarInInitalizer(Loc),
    VarAlreadyInScope(Loc, String),
    DuplicateArgumentName(Loc, String),
    DuplicateMethod(Loc, String, String, String),
    ReturnOutsideFun(Loc),
    ThisOutsideClass(Loc),
    ReturnInInitializer(Loc),
    ThisInStaticMethod(Loc),
    ClassInheritsItself(Loc, String),
    SuperOutsideClass(Loc),
    SuperNoInSubclass(Loc),
    BreakOutsideLoop(Loc),
    Multiple(Vec<ResolutionError>),
}

type ResolveRes = Result<(), !>;

impl<'a> Resolver<'a> {
    pub fn new(inter: &'a mut Interpreter) -> Self {
        Self {
            inter,
            scopes: Vec::new(),
            current_fun: FunctionType::None,
            current_class: ClassType::None,
            in_loop: false,
            errors: vec![],
            warnings: vec![],
        }
    }

    pub fn resolve(&mut self, stmts: &[Stmt]) -> Result<(), ResolutionError> {
        for stmt in stmts {
            let Ok(()) = self.resolve_stmt(stmt);
        }

        match self.errors.len() {
            0 => Ok(()),
            1 => Err(self.errors.pop().unwrap()),
            _ => {
                let mut errors = Vec::new();
                std::mem::swap(&mut self.errors, &mut errors);
                Err(ResolutionError::Multiple(errors))
            }
        }
    }

    fn resolve_expr(&mut self, expr: &Expr) -> ResolveRes {
        expr.accept(self)
    }

    fn resolve_exprs(&mut self, exprs: &[Expr]) -> ResolveRes {
        for expr in exprs {
            self.resolve_expr(expr)?;
        }

        Ok(())
    }

    fn resolve_stmt(&mut self, stmt: &Stmt) -> ResolveRes {
        stmt.accept(self)
    }

    fn resolve_stmts(&mut self, stmts: &[Stmt]) -> ResolveRes {
        for stmt in stmts {
            self.resolve_stmt(stmt)?;
        }

        Ok(())
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        let scope = self
            .scopes
            .pop()
            .expect("There should be a scope to end here");
        for (name, resolved) in scope {
            if !resolved.used {
                self.warnings
                    .push(Warning::UnusedVariable(resolved.loc, name));
            }
        }
    }

    fn declare_name<F>(&mut self, name: &str, loc: Loc, err_fn: F) -> ResolveRes
    where
        F: FnOnce() -> ResolutionError,
    {
        if let Some(scope) = self.scopes.last_mut() {
            if scope.contains_key(name) {
                self.errors.push(err_fn());
                return Ok(());
            }
            scope.insert(String::from(name), ResolvedVar::new(loc, scope.len()));
        }

        Ok(())
    }

    fn declare_var(&mut self, name: &str, loc: Loc) -> ResolveRes {
        self.declare_name(name, loc, || {
            ResolutionError::var_already_in_scope(loc, name)
        })
    }

    fn declare_param(&mut self, name: &str, loc: Loc) -> ResolveRes {
        self.declare_name(name, loc, || ResolutionError::duplicate_arg_name(loc, name))
    }

    fn define(&mut self, name: &str) {
        if let Some(scope) = self.scopes.last_mut() {
            let resolved = scope
                .get_mut(name)
                .unwrap_or_else(|| panic!("Variable '{}' wasn't declared", name));
            resolved.defined = true;
        }
    }

    fn declare_define_special(&mut self, keyword: &str, loc: Loc) {
        let scope = self.scopes.last_mut().unwrap();
        scope.insert(
            String::from(keyword),
            ResolvedVar {
                loc,
                index: scope.len(),
                defined: true,
                used: true,
            },
        );
    }

    fn resolve_local(&mut self, name: &str, loc: Loc, reading: bool) {
        if !self.scopes.is_empty() {
            let len = self.scopes.len();
            for i in (0..len).rev() {
                if let Some(resolved) = self.scopes[i].get_mut(name) {
                    self.inter.resolve(name, loc, len - 1 - i, resolved.index);
                    if reading {
                        resolved.used = true;
                    }
                    return;
                }
            }
        }
    }

    fn resolve_function(
        &mut self,
        params: &[Param],
        body: &[Stmt],
        fun_type: FunctionType,
    ) -> ResolveRes {
        let enclosing_fun = self.current_fun;
        let enclosing_loop = self.in_loop;
        self.current_fun = fun_type;
        self.in_loop = false;

        self.begin_scope();

        for Param { kind: name, loc } in params {
            self.declare_param(name, *loc)?;
            self.define(name);
        }

        self.resolve_stmts(body)?;
        self.end_scope();

        self.current_fun = enclosing_fun;
        self.in_loop = enclosing_loop;

        Ok(())
    }
}

impl ExprVisitor<()> for Resolver<'_> {
    type Error = !;

    fn visit_literal_expr(&mut self, _literal: &LitExpr, _loc: Loc) -> ResolveRes {
        Ok(())
    }

    fn visit_function_expr(&mut self, params: &[Param], body: &[Stmt], _loc: Loc) -> ResolveRes {
        self.resolve_function(params, body, FunctionType::Function)
    }

    fn visit_grouping_expr(&mut self, expr: &Expr, _loc: Loc) -> ResolveRes {
        self.resolve_expr(expr)
    }

    fn visit_unary_expr(&mut self, _op: &UnOp, expr: &Expr, _loc: Loc) -> ResolveRes {
        self.resolve_expr(expr)
    }

    fn visit_binary_expr(
        &mut self,
        left: &Expr,
        _op: &BinOp,
        right: &Expr,
        _loc: Loc,
    ) -> ResolveRes {
        self.resolve_expr(left)?;
        self.resolve_expr(right)
    }

    fn visit_logical_expr(
        &mut self,
        left: &Expr,
        _op: &LogOp,
        right: &Expr,
        _loc: Loc,
    ) -> ResolveRes {
        self.resolve_expr(left)?;
        self.resolve_expr(right)
    }

    fn visit_comma_expr(&mut self, left: &Expr, right: &Expr, _loc: Loc) -> ResolveRes {
        self.resolve_expr(left)?;
        self.resolve_expr(right)
    }

    fn visit_cond_expr(&mut self, cond: &Expr, left: &Expr, right: &Expr, _loc: Loc) -> ResolveRes {
        self.resolve_expr(cond)?;
        self.resolve_expr(left)?;
        self.resolve_expr(right)
    }

    fn visit_variable_expr(&mut self, name: &str, loc: Loc) -> ResolveRes {
        if let Some(scope) = self.scopes.last() {
            if let Some(ResolvedVar { defined: false, .. }) = scope.get(name) {
                self.errors.push(ResolutionError::VarInInitalizer(loc));
                return Ok(());
            }
        }

        self.resolve_local(name, loc, true);

        Ok(())
    }

    fn visit_assign_expr(&mut self, name: &str, expr: &Expr, loc: Loc) -> ResolveRes {
        self.resolve_expr(expr)?;
        self.resolve_local(name, loc, false);
        Ok(())
    }

    fn visit_call_expr(&mut self, callee: &Expr, args: &[Expr], _loc: Loc) -> ResolveRes {
        self.resolve_expr(callee)?;
        self.resolve_exprs(args)
    }

    fn visit_get_expr(&mut self, obj: &Expr, _name: &str, _loc: Loc) -> ResolveRes {
        self.resolve_expr(obj)
    }

    fn visit_set_expr(&mut self, obj: &Expr, _name: &str, expr: &Expr, _loc: Loc) -> ResolveRes {
        self.resolve_expr(expr)?;
        self.resolve_expr(obj)
    }

    fn visit_array_expr(&mut self, elements: &[Expr], _loc: Loc) -> ResolveRes {
        self.resolve_exprs(elements)
    }

    fn visit_this_expr(&mut self, loc: Loc) -> ResolveRes {
        if self.current_class == ClassType::None {
            self.errors.push(ResolutionError::ThisOutsideClass(loc));
        } else if self.current_fun == FunctionType::StaticMethod {
            self.errors.push(ResolutionError::ThisInStaticMethod(loc));
        } else {
            self.resolve_local(THIS_KEYWORD, loc, true);
        }

        Ok(())
    }

    fn visit_super_expr(&mut self, _method: &str, loc: Loc) -> ResolveRes {
        match self.current_class {
            ClassType::None => self.errors.push(ResolutionError::SuperOutsideClass(loc)),
            ClassType::Class => self.errors.push(ResolutionError::SuperNoInSubclass(loc)),
            ClassType::Subclass => {
                self.resolve_local(SUPER_KEYWORD, loc, true);
                self.resolve_local(THIS_KEYWORD, loc, true);
            }
        }

        Ok(())
    }
}

impl StmtVisitor<()> for Resolver<'_> {
    type Error = !;

    fn visit_expression_stmt(&mut self, expr: &Expr, _loc: Loc) -> ResolveRes {
        self.resolve_expr(expr)
    }

    fn visit_if_stmt(
        &mut self,
        cond: &Expr,
        then_branch: &Stmt,
        else_branch: &Option<Box<Stmt>>,
        _loc: Loc,
    ) -> ResolveRes {
        self.resolve_expr(cond)?;
        self.resolve_stmt(then_branch)?;
        if let Some(else_stmt) = else_branch {
            self.resolve_stmt(else_stmt)?;
        }

        Ok(())
    }

    fn visit_print_stmt(&mut self, expr: &Expr, _loc: Loc) -> ResolveRes {
        self.resolve_expr(expr)
    }

    fn visit_while_stmt(&mut self, cond: &Expr, body: &Stmt, _loc: Loc) -> ResolveRes {
        let enclosing_loop = self.in_loop;
        self.in_loop = true;
        self.resolve_expr(cond)?;
        self.resolve_stmt(body)?;
        self.in_loop = enclosing_loop;
        Ok(())
    }

    fn visit_var_stmt(&mut self, name: &str, init: &Option<Expr>, loc: Loc) -> ResolveRes {
        self.declare_var(name, loc)?;
        if let Some(init_expr) = init {
            self.resolve_expr(init_expr)?;
        }

        self.define(name);

        Ok(())
    }

    fn visit_block_stmt(&mut self, stmts: &[Stmt], _loc: Loc) -> ResolveRes {
        self.begin_scope();
        self.resolve_stmts(stmts)?;
        self.end_scope();
        Ok(())
    }

    fn visit_function_stmt(
        &mut self,
        name: &str,
        params: &[Param],
        body: &[Stmt],
        _kind: FunctionKind,
        loc: Loc,
    ) -> ResolveRes {
        self.declare_var(name, loc)?;
        self.define(name);

        self.resolve_function(params, body, FunctionType::Function)
    }

    fn visit_return_stmt(&mut self, ret: &Option<Expr>, loc: Loc) -> ResolveRes {
        if self.current_fun == FunctionType::None {
            self.errors.push(ResolutionError::ReturnOutsideFun(loc));
        }

        if let Some(ret_expr) = ret {
            if self.current_fun == FunctionType::Initializer {
                self.errors.push(ResolutionError::ReturnInInitializer(loc));
            }

            self.resolve_expr(ret_expr)?;
        }

        Ok(())
    }

    fn visit_class_stmt(
        &mut self,
        name: &str,
        superclass: &Option<Expr>,
        methods: &[Stmt],
        loc: Loc,
    ) -> ResolveRes {
        use FunctionKind::*;
        let enclosing_class = self.current_class;
        self.current_class = ClassType::Class;

        self.declare_var(name, loc)?;
        self.define(name);

        if let Some(superclass) = superclass {
            match &superclass.kind {
                ExprKind::Variable(supername) if supername == name => {
                    self.errors
                        .push(ResolutionError::class_inherits_itself(loc, name));
                }
                ExprKind::Variable(_supername) => {}
                _ => unreachable!(),
            }

            self.current_class = ClassType::Subclass;

            self.resolve_expr(superclass)?;

            self.begin_scope();
            self.declare_define_special(SUPER_KEYWORD, superclass.loc);
        }

        self.begin_scope();
        self.declare_define_special(THIS_KEYWORD, loc);

        let mut method_names = Vec::new();
        let mut static_method_names = Vec::new();

        for method in methods {
            match &method.kind {
                StmtKind::Function(method_name, params, body, kind)
                    if [Method, Getter].contains(kind) =>
                {
                    let declaration = if kind == &Method && method_name == INIT_METHOD {
                        FunctionType::Initializer
                    } else {
                        FunctionType::Method
                    };

                    self.resolve_function(params, body, declaration)?;

                    if method_names.contains(method_name) {
                        self.errors.push(ResolutionError::duplicate_method(
                            method.loc,
                            name,
                            "a method",
                            method_name,
                        ));
                    } else {
                        method_names.push(String::from(method_name));
                    }
                }
                StmtKind::Function(method_name, params, body, StaticMethod) => {
                    self.resolve_function(params, body, FunctionType::StaticMethod)?;

                    if static_method_names.contains(method_name) {
                        self.errors.push(ResolutionError::duplicate_method(
                            method.loc,
                            name,
                            "a static method",
                            method_name,
                        ));
                    } else {
                        static_method_names.push(String::from(method_name));
                    }
                }
                _ => unreachable!(),
            }
        }

        self.end_scope();

        if superclass.is_some() {
            self.end_scope();
        }

        self.resolve_local(name, loc, false);

        self.current_class = enclosing_class;

        Ok(())
    }

    fn visit_break_stmt(&mut self, loc: Loc) -> ResolveRes {
        if !self.in_loop {
            self.errors.push(ResolutionError::BreakOutsideLoop(loc));
        }

        Ok(())
    }
}

impl ResolvedVar {
    pub fn new(loc: Loc, index: usize) -> Self {
        Self {
            loc,
            index,
            defined: false,
            used: false,
        }
    }
}

impl ResolutionError {
    fn var_already_in_scope(loc: Loc, name: &str) -> Self {
        Self::VarAlreadyInScope(loc, String::from(name))
    }

    fn duplicate_arg_name(loc: Loc, name: &str) -> Self {
        Self::DuplicateArgumentName(loc, String::from(name))
    }

    fn duplicate_method(loc: Loc, class_name: &str, mtype: &str, name: &str) -> Self {
        Self::DuplicateMethod(
            loc,
            String::from(class_name),
            String::from(mtype),
            String::from(name),
        )
    }

    fn class_inherits_itself(loc: Loc, name: &str) -> Self {
        Self::ClassInheritsItself(loc, String::from(name))
    }
}
