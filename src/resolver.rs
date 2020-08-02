#[cfg(test)]
mod tests;

use crate::error::Warning;
use crate::eval::Interpreter;
use crate::expr::{BinOp, Expr, LitExpr, LogOp, Param, UnOp, Visitor as ExprVisitor};
use crate::location::Loc;
use crate::stmt::{Stmt, Visitor as StmtVisitor};
use std::collections::HashMap;

pub struct Resolver<'a> {
    inter: &'a mut Interpreter,
    scopes: Vec<HashMap<String, ResolvedVar>>,
    current_fun: FunctionType,
    in_loop: bool,
    errors: Vec<ResolutionError>,
    pub warnings: Vec<Warning>,
}

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
}

#[derive(Debug, PartialEq, Fail)]
pub enum ResolutionError {
    VarInInitalizer(Loc),
    VarAlreadyInScope(Loc, String),
    DuplicateArgumentName(Loc, String),
    ReturnOutsideFun(Loc),
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
            self.resolve_expr(ret_expr)?;
        }

        Ok(())
    }

    fn visit_class_stmt(&mut self, name: &str, _methods: &[Stmt], loc: Loc) -> ResolveRes {
        self.declare_var(name, loc)?;
        self.define(name);

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
}
