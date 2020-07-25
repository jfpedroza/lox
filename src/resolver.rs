use crate::eval::Interpreter;
use crate::expr::{BinOp, Expr, LitExpr, LogOp, Param, UnOp, Visitor as ExprVisitor};
use crate::location::Loc;
use crate::stmt::{Stmt, Visitor as StmtVisitor};
use std::collections::HashMap;

pub struct Resolver<'a> {
    inter: &'a mut Interpreter,
    scopes: Vec<HashMap<String, bool>>,
    current_fun: FunctionType,
    errors: Vec<ResolutionError>,
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
    Multiple(Vec<ResolutionError>),
}

type ResolveRes = Result<(), !>;

impl<'a> Resolver<'a> {
    pub fn new(inter: &'a mut Interpreter) -> Self {
        Self {
            inter,
            scopes: Vec::new(),
            current_fun: FunctionType::None,
            errors: vec![],
        }
    }

    pub fn resolve(&mut self, stmts: &[Stmt]) -> Result<(), ResolutionError> {
        for stmt in stmts {
            let Ok(()) = self.resolve_stmt(stmt);
        }

        match self.errors.len() {
            0 => Ok(()),
            1 => Err(self.errors.pop().unwrap()),
            len => {
                let mut errors = Vec::with_capacity(len);
                errors.append(&mut self.errors);
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
        self.scopes.pop();
    }

    fn declare_name<F>(&mut self, name: &str, err_fn: F) -> ResolveRes
    where
        F: FnOnce() -> ResolutionError,
    {
        if let Some(scope) = self.scopes.last_mut() {
            if scope.contains_key(name) {
                self.errors.push(err_fn());
                return Ok(());
            }
            scope.insert(String::from(name), false);
        }

        Ok(())
    }

    fn declare_var(&mut self, name: &str, loc: Loc) -> ResolveRes {
        self.declare_name(name, || ResolutionError::var_already_in_scope(loc, name))
    }

    fn declare_param(&mut self, name: &str, loc: Loc) -> ResolveRes {
        self.declare_name(name, || ResolutionError::duplicate_arg_name(loc, name))
    }

    fn define(&mut self, name: &str) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(String::from(name), true);
        }
    }

    fn resolve_local(&mut self, name: &str, loc: Loc) {
        if !self.scopes.is_empty() {
            for i in (0..self.scopes.len()).rev() {
                if self.scopes[i].contains_key(name) {
                    self.inter.resolve(name, loc, self.scopes.len() - 1 - i);
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
        self.current_fun = fun_type;

        self.begin_scope();

        for Param { kind: name, loc } in params {
            self.declare_param(name, *loc)?;
            self.define(name);
        }

        self.resolve_stmts(body)?;
        self.end_scope();

        self.current_fun = enclosing_fun;

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
            if Some(&false) == scope.get(name) {
                self.errors.push(ResolutionError::VarInInitalizer(loc));
                return Ok(());
            }
        }

        self.resolve_local(name, loc);

        Ok(())
    }

    fn visit_assign_expr(&mut self, name: &str, expr: &Expr, loc: Loc) -> ResolveRes {
        self.resolve_expr(expr)?;
        self.resolve_local(name, loc);
        Ok(())
    }

    fn visit_call_expr(&mut self, callee: &Expr, args: &[Expr], _loc: Loc) -> ResolveRes {
        self.resolve_expr(callee)?;
        self.resolve_exprs(args)
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
        self.resolve_expr(cond)?;
        self.resolve_stmt(body)
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

    fn visit_break_stmt(&mut self, _loc: Loc) -> ResolveRes {
        Ok(())
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
