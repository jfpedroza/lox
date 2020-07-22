use crate::eval::Interpreter;
use crate::expr::{BinOp, Expr, LitExpr, LogOp, UnOp, Visitor as ExprVisitor};
use crate::location::Loc;
use crate::stmt::{Stmt, Visitor as StmtVisitor};
use std::collections::HashMap;

pub struct Resolver<'a> {
    inter: &'a mut Interpreter,
    scopes: Vec<HashMap<String, bool>>,
}

#[derive(Debug, PartialEq, Fail)]
pub enum ResolutionError {
    #[fail(
        display = "[{}] Cannot read local variable in its own initializer.",
        _0
    )]
    VarInInitalizer(Loc),
}

type ResolveRes = Result<(), ResolutionError>;

impl<'a> Resolver<'a> {
    pub fn new(inter: &'a mut Interpreter) -> Self {
        Self {
            inter,
            scopes: Vec::new(),
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

    pub fn resolve_stmts(&mut self, stmts: &[Stmt]) -> ResolveRes {
        for stmt in stmts {
            self.resolve_stmt(stmt)?;
        }

        Ok(())
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new())
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, name: &str) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(String::from(name), false);
        }
    }

    fn define(&mut self, name: &str) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(String::from(name), true);
        }
    }

    fn resolve_local(&mut self, name: &str) {
        for i in self.scopes.len()..=0 {
            if self.scopes[i].contains_key(name) {
                self.inter.resolve(self.scopes.len() - 1 - i);
                return;
            }
        }
    }

    fn resolve_function(&mut self, params: &[String], body: &[Stmt]) -> ResolveRes {
        self.begin_scope();

        for param in params {
            self.declare(param);
            self.define(param);
        }

        self.resolve_stmts(body)?;
        self.end_scope();
        Ok(())
    }
}

impl ExprVisitor<()> for Resolver<'_> {
    type Error = ResolutionError;

    fn visit_literal_expr(&mut self, _literal: &LitExpr, _loc: Loc) -> ResolveRes {
        Ok(())
    }

    fn visit_function_expr(&mut self, params: &[String], body: &[Stmt], _loc: Loc) -> ResolveRes {
        self.resolve_function(params, body)
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
            if !scope[name] {
                return Err(ResolutionError::VarInInitalizer(loc));
            }
        }

        self.resolve_local(name);

        Ok(())
    }

    fn visit_assign_expr(&mut self, name: &str, expr: &Expr, _loc: Loc) -> ResolveRes {
        self.resolve_expr(expr)?;
        self.resolve_local(name);
        Ok(())
    }

    fn visit_call_expr(&mut self, callee: &Expr, args: &[Expr], _loc: Loc) -> ResolveRes {
        self.resolve_expr(callee)?;
        self.resolve_exprs(args)
    }
}

impl StmtVisitor<()> for Resolver<'_> {
    type Error = ResolutionError;

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

    fn visit_var_stmt(&mut self, name: &str, init: &Option<Expr>, _loc: Loc) -> ResolveRes {
        self.declare(name);
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
        params: &[String],
        body: &[Stmt],
        _loc: Loc,
    ) -> ResolveRes {
        self.declare(name);
        self.define(name);

        self.resolve_function(params, body)
    }

    fn visit_return_stmt(&mut self, ret: &Option<Expr>, _loc: Loc) -> ResolveRes {
        if let Some(ret_expr) = ret {
            self.resolve_expr(ret_expr)?;
        }

        Ok(())
    }

    fn visit_break_stmt(&mut self, _loc: Loc) -> ResolveRes {
        Ok(())
    }
}
