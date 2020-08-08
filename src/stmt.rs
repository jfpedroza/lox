use crate::expr::{Expr, Param};
use crate::location::{Loc, Located};

#[derive(PartialEq, Debug, Clone)]
pub enum StmtKind {
    Expression(Expr),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    Print(Expr),
    While(Expr, Box<Stmt>),
    Var(String, Option<Expr>),
    Block(Vec<Stmt>),
    Function(String, Vec<Param>, Vec<Stmt>),
    Return(Option<Expr>),
    Class(String, Vec<Stmt>, Vec<Stmt>),
    Break,
}

pub type Stmt = Located<StmtKind>;

pub trait Visitor<Res> {
    type Error;
    type Result = std::result::Result<Res, Self::Error>;

    fn visit_expression_stmt(&mut self, expr: &Expr, loc: Loc) -> Self::Result;

    fn visit_if_stmt(
        &mut self,
        cond: &Expr,
        then_branch: &Stmt,
        else_branch: &Option<Box<Stmt>>,
        loc: Loc,
    ) -> Self::Result;

    fn visit_print_stmt(&mut self, expr: &Expr, loc: Loc) -> Self::Result;

    fn visit_while_stmt(&mut self, cond: &Expr, body: &Stmt, loc: Loc) -> Self::Result;

    fn visit_var_stmt(&mut self, name: &str, init: &Option<Expr>, loc: Loc) -> Self::Result;

    fn visit_block_stmt(&mut self, stmts: &[Stmt], loc: Loc) -> Self::Result;

    fn visit_function_stmt(
        &mut self,
        name: &str,
        params: &[Param],
        body: &[Stmt],
        loc: Loc,
    ) -> Self::Result;

    fn visit_return_stmt(&mut self, ret: &Option<Expr>, loc: Loc) -> Self::Result;

    fn visit_class_stmt(
        &mut self,
        name: &str,
        methods: &[Stmt],
        static_methods: &[Stmt],
        loc: Loc,
    ) -> Self::Result;

    fn visit_break_stmt(&mut self, loc: Loc) -> Self::Result;
}

impl Stmt {
    pub fn expression(expr: Expr, loc: Loc) -> Self {
        Stmt::new(StmtKind::Expression(expr), loc)
    }

    pub fn if_stmt(cond: Expr, then_branch: Stmt, else_branch: Option<Stmt>, loc: Loc) -> Self {
        Stmt::new(
            StmtKind::If(cond, Box::new(then_branch), else_branch.map(Box::new)),
            loc,
        )
    }

    pub fn print(expr: Expr, loc: Loc) -> Self {
        Stmt::new(StmtKind::Print(expr), loc)
    }

    pub fn while_stmt(cond: Expr, body: Stmt, loc: Loc) -> Self {
        Stmt::new(StmtKind::While(cond, Box::new(body)), loc)
    }

    pub fn var(name: &str, init: Option<Expr>, loc: Loc) -> Self {
        Stmt::new(StmtKind::Var(String::from(name), init), loc)
    }

    pub fn block(stmts: Vec<Stmt>, loc: Loc) -> Self {
        Stmt::new(StmtKind::Block(stmts), loc)
    }

    pub fn function(name: &str, params: Vec<Param>, body: Vec<Stmt>, loc: Loc) -> Self {
        Stmt::new(StmtKind::Function(String::from(name), params, body), loc)
    }

    pub fn return_stmt(ret: Option<Expr>, loc: Loc) -> Self {
        Stmt::new(StmtKind::Return(ret), loc)
    }

    pub fn class(name: &str, methods: Vec<Stmt>, static_methods: Vec<Stmt>, loc: Loc) -> Self {
        Stmt::new(
            StmtKind::Class(String::from(name), methods, static_methods),
            loc,
        )
    }

    pub fn break_stmt(loc: Loc) -> Self {
        Stmt::new(StmtKind::Break, loc)
    }

    pub fn accept<Vis, Res, Error>(&self, visitor: &mut Vis) -> Vis::Result
    where
        Vis: Visitor<Res, Error = Error>,
    {
        use StmtKind::*;
        match &self.kind {
            Expression(expr) => visitor.visit_expression_stmt(expr, self.loc),
            If(cond, then_branch, else_branch) => {
                visitor.visit_if_stmt(cond, then_branch, else_branch, self.loc)
            }
            Print(expr) => visitor.visit_print_stmt(expr, self.loc),
            While(expr, body) => visitor.visit_while_stmt(expr, body, self.loc),
            Var(name, init) => visitor.visit_var_stmt(name, init, self.loc),
            Block(stmts) => visitor.visit_block_stmt(stmts, self.loc),
            Function(name, params, body) => {
                visitor.visit_function_stmt(name, params, body, self.loc)
            }
            Return(ret) => visitor.visit_return_stmt(ret, self.loc),
            Class(name, methods, static_methods) => {
                visitor.visit_class_stmt(name, methods, static_methods, self.loc)
            }
            Break => visitor.visit_break_stmt(self.loc),
        }
    }
}

impl From<Expr> for Stmt {
    fn from(expr: Expr) -> Self {
        let loc = expr.loc;
        Self::expression(expr, loc)
    }
}

impl Default for StmtKind {
    fn default() -> Self {
        StmtKind::Expression(Expr::default())
    }
}
