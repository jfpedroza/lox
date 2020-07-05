use crate::expr::Expr;
use crate::location::{Loc, Located};

#[derive(PartialEq, Debug)]
pub enum StmtKind {
    Expression(Expr),
    Print(Expr),
    Var(String, Option<Expr>),
    Block(Vec<Stmt>),
}

pub type Stmt = Located<StmtKind>;

pub trait Visitor<Res> {
    type Error;
    type Result = std::result::Result<Res, Self::Error>;

    fn visit_expression_stmt(&mut self, expr: &Expr, loc: Loc) -> Self::Result;

    fn visit_print_stmt(&mut self, expr: &Expr, loc: Loc) -> Self::Result;

    fn visit_var_stmt(&mut self, name: &str, init: &Option<Expr>, loc: Loc) -> Self::Result;

    fn visit_block_stmt(&mut self, stmts: &[Stmt], loc: Loc) -> Self::Result;
}

impl Stmt {
    pub fn expression(expr: Expr, loc: Loc) -> Self {
        Stmt::new(StmtKind::Expression(expr), loc)
    }

    pub fn print(expr: Expr, loc: Loc) -> Self {
        Stmt::new(StmtKind::Print(expr), loc)
    }

    pub fn var(name: &str, init: Option<Expr>, loc: Loc) -> Self {
        Stmt::new(StmtKind::Var(String::from(name), init), loc)
    }

    pub fn block(stmts: Vec<Stmt>, loc: Loc) -> Self {
        Stmt::new(StmtKind::Block(stmts), loc)
    }

    pub fn accept<Vis, Res, Error>(&self, visitor: &mut Vis) -> Vis::Result
    where
        Vis: Visitor<Res, Error = Error>,
    {
        use StmtKind::*;
        match &self.kind {
            Expression(expr) => visitor.visit_expression_stmt(expr, self.loc),
            Print(expr) => visitor.visit_print_stmt(expr, self.loc),
            Var(name, init) => visitor.visit_var_stmt(name, init, self.loc),
            Block(stmts) => visitor.visit_block_stmt(stmts, self.loc),
        }
    }
}
