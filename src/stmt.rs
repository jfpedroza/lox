use crate::expr::Expr;
use crate::location::{Loc, Located};

#[derive(PartialEq, Debug)]
pub enum StmtKind {
    Expression(Expr),
    Print(Expr),
    Var(String, Option<Expr>),
}

pub type Stmt = Located<StmtKind>;

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
}
