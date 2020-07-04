use crate::eval::RuntimeError;
use crate::expr::{BinOp, Expr, ExprKind, LitExpr, UnOp};
use crate::lexer::{Scanner, Token};
use crate::location::Loc;
use crate::parser::Parser;
use crate::stmt::Stmt;

pub fn get_tokens<'a>(input: &'a str) -> Vec<Token<'a>> {
    let mut scanner = Scanner::new(input);
    scanner.scan_tokens().unwrap()
}

pub fn get_expr(input: &str) -> Expr {
    let tokens = get_tokens(input);
    let mut parser = Parser::new(&tokens);
    parser.expression().unwrap()
}

pub fn get_stmts(input: &str) -> Vec<Stmt> {
    let tokens = get_tokens(input);
    let mut parser = Parser::new(&tokens);
    parser.parse().unwrap()
}

pub fn int_expr(int: i64, (line, col): (usize, usize)) -> Expr {
    Expr::new(
        ExprKind::Literal(LitExpr::Integer(int)),
        Loc::new(line, col),
    )
}

pub fn float_expr(float: f64, (line, col): (usize, usize)) -> Expr {
    Expr::new(
        ExprKind::Literal(LitExpr::Float(float)),
        Loc::new(line, col),
    )
}

pub fn str_expr(string: &str, (line, col): (usize, usize)) -> Expr {
    Expr::new(
        ExprKind::Literal(LitExpr::Str(String::from(string))),
        Loc::new(line, col),
    )
}

pub fn bool_expr(boolean: bool, (line, col): (usize, usize)) -> Expr {
    Expr::new(
        ExprKind::Literal(LitExpr::Boolean(boolean)),
        Loc::new(line, col),
    )
}

pub fn nil_expr((line, col): (usize, usize)) -> Expr {
    Expr::new(ExprKind::Literal(LitExpr::Nil), Loc::new(line, col))
}

pub fn not_expr(expr: Expr, (line, col): (usize, usize)) -> Expr {
    Expr::unary(UnOp::Not, expr, Loc::new(line, col))
}

pub fn neg_expr(expr: Expr, (line, col): (usize, usize)) -> Expr {
    Expr::unary(UnOp::Negate, expr, Loc::new(line, col))
}

pub fn add_expr(left: Expr, right: Expr, (line, col): (usize, usize)) -> Expr {
    Expr::binary(left, BinOp::Add, right, Loc::new(line, col))
}

pub fn sub_expr(left: Expr, right: Expr, (line, col): (usize, usize)) -> Expr {
    Expr::binary(left, BinOp::Sub, right, Loc::new(line, col))
}

pub fn mult_expr(left: Expr, right: Expr, (line, col): (usize, usize)) -> Expr {
    Expr::binary(left, BinOp::Mul, right, Loc::new(line, col))
}

pub fn div_expr(left: Expr, right: Expr, (line, col): (usize, usize)) -> Expr {
    Expr::binary(left, BinOp::Div, right, Loc::new(line, col))
}

pub fn rem_expr(left: Expr, right: Expr, (line, col): (usize, usize)) -> Expr {
    Expr::binary(left, BinOp::Rem, right, Loc::new(line, col))
}

pub fn eq_expr(left: Expr, right: Expr, (line, col): (usize, usize)) -> Expr {
    Expr::binary(left, BinOp::Equal, right, Loc::new(line, col))
}

pub fn not_eq_expr(left: Expr, right: Expr, (line, col): (usize, usize)) -> Expr {
    Expr::binary(left, BinOp::NotEqual, right, Loc::new(line, col))
}

pub fn gt_expr(left: Expr, right: Expr, (line, col): (usize, usize)) -> Expr {
    Expr::binary(left, BinOp::Greater, right, Loc::new(line, col))
}

pub fn gt_eq_expr(left: Expr, right: Expr, (line, col): (usize, usize)) -> Expr {
    Expr::binary(left, BinOp::GreaterEqual, right, Loc::new(line, col))
}

pub fn less_expr(left: Expr, right: Expr, (line, col): (usize, usize)) -> Expr {
    Expr::binary(left, BinOp::Less, right, Loc::new(line, col))
}

pub fn less_eq_expr(left: Expr, right: Expr, (line, col): (usize, usize)) -> Expr {
    Expr::binary(left, BinOp::LessEqual, right, Loc::new(line, col))
}

pub fn group_expr(expr: Expr, (line, col): (usize, usize)) -> Expr {
    Expr::groping(expr, Loc::new(line, col))
}

pub fn comma_expr(left: Expr, right: Expr, (line, col): (usize, usize)) -> Expr {
    Expr::comma(left, right, Loc::new(line, col))
}

pub fn cond_expr(cond: Expr, left: Expr, right: Expr, (line, col): (usize, usize)) -> Expr {
    Expr::conditional(cond, left, right, Loc::new(line, col))
}

pub fn var_expr(name: &str, (line, col): (usize, usize)) -> Expr {
    Expr::variable(name, Loc::new(line, col))
}

pub fn assign_expr(name: &str, expr: Expr, (line, col): (usize, usize)) -> Expr {
    Expr::assign(String::from(name), expr, Loc::new(line, col))
}

pub fn expr_stmt(expr: Expr, (line, col): (usize, usize)) -> Stmt {
    Stmt::expression(expr, Loc::new(line, col))
}

pub fn print_stmt(expr: Expr, (line, col): (usize, usize)) -> Stmt {
    Stmt::print(expr, Loc::new(line, col))
}

pub fn var_stmt(name: &str, init: Option<Expr>, (line, col): (usize, usize)) -> Stmt {
    Stmt::var(name, init, Loc::new(line, col))
}

pub fn block_stmt(stmts: Vec<Stmt>, (line, col): (usize, usize)) -> Stmt {
    Stmt::block(stmts, Loc::new(line, col))
}

pub fn unsup_op(op: &str, operand_type: &str, (line, col): (usize, usize)) -> RuntimeError {
    RuntimeError::UnsupportedOperand(
        Loc::new(line, col),
        String::from(op),
        String::from(operand_type),
    )
}

pub fn unsup_ops(
    op: &str,
    left_type: &str,
    right_type: &str,
    (line, col): (usize, usize),
) -> RuntimeError {
    RuntimeError::UnsupportedOperands(
        Loc::new(line, col),
        String::from(op),
        String::from(left_type),
        String::from(right_type),
    )
}
