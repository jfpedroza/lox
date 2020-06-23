use crate::expr::{BinOp, Expr, ExprKind, LitExpr, UnOp};
use crate::lexer::{Scanner, Token};
use crate::location::Loc;
use crate::parser::Parser;

pub fn get_tokens<'a>(input: &'a str) -> Vec<Token<'a>> {
    let mut scanner = Scanner::new(input);
    scanner.scan_tokens().unwrap()
}

pub fn get_expr(input: &str) -> Expr {
    let tokens = get_tokens(input);
    let mut parser = Parser::new(&tokens);
    parser.expression().unwrap()
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
