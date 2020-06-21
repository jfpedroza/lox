use super::*;
use crate::expr::{BinaryOp, Expr, LiteralExpr, UnaryOp};
use crate::lexer::{Scanner, Token};
use crate::location::Location;

fn get_tokens<'a>(input: &'a str) -> Vec<Token<'a>> {
    let mut scanner = Scanner::new(input);
    scanner.scan_tokens().unwrap()
}

#[test]
fn test_integer_expr() {
    let tokens = get_tokens("1996");
    let mut parser = Parser::new(&tokens);
    assert_eq!(Ok(int_expr(1996)), parser.expression());
}

#[test]
fn test_float_expr() {
    let tokens = get_tokens("8.5");
    let mut parser = Parser::new(&tokens);
    assert_eq!(Ok(float_expr(8.5)), parser.expression());
}

#[test]
fn test_string_expr() {
    let tokens = get_tokens("\"1996\"");
    let mut parser = Parser::new(&tokens);
    assert_eq!(Ok(str_expr("1996")), parser.expression());
}

#[test]
fn test_true_expr() {
    let tokens = get_tokens("true");
    let mut parser = Parser::new(&tokens);
    assert_eq!(Ok(bool_expr(true)), parser.expression());
}

#[test]
fn test_false_expr() {
    let tokens = get_tokens("false");
    let mut parser = Parser::new(&tokens);
    assert_eq!(Ok(bool_expr(false)), parser.expression());
}

#[test]
fn test_nil_expr() {
    let tokens = get_tokens("nil");
    let mut parser = Parser::new(&tokens);
    assert_eq!(Ok(nil_expr()), parser.expression());
}

#[test]
fn test_unary_not_expr() {
    let tokens = get_tokens("!true");
    let mut parser = Parser::new(&tokens);
    assert_eq!(Ok(not_expr(bool_expr(true))), parser.expression());
}

#[test]
fn test_unary_negate_expr() {
    let tokens = get_tokens("-100");
    let mut parser = Parser::new(&tokens);
    assert_eq!(Ok(neg_expr(int_expr(100))), parser.expression());
}

#[test]
fn test_binary_add_expr() {
    let tokens = get_tokens("30 + 100");
    let mut parser = Parser::new(&tokens);
    assert_eq!(
        Ok(add_expr(int_expr(30), int_expr(100))),
        parser.expression()
    );
}

#[test]
fn test_binary_sub_expr() {
    let tokens = get_tokens("30.5 - 100");
    let mut parser = Parser::new(&tokens);
    assert_eq!(
        Ok(sub_expr(float_expr(30.5), int_expr(100))),
        parser.expression()
    );
}

#[test]
fn test_binary_mult_expr() {
    let tokens = get_tokens("30*100");
    let mut parser = Parser::new(&tokens);
    assert_eq!(
        Ok(mult_expr(int_expr(30), int_expr(100))),
        parser.expression()
    );
}

#[test]
fn test_binary_div_expr() {
    let tokens = get_tokens("30/100");
    let mut parser = Parser::new(&tokens);
    assert_eq!(
        Ok(div_expr(int_expr(30), int_expr(100))),
        parser.expression()
    );
}

#[test]
fn test_binary_rem_expr() {
    let tokens = get_tokens("30 % 100");
    let mut parser = Parser::new(&tokens);
    assert_eq!(
        Ok(rem_expr(int_expr(30), int_expr(100))),
        parser.expression()
    );
}

#[test]
fn test_binary_equal_expr() {
    let tokens = get_tokens("30 == 100");
    let mut parser = Parser::new(&tokens);
    assert_eq!(
        Ok(eq_expr(int_expr(30), int_expr(100))),
        parser.expression()
    );
}

#[test]
fn test_binary_not_equal_expr() {
    let tokens = get_tokens("30 != 100");
    let mut parser = Parser::new(&tokens);
    assert_eq!(
        Ok(not_eq_expr(int_expr(30), int_expr(100))),
        parser.expression()
    );
}

#[test]
fn test_binary_greater_expr() {
    let tokens = get_tokens("30 > 100");
    let mut parser = Parser::new(&tokens);
    assert_eq!(
        Ok(gt_expr(int_expr(30), int_expr(100))),
        parser.expression()
    );
}

#[test]
fn test_binary_greater_equal_expr() {
    let tokens = get_tokens("30 >= 100");
    let mut parser = Parser::new(&tokens);
    assert_eq!(
        Ok(gt_eq_expr(int_expr(30), int_expr(100))),
        parser.expression()
    );
}

#[test]
fn test_binary_less_expr() {
    let tokens = get_tokens("30 < 100");
    let mut parser = Parser::new(&tokens);
    assert_eq!(
        Ok(less_expr(int_expr(30), int_expr(100))),
        parser.expression()
    );
}

#[test]
fn test_binary_less_equal_expr() {
    let tokens = get_tokens("30 <= 100");
    let mut parser = Parser::new(&tokens);
    assert_eq!(
        Ok(less_eq_expr(int_expr(30), int_expr(100))),
        parser.expression()
    );
}

#[test]
fn test_group_expr() {
    let tokens = get_tokens("(30 / 100)");
    let mut parser = Parser::new(&tokens);
    assert_eq!(
        Ok(group_expr(div_expr(int_expr(30), int_expr(100)))),
        parser.expression()
    );
}

#[test]
fn test_comma_expr() {
    let tokens = get_tokens("30 / 100, 2*5.0, true");
    let mut parser = Parser::new(&tokens);
    assert_eq!(
        Ok(comma_expr(
            div_expr(int_expr(30), int_expr(100)),
            comma_expr(mult_expr(int_expr(2), float_expr(5.0)), bool_expr(true))
        )),
        parser.expression()
    );
}

#[test]
fn test_conditional_expr() {
    let tokens = get_tokens(r#"true ? "hello" : "world""#);
    let mut parser = Parser::new(&tokens);
    assert_eq!(
        Ok(cond_expr(
            bool_expr(true),
            str_expr("hello"),
            str_expr("world")
        )),
        parser.expression()
    );
}

#[test]
fn test_nested_conditional_expr() {
    let tokens = get_tokens(r#"true ? "hello" ? 1 : 2 : "world" ? 3 : 4"#);
    let mut parser = Parser::new(&tokens);
    assert_eq!(
        Ok(cond_expr(
            bool_expr(true),
            cond_expr(str_expr("hello"), int_expr(1), int_expr(2)),
            cond_expr(str_expr("world"), int_expr(3), int_expr(4))
        )),
        parser.expression()
    );
}

#[test]
fn test_nested_expr() {
    let tokens = get_tokens(r#"2 > 3 ? "yes" : (1e4 + 29 - 3.1416) * 2/1.0"#);
    let mut parser = Parser::new(&tokens);
    assert_eq!(
        Ok(cond_expr(
            gt_expr(int_expr(2), int_expr(3)),
            str_expr("yes"),
            div_expr(
                mult_expr(
                    group_expr(sub_expr(
                        add_expr(float_expr(10000.0), int_expr(29)),
                        float_expr(3.1416)
                    )),
                    int_expr(2)
                ),
                float_expr(1.0)
            )
        )),
        parser.expression()
    );
}

#[test]
fn test_missing_close_paren() {
    let tokens = get_tokens("(100");
    let mut parser = Parser::new(&tokens);
    assert_eq!(
        Err(ParsingError::ExpectedCloseParen(
            Location::new(0, 4),
            String::from("EOF")
        )),
        parser.expression()
    );
}

#[test]
fn test_missing_colon() {
    let tokens = get_tokens("true ? 100;");
    let mut parser = Parser::new(&tokens);
    assert_eq!(
        Err(ParsingError::ExpectedColon(
            Location::new(0, 10),
            String::from(";")
        )),
        parser.expression()
    );
}

#[test]
fn test_expected_expression() {
    let tokens = get_tokens("if");
    let mut parser = Parser::new(&tokens);
    assert_eq!(
        Err(ParsingError::ExpectedExpression(
            Location::new(0, 0),
            String::from("if")
        )),
        parser.expression()
    );
}

fn int_expr(int: i64) -> Expr {
    Expr::Literal(LiteralExpr::Integer(int))
}

fn float_expr(float: f64) -> Expr {
    Expr::Literal(LiteralExpr::Float(float))
}

fn str_expr(string: &str) -> Expr {
    Expr::Literal(LiteralExpr::Str(String::from(string)))
}

fn bool_expr(boolean: bool) -> Expr {
    Expr::Literal(LiteralExpr::Boolean(boolean))
}

fn nil_expr() -> Expr {
    Expr::Literal(LiteralExpr::Nil)
}

fn not_expr(expr: Expr) -> Expr {
    Expr::unary(UnaryOp::Not, expr)
}

fn neg_expr(expr: Expr) -> Expr {
    Expr::unary(UnaryOp::Negate, expr)
}

fn add_expr(left: Expr, right: Expr) -> Expr {
    Expr::binary(left, BinaryOp::Add, right)
}

fn sub_expr(left: Expr, right: Expr) -> Expr {
    Expr::binary(left, BinaryOp::Sub, right)
}

fn mult_expr(left: Expr, right: Expr) -> Expr {
    Expr::binary(left, BinaryOp::Mult, right)
}

fn div_expr(left: Expr, right: Expr) -> Expr {
    Expr::binary(left, BinaryOp::Div, right)
}

fn rem_expr(left: Expr, right: Expr) -> Expr {
    Expr::binary(left, BinaryOp::Rem, right)
}

fn eq_expr(left: Expr, right: Expr) -> Expr {
    Expr::binary(left, BinaryOp::Equal, right)
}

fn not_eq_expr(left: Expr, right: Expr) -> Expr {
    Expr::binary(left, BinaryOp::NotEqual, right)
}

fn gt_expr(left: Expr, right: Expr) -> Expr {
    Expr::binary(left, BinaryOp::Greater, right)
}

fn gt_eq_expr(left: Expr, right: Expr) -> Expr {
    Expr::binary(left, BinaryOp::GreaterEqual, right)
}

fn less_expr(left: Expr, right: Expr) -> Expr {
    Expr::binary(left, BinaryOp::Less, right)
}

fn less_eq_expr(left: Expr, right: Expr) -> Expr {
    Expr::binary(left, BinaryOp::LessEqual, right)
}

fn group_expr(expr: Expr) -> Expr {
    Expr::groping(expr)
}

fn comma_expr(left: Expr, right: Expr) -> Expr {
    Expr::comma(left, right)
}

fn cond_expr(cond: Expr, left: Expr, right: Expr) -> Expr {
    Expr::conditional(cond, left, right)
}
