use super::*;
use crate::test_utils::*;
use crate::value::{types::*, Value::*};

pub fn env_get(inter: &Interpreter, name: &str) -> ValueRes {
    inter.env.borrow().get(name, Loc::default())
}

fn test_value_res(input: &str, expected: ValueRes) {
    let expr = get_expr(input);
    let mut inter = Interpreter::new();
    match (inter.evaluate(&expr), expected) {
        (Ok(val), Ok(output)) if val.equal(&output).is_truthy() => {}
        (got, expected) => assert_eq!(expected, got),
    }
}

#[test]
fn test_integer_expr() {
    let expr = get_expr("1996");
    let mut inter = Interpreter::new();
    assert_eq!(Ok(Integer(1996)), inter.evaluate(&expr));
}

#[test]
fn test_float_expr() {
    let expr = get_expr("8.5");
    let mut inter = Interpreter::new();
    assert_eq!(Ok(Float(8.5)), inter.evaluate(&expr));
}

#[test]
fn test_string_expr() {
    let expr = get_expr("\"1996\"");
    let mut inter = Interpreter::new();
    assert_eq!(Ok("1996".into()), inter.evaluate(&expr));
}

#[test]
fn test_true_expr() {
    let expr = get_expr("true");
    let mut inter = Interpreter::new();
    assert_eq!(Ok(Boolean(true)), inter.evaluate(&expr));
}

#[test]
fn test_false_expr() {
    let expr = get_expr("false");
    let mut inter = Interpreter::new();
    assert_eq!(Ok(Boolean(false)), inter.evaluate(&expr));
}

#[test]
fn test_nil_expr() {
    let expr = get_expr("nil");
    let mut inter = Interpreter::new();
    assert_eq!(Ok(Nil), inter.evaluate(&expr));
}

#[test]
fn test_unary_not_expr() {
    for (input, output) in vec![
        ("!true", false),
        ("!false", true),
        ("!nil", true),
        ("!0", false),
        ("!0.5", false),
        ("!-100", false),
        ("!\"string\"", false),
        ("!\"\"", false),
    ] {
        let expr = get_expr(input);
        let mut inter = Interpreter::new();
        assert_eq!(Ok(Boolean(output)), inter.evaluate(&expr));
    }
}

#[test]
fn test_unary_negate_expr() {
    for (input, output) in vec![
        ("-100", Ok(Integer(-100))),
        ("-100.23", Ok(Float(-100.23))),
        ("-true", Err(unsup_op("-", BOOL, (0, 0)))),
        ("-nil", Err(unsup_op("-", NIL, (0, 0)))),
        ("-\"hi\"", Err(unsup_op("-", STRING, (0, 0)))),
    ] {
        test_value_res(input, output);
    }
}

#[test]
fn test_binary_add_expr() {
    for (input, output) in vec![
        ("30 + 50", Ok(Integer(80))),
        ("30 + 50.3", Ok(Float(80.3))),
        ("30.4 + 50.3", Ok(Float(80.7))),
        ("30.4 + 50", Ok(Float(80.4))),
        ("\"hello_\" + \"world\"", Ok("hello_world".into())),
        ("\"hello\" + 3", Err(unsup_ops("+", STRING, INT, (0, 8)))),
        (
            "\"hello\" + true",
            Err(unsup_ops("+", STRING, BOOL, (0, 8))),
        ),
        ("3.2 + false", Err(unsup_ops("+", FLOAT, BOOL, (0, 4)))),
        ("nil + 100", Err(unsup_ops("+", NIL, INT, (0, 4)))),
        ("true + false", Err(unsup_ops("+", BOOL, BOOL, (0, 5)))),
        ("nil + nil", Err(unsup_ops("+", NIL, NIL, (0, 4)))),
    ] {
        test_value_res(input, output);
    }
}

#[test]
fn test_binary_sub_expr() {
    for (input, output) in vec![
        ("30 - 50", Ok(Integer(-20))),
        ("30 - 50.3", Ok(Float(-20.3))),
        ("30.4 - 50.3", Ok(Float(-19.9))),
        ("30.4 - 50", Ok(Float(-19.6))),
        (
            "\"hello_\" - \"world\"",
            Err(unsup_ops("-", STRING, STRING, (0, 9))),
        ),
        ("\"hello\" - 3", Err(unsup_ops("-", STRING, INT, (0, 8)))),
        (
            "\"hello\" - true",
            Err(unsup_ops("-", STRING, BOOL, (0, 8))),
        ),
        ("3.2 - false", Err(unsup_ops("-", FLOAT, BOOL, (0, 4)))),
        ("nil - 100", Err(unsup_ops("-", NIL, INT, (0, 4)))),
        ("true - false", Err(unsup_ops("-", BOOL, BOOL, (0, 5)))),
        ("nil - nil", Err(unsup_ops("-", NIL, NIL, (0, 4)))),
    ] {
        test_value_res(input, output);
    }
}

#[test]
fn test_binary_mul_expr() {
    for (input, output) in vec![
        ("30 * 50", Ok(Integer(1500))),
        ("30 * 50.3", Ok(Float(1509.0))),
        ("30.4 * 50.3", Ok(Float(1529.12))),
        ("30.4 * 50", Ok(Float(1520.0))),
        (
            "\"hello_\" * \"world\"",
            Err(unsup_ops("*", STRING, STRING, (0, 9))),
        ),
        ("\"hello\" * 3", Err(unsup_ops("*", STRING, INT, (0, 8)))),
        (
            "\"hello\" * true",
            Err(unsup_ops("*", STRING, BOOL, (0, 8))),
        ),
        ("3.2 * false", Err(unsup_ops("*", FLOAT, BOOL, (0, 4)))),
        ("nil * 100", Err(unsup_ops("*", NIL, INT, (0, 4)))),
        ("true * false", Err(unsup_ops("*", BOOL, BOOL, (0, 5)))),
        ("nil * nil", Err(unsup_ops("*", NIL, NIL, (0, 4)))),
    ] {
        test_value_res(input, output);
    }
}

#[test]
fn test_binary_div_expr() {
    for (input, output) in vec![
        ("50 / 20", Ok(Integer(2))),
        ("30 / 50.0", Ok(Float(0.6))),
        ("30.4 / 50.3", Ok(Float(0.6043737574552684))),
        ("30.4 / 50", Ok(Float(0.608))),
        (
            "\"hello_\" / \"world\"",
            Err(unsup_ops("/", STRING, STRING, (0, 9))),
        ),
        ("\"hello\" / 3", Err(unsup_ops("/", STRING, INT, (0, 8)))),
        (
            "\"hello\" / true",
            Err(unsup_ops("/", STRING, BOOL, (0, 8))),
        ),
        ("3.2 / false", Err(unsup_ops("/", FLOAT, BOOL, (0, 4)))),
        ("nil / 100", Err(unsup_ops("/", NIL, INT, (0, 4)))),
        ("true / false", Err(unsup_ops("/", BOOL, BOOL, (0, 5)))),
        ("nil / nil", Err(unsup_ops("/", NIL, NIL, (0, 4)))),
    ] {
        test_value_res(input, output);
    }
}

#[test]
fn test_binary_rem_expr() {
    for (input, output) in vec![
        ("30 % 50", Ok(Integer(30))),
        ("30 % 5", Ok(Integer(0))),
        ("30 % 4.3", Ok(Float(4.2))),
        ("34.2 % 8.1", Ok(Float(1.8))),
        ("34.2 % 8", Ok(Float(2.2))),
        (
            "\"hello_\" % \"world\"",
            Err(unsup_ops("%", STRING, STRING, (0, 9))),
        ),
        ("\"hello\" % 3", Err(unsup_ops("%", STRING, INT, (0, 8)))),
        (
            "\"hello\" % true",
            Err(unsup_ops("%", STRING, BOOL, (0, 8))),
        ),
        ("3.2 % false", Err(unsup_ops("%", FLOAT, BOOL, (0, 4)))),
        ("nil % 100", Err(unsup_ops("%", NIL, INT, (0, 4)))),
        ("true % false", Err(unsup_ops("%", BOOL, BOOL, (0, 5)))),
        ("nil % nil", Err(unsup_ops("%", NIL, NIL, (0, 4)))),
    ] {
        test_value_res(input, output);
    }
}

#[test]
fn test_binary_comparison_expr() {
    for (input, output) in vec![
        ("50 == 20", Ok(false)),
        ("10 + 10 == 20", Ok(true)),
        ("30 == 50.0", Ok(false)),
        ("30 == 30.0", Ok(true)),
        ("30.4 == 50.3", Ok(false)),
        ("30.4 + 50.3 == 80.7", Ok(true)),
        ("30.4 == 50", Ok(false)),
        ("true == true", Ok(true)),
        ("true == !false", Ok(true)),
        ("\"hello\" == \"hell\" + \"o\"", Ok(true)),
        ("\"hello\" == 3", Ok(false)),
        ("nil == true", Ok(false)),
        ("nil == nil", Ok(true)),
        ("50 == 20", Ok(false)),
        ("10 + 10 != 20", Ok(false)),
        ("30 != 50.0", Ok(true)),
        ("30 != 30.0", Ok(false)),
        ("30.4 != 50.3", Ok(true)),
        ("30.4 + 50.3 != 80.7", Ok(false)),
        ("30.4 != 50", Ok(true)),
        ("true != true", Ok(false)),
        ("true != !false", Ok(false)),
        ("\"hello\" != \"hell\" + \"o\"", Ok(false)),
        ("\"hello\" != 3", Ok(true)),
        ("nil != true", Ok(true)),
        ("nil != nil", Ok(false)),
        ("50 > 20", Ok(true)),
        ("30 > 50.0", Ok(false)),
        ("30.4 > 50.3", Ok(false)),
        ("30.4 > 50", Ok(false)),
        ("50 >= 20", Ok(true)),
        ("30 >= 50.0", Ok(false)),
        ("30.4 >= 50.3", Ok(false)),
        ("30.4 >= 50", Ok(false)),
        ("30.4 >= 30.4", Ok(true)),
        ("50 < 20", Ok(false)),
        ("30 < 50.0", Ok(true)),
        ("30.4 < 50.3", Ok(true)),
        ("30.4 < 50", Ok(true)),
        ("50 <= 20", Ok(false)),
        ("30 <= 50.0", Ok(true)),
        ("30.4 <= 50.3", Ok(true)),
        ("30.4 <= 50", Ok(true)),
        ("30.4 <= 30.4", Ok(true)),
        (
            "\"hello_\" > \"world\"",
            Err(unsup_ops(">", STRING, STRING, (0, 9))),
        ),
        ("\"hello\" < 3", Err(unsup_ops("<", STRING, INT, (0, 8)))),
        (
            "\"hello\" >= true",
            Err(unsup_ops(">=", STRING, BOOL, (0, 8))),
        ),
        ("3.2 <= false", Err(unsup_ops("<=", FLOAT, BOOL, (0, 4)))),
        ("nil > 100", Err(unsup_ops(">", NIL, INT, (0, 4)))),
        ("true < false", Err(unsup_ops("<", BOOL, BOOL, (0, 5)))),
        ("nil >= nil", Err(unsup_ops(">=", NIL, NIL, (0, 4)))),
        (
            "\"hello_\" <= \"world\"",
            Err(unsup_ops("<=", STRING, STRING, (0, 9))),
        ),
        ("\"hello\" > 3", Err(unsup_ops(">", STRING, INT, (0, 8)))),
        (
            "\"hello\" < true",
            Err(unsup_ops("<", STRING, BOOL, (0, 8))),
        ),
        ("3.2 >= false", Err(unsup_ops(">=", FLOAT, BOOL, (0, 4)))),
        ("nil <= 100", Err(unsup_ops("<=", NIL, INT, (0, 4)))),
    ] {
        let output = output.map(Boolean);
        test_value_res(input, output);
    }
}

#[test]
fn test_logical_expr() {
    for (input, output) in vec![
        ("true and 5", Integer(5)),
        ("3.0 and nil", Nil),
        ("false and 5", Boolean(false)),
        ("nil and false", Nil),
        ("3.0 or 5", Float(3.0)),
        ("\"string\" or false", "string".into()),
        ("nil or \"car\"", "car".into()),
        ("false or false", Boolean(false)),
    ] {
        test_value_res(input, Ok(output));
    }
}

#[test]
fn test_logical_short_circuit() {
    for (input, output) in vec![
        ("true and (hello = 5)", Integer(5)),
        ("3.0 and (hello = nil)", Nil),
        ("false and (hello = 5)", "world".into()),
        ("nil and (hello = false)", "world".into()),
        ("3.0 or (hello = 5)", "world".into()),
        ("\"string\" or (hello = false)", "world".into()),
        ("nil or (hello = \"car\")", "car".into()),
        ("false or (hello = false)", Boolean(false)),
    ] {
        let input = format!(r#"var hello = "world"; {};"#, input);
        let stmts = get_stmts(&input);
        let mut inter = Interpreter::new();
        assert_eq!(Ok(()), inter.interpret(&stmts));
        assert_eq!(Ok(output), env_get(&inter, "hello"));
    }
}

#[test]
fn test_conditional_expr() {
    for (input, output) in vec![
        ("true ? 30 : 50", Integer(30)),
        ("\"string\" ? 1.0 : 200", Float(1.0)),
        ("!3 ? (3 + 5) : (88/11)", Integer(8)),
        ("nil? true ? 1 : 2 : 0.0 ? 3 : 4", Integer(3)),
        ("true ? 30 : 50, false ? 7 : 9", Integer(9)),
    ] {
        test_value_res(input, Ok(output));
    }
}

#[test]
fn test_comma_expr() {
    for (input, output) in vec![
        ("3434 + 76, (5 * 8 + 2)", Integer(42)),
        ("30, \"string\", true", Boolean(true)),
        ("(3 + 5), (4, (7, 88/11))", Integer(8)),
    ] {
        test_value_res(input, Ok(output));
    }
}

#[test]
fn test_division_by_zero() {
    for (input, col) in vec![
        ("30/0", 2),
        ("30/0.0", 2),
        ("30.0/0.0", 4),
        ("30.0/0", 4),
        ("30%0", 2),
        ("30%0.0", 2),
        ("30.0%0.0", 4),
        ("30.0%0", 4),
    ] {
        let expr = get_expr(input);
        let mut inter = Interpreter::new();
        assert_eq!(
            Err(RuntimeError::DivisionByZero(Loc::new(0, col),)),
            inter.evaluate(&expr)
        );
    }
}

#[test]
fn test_empty_stmt() {
    let input = "";
    let stmts = get_stmts(input);
    let mut inter = Interpreter::new();
    assert_eq!(Ok(()), inter.interpret(&stmts));
}

#[test]
fn test_var_stmt() {
    let input = r#"var hello = "world";"#;
    let stmts = get_stmts(input);
    let mut inter = Interpreter::new();
    assert_eq!(Ok(()), inter.interpret(&stmts));
    assert_eq!(Ok("world".into()), env_get(&inter, "hello"));
}

#[test]
fn test_var_assignment() {
    let input = r#"var hello = "world";
    hello = 1 + 1;
    hello = "Earth";"#;
    let stmts = get_stmts(input);
    let mut inter = Interpreter::new();
    assert_eq!(Ok(()), inter.interpret(&stmts));
    assert_eq!(Ok("Earth".into()), env_get(&inter, "hello"));
}

#[test]
fn test_var_assignment_in_block() {
    let input = r#"var hello = "world";
    {
        1 + 1; // This does nothing
        hello = "Earth";
    }"#;
    let stmts = get_stmts(input);
    let mut inter = Interpreter::new();
    assert_eq!(Ok(()), inter.interpret(&stmts));
    assert_eq!(Ok("Earth".into()), env_get(&inter, "hello"));
}

#[test]
fn test_var_shadowing() {
    let input = r#"var hello = "world";
    {
        var hello = "Earth";
        hello = "planet";
    }"#;
    let stmts = get_stmts(input);
    let mut inter = Interpreter::new();
    assert_eq!(Ok(()), inter.interpret(&stmts));
    assert_eq!(Ok("world".into()), env_get(&inter, "hello"));
}

#[test]
fn test_if_stmt_true() {
    let input = r#"var hello;
    if (3 > 2) {
        hello = "world";
    }"#;
    let stmts = get_stmts(input);
    let mut inter = Interpreter::new();
    assert_eq!(Ok(()), inter.interpret(&stmts));
    assert_eq!(Ok("world".into()), env_get(&inter, "hello"));
}

#[test]
fn test_if_stmt_false() {
    let input = r#"var hello;
    if (3 < 2) {
        hello = "world";
    }"#;
    let stmts = get_stmts(input);
    let mut inter = Interpreter::new();
    assert_eq!(Ok(()), inter.interpret(&stmts));
    assert_eq!(Ok(Nil), env_get(&inter, "hello"));
}

#[test]
fn test_if_else_stmt_true() {
    let input = r#"var hello;
    if (3 > 2) {
        hello = "world";
    } else {
        hello = "Earth";
    }"#;
    let stmts = get_stmts(input);
    let mut inter = Interpreter::new();
    assert_eq!(Ok(()), inter.interpret(&stmts));
    assert_eq!(Ok("world".into()), env_get(&inter, "hello"));
}

#[test]
fn test_if_else_stmt_false() {
    let input = r#"var hello;
    if (3 < 2) {
        hello = "world";
    } else {
        hello = "Earth";
    }"#;
    let stmts = get_stmts(input);
    let mut inter = Interpreter::new();
    assert_eq!(Ok(()), inter.interpret(&stmts));
    assert_eq!(Ok("Earth".into()), env_get(&inter, "hello"));
}

#[test]
fn test_var_print() {
    let input = r#"var hello = "world";
    print hello;"#;
    let stmts = get_stmts(input);
    let mut inter = Interpreter::new();
    assert_eq!(Ok(()), inter.interpret(&stmts));
}

#[test]
fn test_undefined_variable() {
    let input = r#"hello;"#;
    let stmts = get_stmts(input);
    let mut inter = Interpreter::new();
    assert_eq!(
        Err(RuntimeError::UndefinedVariable(
            Loc::new(0, 0),
            String::from("hello")
        )),
        inter.interpret(&stmts)
    );
}

#[test]
fn test_undefined_variable_in_assignment() {
    let input = r#"hello = "world";"#;
    let stmts = get_stmts(input);
    let mut inter = Interpreter::new();
    assert_eq!(
        Err(RuntimeError::UndefinedVariable(
            Loc::new(0, 0),
            String::from("hello")
        )),
        inter.interpret(&stmts)
    );
}
