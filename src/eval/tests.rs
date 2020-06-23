use super::*;
use crate::test_utils::*;
use crate::value::Value::*;

#[test]
fn test_integer_expr() {
    let expr = get_expr("1996");
    let mut inter = Interpreter::new();
    assert_eq!(Ok(Integer(1996)), expr.evaluate(&mut inter));
}

#[test]
fn test_float_expr() {
    let expr = get_expr("8.5");
    let mut inter = Interpreter::new();
    assert_eq!(Ok(Float(8.5)), expr.evaluate(&mut inter));
}

#[test]
fn test_string_expr() {
    let expr = get_expr("\"1996\"");
    let mut inter = Interpreter::new();
    assert_eq!(Ok(Str(String::from("1996"))), expr.evaluate(&mut inter));
}

#[test]
fn test_true_expr() {
    let expr = get_expr("true");
    let mut inter = Interpreter::new();
    assert_eq!(Ok(Boolean(true)), expr.evaluate(&mut inter));
}

#[test]
fn test_false_expr() {
    let expr = get_expr("false");
    let mut inter = Interpreter::new();
    assert_eq!(Ok(Boolean(false)), expr.evaluate(&mut inter));
}

#[test]
fn test_nil_expr() {
    let expr = get_expr("nil");
    let mut inter = Interpreter::new();
    assert_eq!(Ok(Nil), expr.evaluate(&mut inter));
}

#[test]
fn test_unary_not_expr() {
    for (input, output) in &[
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
        assert_eq!(Ok(Boolean(*output)), expr.evaluate(&mut inter));
    }
}

#[test]
fn test_unary_negate_expr() {
    let mut inter = Interpreter::new();
    let expr = get_expr("-100");
    assert_eq!(Ok(Integer(-100)), expr.evaluate(&mut inter));
    let expr = get_expr("-100.23");
    assert_eq!(Ok(Float(-100.23)), expr.evaluate(&mut inter));
}

#[test]
fn test_binary_add_expr() {
    for (input, output) in &[
        ("30 + 50", Integer(80)),
        ("30 + 50.3", Float(80.3)),
        ("30.4 + 50.3", Float(80.7)),
        ("30.4 + 50", Float(80.4)),
        ("\"hello_\" + \"world\"", Str(String::from("hello_world"))),
    ] {
        let expr = get_expr(input);
        let mut inter = Interpreter::new();
        let val = expr.evaluate(&mut inter).unwrap();
        if !val.equal(output).is_truthy() {
            assert_eq!(*output, val);
        }
    }
}

#[test]
fn test_binary_sub_expr() {
    for (input, output) in &[
        ("30 - 50", Integer(-20)),
        ("30 - 50.3", Float(-20.3)),
        ("30.4 - 50.3", Float(-19.9)),
        ("30.4 - 50", Float(-19.6)),
    ] {
        let expr = get_expr(input);
        let mut inter = Interpreter::new();
        let val = expr.evaluate(&mut inter).unwrap();
        if !val.equal(output).is_truthy() {
            assert_eq!(*output, val);
        }
    }
}

#[test]
fn test_binary_mul_expr() {
    for (input, output) in &[
        ("30 * 50", Integer(1500)),
        ("30 * 50.3", Float(1509.0)),
        ("30.4 * 50.3", Float(1529.12)),
        ("30.4 * 50", Float(1520.0)),
    ] {
        let expr = get_expr(input);
        let mut inter = Interpreter::new();
        let val = expr.evaluate(&mut inter).unwrap();
        if !val.equal(output).is_truthy() {
            assert_eq!(*output, val);
        }
    }
}

#[test]
fn test_binary_div_expr() {
    for (input, output) in &[
        ("50 / 20", Integer(2)),
        ("30 / 50.0", Float(0.6)),
        ("30.4 / 50.3", Float(0.6043737574552684)),
        ("30.4 / 50", Float(0.608)),
    ] {
        let expr = get_expr(input);
        let mut inter = Interpreter::new();
        let val = expr.evaluate(&mut inter).unwrap();
        if !val.equal(output).is_truthy() {
            assert_eq!(*output, val);
        }
    }
}

#[test]
fn test_binary_rem_expr() {
    for (input, output) in &[
        ("30 % 50", Integer(30)),
        ("30 % 5", Integer(0)),
        ("30 % 4.3", Float(4.2)),
        ("34.2 % 8.1", Float(1.8)),
        ("34.2 % 8", Float(2.2)),
    ] {
        let expr = get_expr(input);
        let mut inter = Interpreter::new();
        let val = expr.evaluate(&mut inter).unwrap();
        if !val.equal(output).is_truthy() {
            assert_eq!(*output, val);
        }
    }
}

#[test]
fn test_binary_comparison_expr() {
    for (input, output) in [
        ("50 == 20", false),
        ("10 + 10 == 20", true),
        ("30 == 50.0", false),
        ("30 == 30.0", true),
        ("30.4 == 50.3", false),
        ("30.4 + 50.3 == 80.7", true),
        ("30.4 == 50", false),
        ("true == true", true),
        ("true == !false", true),
        ("\"hello\" == \"hell\" + \"o\"", true),
        ("\"hello\" == 3", false),
        ("nil == true", false),
        ("nil == nil", true),
        ("50 == 20", false),
        ("10 + 10 != 20", false),
        ("30 != 50.0", true),
        ("30 != 30.0", false),
        ("30.4 != 50.3", true),
        ("30.4 + 50.3 != 80.7", false),
        ("30.4 != 50", true),
        ("true != true", false),
        ("true != !false", false),
        ("\"hello\" != \"hell\" + \"o\"", false),
        ("\"hello\" != 3", true),
        ("nil != true", true),
        ("nil != nil", false),
        ("50 > 20", true),
        ("30 > 50.0", false),
        ("30.4 > 50.3", false),
        ("30.4 > 50", false),
        ("50 >= 20", true),
        ("30 >= 50.0", false),
        ("30.4 >= 50.3", false),
        ("30.4 >= 50", false),
        ("30.4 >= 30.4", true),
        ("50 < 20", false),
        ("30 < 50.0", true),
        ("30.4 < 50.3", true),
        ("30.4 < 50", true),
        ("50 <= 20", false),
        ("30 <= 50.0", true),
        ("30.4 <= 50.3", true),
        ("30.4 <= 50", true),
        ("30.4 <= 30.4", true),
    ]
    .iter()
    {
        let expr = get_expr(input);
        let mut inter = Interpreter::new();
        let val = expr.evaluate(&mut inter).unwrap();
        let output = Boolean(*output);
        if !val.equal(&output).is_truthy() {
            assert_eq!(output, val);
        }
    }
}

#[test]
fn test_conditional_expr() {
    for (input, output) in &[
        ("true ? 30 : 50", Integer(30)),
        ("\"string\" ? 1.0 : 200", Float(1.0)),
        ("!3 ? (3 + 5) : (88/11)", Integer(8)),
        ("nil? true ? 1 : 2 : 0.0 ? 3 : 4", Integer(3)),
        ("true ? 30 : 50, false ? 7 : 9", Integer(9)),
    ] {
        let expr = get_expr(input);
        let mut inter = Interpreter::new();
        let val = expr.evaluate(&mut inter).unwrap();
        if !val.equal(output).is_truthy() {
            assert_eq!(*output, val);
        }
    }
}

#[test]
fn test_comma_expr() {
    for (input, output) in &[
        ("3434 + 76, (5 * 8 + 2)", Integer(42)),
        ("30, \"string\", true", Boolean(true)),
        ("(3 + 5), (4, (7, 88/11))", Integer(8)),
    ] {
        let expr = get_expr(input);
        let mut inter = Interpreter::new();
        let val = expr.evaluate(&mut inter).unwrap();
        if !val.equal(output).is_truthy() {
            assert_eq!(*output, val);
        }
    }
}
