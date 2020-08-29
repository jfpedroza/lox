use super::*;
use crate::callable::Callable;
use crate::test_utils::*;
use crate::value::{types::*, Value::*};

pub fn env_get(inter: &Interpreter, name: &str) -> ValueRes {
    inter.globals.borrow().get(name, Loc::default())
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
        let (stmts, mut inter) = get_stmts(&input);
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
    let (stmts, mut inter) = get_stmts(input);
    assert_eq!(Ok(()), inter.interpret(&stmts));
}

#[test]
fn test_var_stmt() {
    let input = r#"var hello = "world";"#;
    let (stmts, mut inter) = get_stmts(input);
    assert_eq!(Ok(()), inter.interpret(&stmts));
    assert_eq!(Ok("world".into()), env_get(&inter, "hello"));
}

#[test]
fn test_var_assignment() {
    let input = r#"var hello = "world";
    hello = 1 + 1;
    hello = "Earth";"#;
    let (stmts, mut inter) = get_stmts(input);
    assert_eq!(Ok(()), inter.interpret(&stmts));
    assert_eq!(Ok("Earth".into()), env_get(&inter, "hello"));
}

#[test]
fn test_var_assignment2() {
    let input = r#"var i = 0;
    i += 1;
    i -= 3;
    i *= -15;
    i /= 3;
    i %= 4;
    var j = ++i;
    var k = --i;
    var l = k--;
    var m = j++;
    "#;
    let (stmts, mut inter) = get_stmts(input);
    assert_eq!(Ok(()), inter.interpret(&stmts));
    assert_eq!(Ok(2.into()), env_get(&inter, "i"));
    assert_eq!(Ok(4.into()), env_get(&inter, "j"));
    assert_eq!(Ok(1.into()), env_get(&inter, "k"));
    assert_eq!(Ok(2.into()), env_get(&inter, "l"));
    assert_eq!(Ok(3.into()), env_get(&inter, "m"));
}

#[test]
fn test_var_assignment_in_block() {
    let input = r#"var hello = "world";
    {
        1 + 1; // This does nothing
        hello = "Earth";
    }"#;
    let (stmts, mut inter) = get_stmts(input);
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
    let (stmts, mut inter) = get_stmts(input);
    assert_eq!(Ok(()), inter.interpret(&stmts));
    assert_eq!(Ok("world".into()), env_get(&inter, "hello"));
}

#[test]
fn test_if_stmt_true() {
    let input = r#"var hello;
    if (3 > 2) {
        hello = "world";
    }"#;
    let (stmts, mut inter) = get_stmts(input);
    assert_eq!(Ok(()), inter.interpret(&stmts));
    assert_eq!(Ok("world".into()), env_get(&inter, "hello"));
}

#[test]
fn test_if_stmt_false() {
    let input = r#"var hello;
    if (3 < 2) {
        hello = "world";
    }"#;
    let (stmts, mut inter) = get_stmts(input);
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
    let (stmts, mut inter) = get_stmts(input);
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
    let (stmts, mut inter) = get_stmts(input);
    assert_eq!(Ok(()), inter.interpret(&stmts));
    assert_eq!(Ok("Earth".into()), env_get(&inter, "hello"));
}

#[test]
fn test_var_print() {
    let input = r#"var hello = "world";
    print hello;"#;
    let (stmts, mut inter) = get_stmts(input);
    assert_eq!(Ok(()), inter.interpret(&stmts));
}

#[test]
fn test_while_stmt() {
    let input = r#"var i = 0;
    while (i < 10) {
        i += 1;
    }"#;
    let (stmts, mut inter) = get_stmts(input);
    assert_eq!(Ok(()), inter.interpret(&stmts));
    assert_eq!(Ok(10.into()), env_get(&inter, "i"));
}

#[test]
fn test_for_stmt() {
    let input = r#"var i;
    for (i = 0; i < 10; ++i) {}"#;
    let (stmts, mut inter) = get_stmts(input);
    assert_eq!(Ok(()), inter.interpret(&stmts));
    assert_eq!(Ok(10.into()), env_get(&inter, "i"));
}

#[test]
fn test_for_break_stmt() {
    let input = r#"var i;
    for (i = 10; ; --i) {
        if (i == 0) break;
    }"#;
    let (stmts, mut inter) = get_stmts(input);
    assert_eq!(Ok(()), inter.interpret(&stmts));
    assert_eq!(Ok(0.into()), env_get(&inter, "i"));
}

#[test]
fn test_function_stmt() {
    let input = r#"
    fun my_fun() {}
    "#;
    let (stmts, mut inter) = get_stmts(input);
    assert_eq!(Ok(()), inter.interpret(&stmts));
    let fun = env_get(&inter, "my_fun").unwrap();
    match fun {
        Value::Callable(Callable::Function(fun)) => {
            assert_eq!(fun.name, Some(String::from("my_fun")));
        }
        val => panic!("Not a function! {:?}", val),
    }
}

#[test]
fn test_function_call() {
    let input = r#"
    fun salute(name) { return "Hello, " + name; }
    var salutation = salute("World!");
    "#;
    let (stmts, mut inter) = get_stmts(input);
    assert_eq!(Ok(()), inter.interpret(&stmts));
    assert_eq!(Ok("Hello, World!".into()), env_get(&inter, "salutation"));
}

#[test]
fn test_function_call_native() {
    let input = r#"
    var time = clock();
    var age = str(23);
    "#;
    let (stmts, mut inter) = get_stmts(input);
    assert_eq!(Ok(()), inter.interpret(&stmts));
    assert!(matches!(env_get(&inter, "time").unwrap(), Integer(_)));
    assert_eq!(Ok("23".into()), env_get(&inter, "age"));
}

#[test]
fn test_local_function() {
    let input = r#"
    fun outer() {
        fun inner() {
            return "Inner!";
        }

        return inner;
    }

    var inner = outer();
    var res = inner();
    "#;
    let (stmts, mut inter) = get_stmts(input);
    assert_eq!(Ok(()), inter.interpret(&stmts));
    assert_eq!(Ok("Inner!".into()), env_get(&inter, "res"));
}

#[test]
fn test_closures() {
    let input = r#"
    var global_get;
    var global_set;

    fun outer() {
        var x = "initial";

        fun get_x() {
            return x;
        }

        fun set_x() {
            x = "updated";
        }

        global_get = get_x;
        global_set = set_x;
    }

    outer();
    var before = global_get();
    global_set();
    var after = global_get();
    "#;
    let (stmts, mut inter) = get_stmts(input);
    assert_eq!(Ok(()), inter.interpret(&stmts));
    assert_eq!(Ok("initial".into()), env_get(&inter, "before"));
    assert_eq!(Ok("updated".into()), env_get(&inter, "after"));
}

#[test]
fn test_anon_function() {
    let input = r#"
    fun outer() {
        return fun () {
            return "Inner!";
        };
    }

    var inner = outer();
    var res = inner();
    "#;
    let (stmts, mut inter) = get_stmts(input);
    assert_eq!(Ok(()), inter.interpret(&stmts));
    assert_eq!(Ok("Inner!".into()), env_get(&inter, "res"));
}

#[test]
fn test_local_resolution() {
    let input = r#" var global_x;
    {var x = 1; {{{{{ x = 3; { global_x = x; }}}}}}}
    "#;
    let (stmts, mut inter) = get_stmts(input);
    assert_eq!(Ok(()), inter.interpret(&stmts));
    assert_eq!(Ok(3.into()), env_get(&inter, "global_x"));
}

#[test]
fn test_locals() {
    let input = r#"
    var global_x;
    var global_y;
    var global_z;

    {
        var x = 5;
        var y = true;
        {
            var x = 3;
            var z = "string";

            global_x = x;
        }

        global_y = y;
        global_z = x;
    }
    "#;
    let (stmts, mut inter) = get_stmts(input);
    assert_eq!(Ok(()), inter.interpret(&stmts));
    assert_eq!(Ok(3.into()), env_get(&inter, "global_x"));
    assert_eq!(Ok(true.into()), env_get(&inter, "global_y"));
    assert_eq!(Ok(5.into()), env_get(&inter, "global_z"));
}

#[test]
fn test_class() {
    let input = r#"
    class MyClass {}
    var inst = MyClass();
    "#;
    let (stmts, mut inter) = get_stmts(input);
    assert_eq!(Ok(()), inter.interpret(&stmts));
    assert!(matches!(
        env_get(&inter, "MyClass"),
        Ok(Value::Callable(Callable::Class(_)))
    ));
    assert!(matches!(env_get(&inter, "inst"), Ok(Value::Instance(_))));
}

#[test]
fn test_class_instance_fields() {
    let input = r#"
    class MyClass {}
    var inst = MyClass();
    inst.x = 3;
    var x = inst.x;
    "#;
    let (stmts, mut inter) = get_stmts(input);
    assert_eq!(Ok(()), inter.interpret(&stmts));
    assert_eq!(Ok(3.into()), env_get(&inter, "x"));
}

#[test]
fn test_class_methods() {
    let input = r#"
    class MyClass {
        a_method() { return 1; }
        b_method(name) { return "Hello, " + name; }
    }
    var inst = MyClass();
    var x = inst.a_method();
    var y = inst.b_method("World");
    "#;
    let (stmts, mut inter) = get_stmts(input);
    assert_eq!(Ok(()), inter.interpret(&stmts));
    assert_eq!(Ok(1.into()), env_get(&inter, "x"));
    assert_eq!(Ok("Hello, World".into()), env_get(&inter, "y"));
}

#[test]
fn test_class_static_methods() {
    let input = r#"
    class MyClass {
        class a_method() { return 1; }
        class b_method(name) { return "Hello, " + name; }
    }
    var x = MyClass.a_method();
    var y = MyClass.b_method("World");
    "#;
    let (stmts, mut inter) = get_stmts(input);
    assert_eq!(Ok(()), inter.interpret(&stmts));
    assert_eq!(Ok(1.into()), env_get(&inter, "x"));
    assert_eq!(Ok("Hello, World".into()), env_get(&inter, "y"));
}

#[test]
fn test_class_this() {
    let input = r#"
    class MyClass {
        get_x() { return this.x; }
        set_x(x) { this.x = x; }
    }
    var inst = MyClass();
    inst.set_x(3);
    var x = inst.get_x();
    "#;
    let (stmts, mut inter) = get_stmts(input);
    assert_eq!(Ok(()), inter.interpret(&stmts));
    assert_eq!(Ok(3.into()), env_get(&inter, "x"));
}

#[test]
fn test_class_init() {
    let input = r#"
    class MyClass {
        init(x) { this.x = x; }
        get_x() { return this.x; }
        set_x(x) { this.x = x; }
    }
    var inst = MyClass(1);
    var x = inst.get_x();
    inst.set_x(3);
    var y = inst.get_x();
    inst.init(-1);
    var z = inst.get_x();
    "#;
    let (stmts, mut inter) = get_stmts(input);
    assert_eq!(Ok(()), inter.interpret(&stmts));
    assert_eq!(Ok(1.into()), env_get(&inter, "x"));
    assert_eq!(Ok(3.into()), env_get(&inter, "y"));
    assert_eq!(Ok((-1).into()), env_get(&inter, "z"));
}

#[test]
fn test_class_out_of_scope() {
    let input = r#"var inst;
    {
        class MyClass {
            foo() { return "bar"; }
        }
        inst = MyClass();
    }

    var foo = inst.foo();
    "#;
    let (stmts, mut inter) = get_stmts(input);
    assert_eq!(Ok(()), inter.interpret(&stmts));
    assert!(matches!(env_get(&inter, "inst"), Ok(Value::Instance(_))));
    assert_eq!(Ok("bar".into()), env_get(&inter, "foo"));
}

#[test]
fn test_class_getters() {
    let input = r#"
    class Square {
        init(x) { this.x = x; }
        area { return this.x * this.x; }
    }
    var inst = Square(5);
    var area = inst.area;
    "#;
    let (stmts, mut inter) = get_stmts(input);
    assert_eq!(Ok(()), inter.interpret(&stmts));
    assert_eq!(Ok(25.into()), env_get(&inter, "area"));
}

#[test]
fn test_class_getter_init() {
    let input = r#"
    class MyClass {
        init { this.x = 5; return 3; }
    }
    var inst = MyClass();
    var y = inst.init;
    var x = inst.x;
    "#;
    let (stmts, mut inter) = get_stmts(input);
    assert_eq!(Ok(()), inter.interpret(&stmts));
    assert_eq!(Ok(5.into()), env_get(&inter, "x"));
    assert_eq!(Ok(3.into()), env_get(&inter, "y"));
}

#[test]
fn test_class_inheritance() {
    let input = r#"
    class Rectangle {
        init(x, y) {
            this.x = x;
            this.y = y;
        }

        area { return this.x * this.x; }
    }

    class Square < Rectangle {
        init(x) {
            super.init(x, x);
        }
    }

    var square = Square(3);
    var x = square.x;
    var area = square.area;
    "#;
    let (stmts, mut inter) = get_stmts(input);
    assert_eq!(Ok(()), inter.interpret(&stmts));
    assert_eq!(Ok(3.into()), env_get(&inter, "x"));
    assert_eq!(Ok(9.into()), env_get(&inter, "area"));
}

#[test]
fn test_class_inheritance_method_shadowing() {
    let input = r#"
    class A {
        init() {
            this.x = "";
        }

        method() {
            this.x += "A";
        }
    }

    class B < A {
        method() {
            this.x += "B";
        }
    }

    var a = A();
    a.method();
    var a_x = a.x;
    var b = B();
    b.method();
    var b_x = b.x;
    "#;
    let (stmts, mut inter) = get_stmts(input);
    assert_eq!(Ok(()), inter.interpret(&stmts));
    assert_eq!(Ok("A".into()), env_get(&inter, "a_x"));
    assert_eq!(Ok("B".into()), env_get(&inter, "b_x"));
}

#[test]
fn test_undefined_variable() {
    let input = r#"hello;"#;
    let (stmts, mut inter) = get_stmts(input);
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
    let (stmts, mut inter) = get_stmts(input);
    assert_eq!(
        Err(RuntimeError::UndefinedVariable(
            Loc::new(0, 0),
            String::from("hello")
        )),
        inter.interpret(&stmts)
    );
}

#[test]
fn test_not_a_callable() {
    let input = r#""world"();"#;
    let (stmts, mut inter) = get_stmts(input);
    assert_eq!(
        Err(RuntimeError::NotACallable(
            Loc::new(0, 8),
            String::from(STRING)
        )),
        inter.interpret(&stmts)
    );
}

#[test]
fn test_too_few_arguments() {
    let input = r#"str();"#;
    let (stmts, mut inter) = get_stmts(input);
    assert_eq!(
        Err(RuntimeError::MismatchingArity(Loc::new(0, 4), 1, 0)),
        inter.interpret(&stmts)
    );
}

#[test]
fn test_too_many_arguments() {
    let input = r#"clock(1);"#;
    let (stmts, mut inter) = get_stmts(input);
    assert_eq!(
        Err(RuntimeError::MismatchingArity(Loc::new(0, 7), 0, 1)),
        inter.interpret(&stmts)
    );
}

#[test]
fn test_no_properties() {
    let input = r#"1.field;"#;
    let (stmts, mut inter) = get_stmts(input);
    assert_eq!(
        Err(RuntimeError::NoProperties(
            Loc::new(0, 1),
            String::from(INT)
        )),
        inter.interpret(&stmts)
    );
}

#[test]
fn test_undefined_property() {
    let input = r#"
    class MyClass {}
    var inst = MyClass();
    print inst.field;
    "#;
    let (stmts, mut inter) = get_stmts(input);
    assert_eq!(
        Err(RuntimeError::UndefinedProperty(
            Loc::new(3, 14),
            String::from("field")
        )),
        inter.interpret(&stmts)
    );
}

#[test]
fn test_no_fields() {
    let input = r#"1.field = 3;"#;
    let (stmts, mut inter) = get_stmts(input);
    assert_eq!(
        Err(RuntimeError::NoFields(Loc::new(0, 1), String::from(INT))),
        inter.interpret(&stmts)
    );
}

#[test]
fn test_superclass_is_not_class() {
    let input = r#"
    var A = "a";
    class B < A {}
    "#;
    let (stmts, mut inter) = get_stmts(input);
    assert_eq!(
        Err(RuntimeError::SuperclassIsNotClass(
            Loc::new(2, 14),
            String::from(STRING)
        )),
        inter.interpret(&stmts)
    );
}
