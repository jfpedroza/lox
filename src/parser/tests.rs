use super::*;
use crate::location::Loc;
use crate::test_utils::*;

#[test]
fn test_integer_expr() {
    let tokens = get_tokens("1996");
    let mut parser = Parser::new(&tokens);
    assert_eq!(Ok(int_expr(1996, (0, 0))), parser.expression());
}

#[test]
fn test_float_expr() {
    let tokens = get_tokens("8.5");
    let mut parser = Parser::new(&tokens);
    assert_eq!(Ok(float_expr(8.5, (0, 0))), parser.expression());
}

#[test]
fn test_string_expr() {
    let tokens = get_tokens("\"1996\"");
    let mut parser = Parser::new(&tokens);
    assert_eq!(Ok(str_expr("1996", (0, 0))), parser.expression());
}

#[test]
fn test_true_expr() {
    let tokens = get_tokens("true");
    let mut parser = Parser::new(&tokens);
    assert_eq!(Ok(bool_expr(true, (0, 0))), parser.expression());
}

#[test]
fn test_false_expr() {
    let tokens = get_tokens("false");
    let mut parser = Parser::new(&tokens);
    assert_eq!(Ok(bool_expr(false, (0, 0))), parser.expression());
}

#[test]
fn test_nil_expr() {
    let tokens = get_tokens("nil");
    let mut parser = Parser::new(&tokens);
    assert_eq!(Ok(nil_expr((0, 0))), parser.expression());
}

#[test]
fn test_unary_not_expr() {
    let tokens = get_tokens("!true");
    let mut parser = Parser::new(&tokens);
    assert_eq!(
        Ok(not_expr(bool_expr(true, (0, 1)), (0, 0))),
        parser.expression()
    );
}

#[test]
fn test_unary_negate_expr() {
    let tokens = get_tokens("-100");
    let mut parser = Parser::new(&tokens);
    assert_eq!(
        Ok(neg_expr(int_expr(100, (0, 1)), (0, 0))),
        parser.expression()
    );
}

#[test]
fn test_binary_add_expr() {
    let tokens = get_tokens("30 + 100");
    let mut parser = Parser::new(&tokens);
    assert_eq!(
        Ok(add_expr(
            int_expr(30, (0, 0)),
            int_expr(100, (0, 5)),
            (0, 3)
        )),
        parser.expression()
    );
}

#[test]
fn test_binary_sub_expr() {
    let tokens = get_tokens("30.5 - 100");
    let mut parser = Parser::new(&tokens);
    assert_eq!(
        Ok(sub_expr(
            float_expr(30.5, (0, 0)),
            int_expr(100, (0, 7)),
            (0, 5)
        )),
        parser.expression()
    );
}

#[test]
fn test_binary_mult_expr() {
    let tokens = get_tokens("30*100");
    let mut parser = Parser::new(&tokens);
    assert_eq!(
        Ok(mult_expr(
            int_expr(30, (0, 0)),
            int_expr(100, (0, 3)),
            (0, 2)
        )),
        parser.expression()
    );
}

#[test]
fn test_binary_div_expr() {
    let tokens = get_tokens("30/100");
    let mut parser = Parser::new(&tokens);
    assert_eq!(
        Ok(div_expr(
            int_expr(30, (0, 0)),
            int_expr(100, (0, 3)),
            (0, 2)
        )),
        parser.expression()
    );
}

#[test]
fn test_binary_rem_expr() {
    let tokens = get_tokens("30 % 100");
    let mut parser = Parser::new(&tokens);
    assert_eq!(
        Ok(rem_expr(
            int_expr(30, (0, 0)),
            int_expr(100, (0, 5)),
            (0, 3)
        )),
        parser.expression()
    );
}

#[test]
fn test_binary_equal_expr() {
    let tokens = get_tokens("30 == 100");
    let mut parser = Parser::new(&tokens);
    assert_eq!(
        Ok(eq_expr(int_expr(30, (0, 0)), int_expr(100, (0, 6)), (0, 3))),
        parser.expression()
    );
}

#[test]
fn test_binary_not_equal_expr() {
    let tokens = get_tokens("30 != 100");
    let mut parser = Parser::new(&tokens);
    assert_eq!(
        Ok(not_eq_expr(
            int_expr(30, (0, 0)),
            int_expr(100, (0, 6)),
            (0, 3)
        )),
        parser.expression()
    );
}

#[test]
fn test_binary_greater_expr() {
    let tokens = get_tokens("30 > 100");
    let mut parser = Parser::new(&tokens);
    assert_eq!(
        Ok(gt_expr(int_expr(30, (0, 0)), int_expr(100, (0, 5)), (0, 3))),
        parser.expression()
    );
}

#[test]
fn test_binary_greater_equal_expr() {
    let tokens = get_tokens("30 >= 100");
    let mut parser = Parser::new(&tokens);
    assert_eq!(
        Ok(gt_eq_expr(
            int_expr(30, (0, 0)),
            int_expr(100, (0, 6)),
            (0, 3)
        )),
        parser.expression()
    );
}

#[test]
fn test_binary_less_expr() {
    let tokens = get_tokens("30 < 100");
    let mut parser = Parser::new(&tokens);
    assert_eq!(
        Ok(less_expr(
            int_expr(30, (0, 0)),
            int_expr(100, (0, 5)),
            (0, 3)
        )),
        parser.expression()
    );
}

#[test]
fn test_binary_less_equal_expr() {
    let tokens = get_tokens("30 <= 100");
    let mut parser = Parser::new(&tokens);
    assert_eq!(
        Ok(less_eq_expr(
            int_expr(30, (0, 0)),
            int_expr(100, (0, 6)),
            (0, 3)
        )),
        parser.expression()
    );
}

#[test]
fn test_group_expr() {
    let tokens = get_tokens("(30 / 100)");
    let mut parser = Parser::new(&tokens);
    assert_eq!(
        Ok(group_expr(
            div_expr(int_expr(30, (0, 1)), int_expr(100, (0, 6)), (0, 4)),
            (0, 0)
        )),
        parser.expression()
    );
}

#[test]
fn test_comma_expr() {
    let tokens = get_tokens("30 / 100, 2*5.0, true");
    let mut parser = Parser::new(&tokens);
    assert_eq!(
        Ok(comma_expr(
            div_expr(int_expr(30, (0, 0)), int_expr(100, (0, 5)), (0, 3)),
            comma_expr(
                mult_expr(int_expr(2, (0, 10)), float_expr(5.0, (0, 12)), (0, 11)),
                bool_expr(true, (0, 17)),
                (0, 15)
            ),
            (0, 8)
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
            bool_expr(true, (0, 0)),
            str_expr("hello", (0, 7)),
            str_expr("world", (0, 17)),
            (0, 5)
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
            bool_expr(true, (0, 0)),
            cond_expr(
                str_expr("hello", (0, 7)),
                int_expr(1, (0, 17)),
                int_expr(2, (0, 21)),
                (0, 15)
            ),
            cond_expr(
                str_expr("world", (0, 25)),
                int_expr(3, (0, 35)),
                int_expr(4, (0, 39)),
                (0, 33)
            ),
            (0, 5)
        )),
        parser.expression()
    );
}

#[test]
fn test_variable_expr() {
    let tokens = get_tokens("variable");
    let mut parser = Parser::new(&tokens);
    assert_eq!(Ok(var_expr("variable", (0, 0)),), parser.expression());
}

#[test]
fn test_assign_expr() {
    let tokens = get_tokens(r#"hello = "world""#);
    let mut parser = Parser::new(&tokens);
    assert_eq!(
        Ok(assign_expr("hello", str_expr("world", (0, 8)), (0, 0))),
        parser.expression()
    );
}

#[test]
fn test_nested_expr() {
    let tokens = get_tokens(r#"2 > 3 ? "yes" : (1e4 + 29 - 3.1416) * a/1.0"#);
    let mut parser = Parser::new(&tokens);
    assert_eq!(
        Ok(cond_expr(
            gt_expr(int_expr(2, (0, 0)), int_expr(3, (0, 4)), (0, 2)),
            str_expr("yes", (0, 8)),
            div_expr(
                mult_expr(
                    group_expr(
                        sub_expr(
                            add_expr(float_expr(10000.0, (0, 17)), int_expr(29, (0, 23)), (0, 21)),
                            float_expr(3.1416, (0, 28)),
                            (0, 26)
                        ),
                        (0, 16)
                    ),
                    var_expr("a", (0, 38)),
                    (0, 36)
                ),
                float_expr(1.0, (0, 40)),
                (0, 39)
            ),
            (0, 6)
        )),
        parser.expression()
    );
}

#[test]
fn test_expr_stmt() {
    let tokens = get_tokens("1996;");
    let mut parser = Parser::new(&tokens);
    assert_eq!(
        Ok(vec![expr_stmt(int_expr(1996, (0, 0)), (0, 0))]),
        parser.parse()
    );
}

#[test]
fn test_print_stmt() {
    let tokens = get_tokens("print \"1996\";");
    let mut parser = Parser::new(&tokens);
    assert_eq!(
        Ok(vec![print_stmt(str_expr("1996", (0, 6)), (0, 0))]),
        parser.parse()
    );
}

#[test]
fn test_var_stmt() {
    let tokens = get_tokens("var pi = 3.1416;");
    let mut parser = Parser::new(&tokens);
    assert_eq!(
        Ok(vec![var_stmt(
            "pi",
            Some(float_expr(3.1416, (0, 9))),
            (0, 0)
        )]),
        parser.parse()
    );
}

#[test]
fn test_null_var_stmt() {
    let tokens = get_tokens("var name;");
    let mut parser = Parser::new(&tokens);
    assert_eq!(Ok(vec![var_stmt("name", None, (0, 0))]), parser.parse());
}

#[test]
fn test_block_stmt() {
    let input = r#"{
    var name;
    1996;
    {
        var pi = 3.1416;
        print "1996";
    }
    }"#;
    let tokens = get_tokens(input);
    let mut parser = Parser::new(&tokens);
    assert_eq!(
        Ok(vec![block_stmt(
            vec![
                var_stmt("name", None, (1, 4)),
                expr_stmt(int_expr(1996, (2, 4)), (2, 4)),
                block_stmt(
                    vec![
                        var_stmt("pi", Some(float_expr(3.1416, (4, 17))), (4, 8)),
                        print_stmt(str_expr("1996", (5, 14)), (5, 8)),
                    ],
                    (3, 4)
                ),
            ],
            (0, 0)
        )]),
        parser.parse()
    );
}

#[test]
fn test_missing_close_paren() {
    let tokens = get_tokens("(100");
    let mut parser = Parser::new(&tokens);
    assert_eq!(
        Err(ParsingError::ExpectedCloseParen(
            Loc::new(0, 4),
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
            Loc::new(0, 10),
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
            Loc::new(0, 0),
            String::from("if")
        )),
        parser.expression()
    );
}

#[test]
fn test_missing_brace_paren() {
    let tokens = get_tokens("{100;");
    let mut parser = Parser::new(&tokens);
    assert_eq!(
        Err(ParsingError::ExpectedCloseBrace(
            Loc::new(0, 5),
            String::from("EOF")
        )),
        parser.parse()
    );
}

#[test]
fn test_missing_semicolon() {
    let tokens = get_tokens("print 1:");
    let mut parser = Parser::new(&tokens);
    assert_eq!(
        Err(ParsingError::ExpectedSemicolon(
            Loc::new(0, 7),
            String::from("value"),
            String::from(":")
        )),
        parser.parse()
    );
}

#[test]
fn test_missing_var_name() {
    let tokens = get_tokens("var 3");
    let mut parser = Parser::new(&tokens);
    assert_eq!(
        Err(ParsingError::ExpectedVarName(
            Loc::new(0, 4),
            String::from("3")
        )),
        parser.parse()
    );
}
