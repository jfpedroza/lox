use super::{ScanningError::*, TokenKind::*, *};
use crate::location::Location;

const LEXEME_KINDS: [(&str, TokenKind); 36] = [
    ("(", LeftParen),
    (")", RightParen),
    ("{", LeftBrace),
    ("}", RightBrace),
    (",", Comma),
    (".", Dot),
    ("-", Minus),
    ("+", Plus),
    (";", Semicolon),
    ("*", Star),
    ("%", Percent),
    ("/", Slash),
    ("!", Bang),
    ("!=", BangEqual),
    ("=", Equal),
    ("==", EqualEqual),
    ("<", Less),
    ("<=", LessEqual),
    (">", Greater),
    (">=", GreaterEqual),
    ("and", And),
    ("class", Class),
    ("else", Else),
    ("false", False),
    ("for", For),
    ("fun", Fun),
    ("if", If),
    ("nil", Nil),
    ("or", Or),
    ("print", Print),
    ("return", Return),
    ("super", Super),
    ("this", This),
    ("true", True),
    ("var", Var),
    ("while", While),
];

fn no_token(line: usize, column: usize) -> Vec<Token<'static>> {
    vec![Token::eof(Location::new(line, column))]
}

fn one_token(token: Token) -> Vec<Token> {
    let mut location = token.location;
    location.column += token.lexeme.len();
    vec![token, Token::eof(location)]
}

#[test]
fn test_empty_input() {
    let tokens = Scanner::get_tokens("");
    assert_eq!(Ok(no_token(0, 0)), tokens);
}

fn non_literal_token<'a>(
    kind: TokenKind,
    lexeme: &'a str,
    line: usize,
    column: usize,
) -> Token<'a> {
    Token {
        kind,
        lexeme,
        literal: None,
        location: Location::new(line, column),
    }
}

fn literal_token<'a>(
    kind: TokenKind,
    lexeme: &'a str,
    literal: Literal,
    line: usize,
    column: usize,
) -> Token<'a> {
    Token {
        kind,
        lexeme,
        literal: Some(literal),
        location: Location::new(line, column),
    }
}

fn string_token<'a>(lexeme: &'a str, literal: &'a str, line: usize, column: usize) -> Token<'a> {
    literal_token(Str, lexeme, Literal::string(literal), line, column)
}

fn integer_token<'a>(lexeme: &'a str, literal: i64, line: usize, column: usize) -> Token<'a> {
    literal_token(
        Number(NumberKind::Integer),
        lexeme,
        Literal::Integer(literal),
        line,
        column,
    )
}

fn float_token<'a>(lexeme: &'a str, literal: f64, line: usize, column: usize) -> Token<'a> {
    literal_token(
        Number(NumberKind::Float),
        lexeme,
        Literal::Float(literal),
        line,
        column,
    )
}

#[test]
fn test_single_token() {
    for (lexeme, kind) in LEXEME_KINDS.iter() {
        let tokens = Scanner::get_tokens(lexeme);
        let expected_token = non_literal_token(*kind, lexeme, 0, 0);
        assert_eq!(Ok(one_token(expected_token)), tokens);
    }
}

#[test]
fn test_line_comment() {
    let tokens = Scanner::get_tokens("// a line comment");
    assert_eq!(Ok(no_token(0, 17)), tokens);
}

#[test]
fn test_block_comment() {
    let input = r#"/* This is a block
        comment */"#;
    let tokens = Scanner::get_tokens(input);
    assert_eq!(Ok(no_token(1, 18)), tokens);
}

#[test]
fn test_multiple_tokens() {
    let tokens = Scanner::get_tokens("(){},.-+;*!=!%===<=</>>=//this should be ignored");
    let expected_tokens = vec![
        non_literal_token(LeftParen, "(", 0, 0),
        non_literal_token(RightParen, ")", 0, 1),
        non_literal_token(LeftBrace, "{", 0, 2),
        non_literal_token(RightBrace, "}", 0, 3),
        non_literal_token(Comma, ",", 0, 4),
        non_literal_token(Dot, ".", 0, 5),
        non_literal_token(Minus, "-", 0, 6),
        non_literal_token(Plus, "+", 0, 7),
        non_literal_token(Semicolon, ";", 0, 8),
        non_literal_token(Star, "*", 0, 9),
        non_literal_token(BangEqual, "!=", 0, 10),
        non_literal_token(Bang, "!", 0, 12),
        non_literal_token(Percent, "%", 0, 13),
        non_literal_token(EqualEqual, "==", 0, 14),
        non_literal_token(Equal, "=", 0, 16),
        non_literal_token(LessEqual, "<=", 0, 17),
        non_literal_token(Less, "<", 0, 19),
        non_literal_token(Slash, "/", 0, 20),
        non_literal_token(Greater, ">", 0, 21),
        non_literal_token(GreaterEqual, ">=", 0, 22),
        Token::eof(Location::new(0, 48)),
    ];
    assert_eq!(Ok(expected_tokens), tokens);
}

#[test]
fn test_string() {
    let input = r#""this is a string""#;
    let tokens = Scanner::get_tokens(&input);
    let expected_token = string_token(&input, "this is a string", 0, 0);
    assert_eq!(Ok(one_token(expected_token)), tokens);
}

#[test]
fn test_string2() {
    let input = r#"("this is a string")"#;
    let tokens = Scanner::get_tokens(&input);
    let expected_tokens = vec![
        non_literal_token(LeftParen, "(", 0, 0),
        string_token(r#""this is a string""#, "this is a string", 0, 1),
        non_literal_token(RightParen, ")", 0, 19),
        Token::eof(Location::new(0, 20)),
    ];
    assert_eq!(Ok(expected_tokens), tokens);
}

#[test]
fn test_escaped_string() {
    let input = r#""this\nis\ta \" string\\""#;
    let tokens = Scanner::get_tokens(&input);
    let expected_token = string_token(input, "this\nis\ta \" string\\", 0, 0);
    assert_eq!(Ok(one_token(expected_token)), tokens);
}

#[test]
fn test_integer() {
    let input = "1234";
    let tokens = Scanner::get_tokens(&input);
    let expected_token = integer_token(input, 1234, 0, 0);
    assert_eq!(Ok(one_token(expected_token)), tokens);
}

#[test]
fn test_float() {
    for (input, output) in &[
        ("1234.567", 1234.567),
        ("1234.567e2", 123456.7),
        ("1234.567e+2", 123456.7),
        ("1234.567e-2", 12.34567),
        ("12e12", 12e12),
    ] {
        let tokens = Scanner::get_tokens(&input);
        let expected_token = float_token(input, *output, 0, 0);
        assert_eq!(Ok(one_token(expected_token)), tokens);
    }
}

#[test]
fn test_identifier() {
    for input in &[
        "hello",
        "hello_world",
        "h1",
        "r",
        "anda",
        "CamelCase",
        "_underscore",
    ] {
        let tokens = Scanner::get_tokens(&input);
        let expected_token = non_literal_token(Identifier, input, 0, 0);
        assert_eq!(Ok(one_token(expected_token)), tokens);
    }
}

#[test]
fn test_big_input() {
    let input = r#"// This is a comment
        var hello = "world";
        var a = (b + c - d) * e/1.0

        /* this is a
         * block comment */
        fun my_function(something) {
            print something;
            return nil;
        }

        if(i == 0 and j != 3) {
            2.sqrt()
        }
        "#;
    let tokens = Scanner::get_tokens(&input);
    let expected_tokens = vec![
        // 2nd line
        non_literal_token(Var, "var", 1, 8),
        non_literal_token(Identifier, "hello", 1, 12),
        non_literal_token(Equal, "=", 1, 18),
        string_token("\"world\"", "world", 1, 20),
        non_literal_token(Semicolon, ";", 1, 27),
        // 3rd line
        non_literal_token(Var, "var", 2, 8),
        non_literal_token(Identifier, "a", 2, 12),
        non_literal_token(Equal, "=", 2, 14),
        non_literal_token(LeftParen, "(", 2, 16),
        non_literal_token(Identifier, "b", 2, 17),
        non_literal_token(Plus, "+", 2, 19),
        non_literal_token(Identifier, "c", 2, 21),
        non_literal_token(Minus, "-", 2, 23),
        non_literal_token(Identifier, "d", 2, 25),
        non_literal_token(RightParen, ")", 2, 26),
        non_literal_token(Star, "*", 2, 28),
        non_literal_token(Identifier, "e", 2, 30),
        non_literal_token(Slash, "/", 2, 31),
        float_token("1.0", 1.0, 2, 32),
        // 7th line
        non_literal_token(Fun, "fun", 6, 8),
        non_literal_token(Identifier, "my_function", 6, 12),
        non_literal_token(LeftParen, "(", 6, 23),
        non_literal_token(Identifier, "something", 6, 24),
        non_literal_token(RightParen, ")", 6, 33),
        non_literal_token(LeftBrace, "{", 6, 35),
        // 8th line
        non_literal_token(Print, "print", 7, 12),
        non_literal_token(Identifier, "something", 7, 18),
        non_literal_token(Semicolon, ";", 7, 27),
        // 9th line
        non_literal_token(Return, "return", 8, 12),
        non_literal_token(Nil, "nil", 8, 19),
        non_literal_token(Semicolon, ";", 8, 22),
        // 10th line
        non_literal_token(RightBrace, "}", 9, 8),
        // 12th line
        non_literal_token(If, "if", 11, 8),
        non_literal_token(LeftParen, "(", 11, 10),
        non_literal_token(Identifier, "i", 11, 11),
        non_literal_token(EqualEqual, "==", 11, 13),
        integer_token("0", 0, 11, 16),
        non_literal_token(And, "and", 11, 18),
        non_literal_token(Identifier, "j", 11, 22),
        non_literal_token(BangEqual, "!=", 11, 24),
        integer_token("3", 3, 11, 27),
        non_literal_token(RightParen, ")", 11, 28),
        non_literal_token(LeftBrace, "{", 11, 30),
        // 13th line
        integer_token("2", 2, 12, 12),
        non_literal_token(Dot, ".", 12, 13),
        non_literal_token(Identifier, "sqrt", 12, 14),
        non_literal_token(LeftParen, "(", 12, 18),
        non_literal_token(RightParen, ")", 12, 19),
        // 14th line
        non_literal_token(RightBrace, "}", 13, 8),
        // End
        Token::eof(Location::new(14, 8)),
    ];

    for (i, token) in tokens.unwrap().into_iter().enumerate() {
        assert_eq!(token, expected_tokens[i]);
    }
}

#[test]
fn test_error1() {
    let tokens = Scanner::get_tokens("invalid_character?");
    assert_eq!(
        Err(UnrecognizedCharacter {
            character: '?',
            location: Location::new(0, 17),
        }),
        tokens
    );
}

#[test]
fn test_error2() {
    let tokens = Scanner::get_tokens("\"unterminated");
    assert_eq!(
        Err(UnterminatedString {
            location: Location::new(0, 13)
        }),
        tokens
    );
}

#[test]
fn test_error3() {
    let tokens = Scanner::get_tokens("var = 3\npi=3.14e+ - 8");
    assert_eq!(
        Err(InvalidNumber {
            number: std::string::String::from("3.14e+"),
            location: Location::new(1, 3)
        }),
        tokens
    );
}

#[test]
fn test_error4() {
    let tokens = Scanner::get_tokens("/* /* */");
    assert_eq!(
        Err(UnterminatedBlockComment {
            location: Location::new(0, 8)
        }),
        tokens
    );
}
