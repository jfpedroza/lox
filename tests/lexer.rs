use lox::lexer::{
    Scanner, Token,
    TokenKind::{self, *},
};
use lox::location::Location;

const LEXEME_KINDS: [(&str, TokenKind); 20] = [
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
];

fn no_token(column: usize) -> Vec<Token<'static>> {
    vec![Token::eof(Location::new(0, column))]
}

fn one_token(token: Token) -> Vec<Token> {
    let mut location = token.location;
    location.column += token.lexeme.len();
    vec![token, Token::eof(location)]
}

#[test]
fn test_empty_input() {
    let tokens = Scanner::get_tokens("");
    assert_eq!(Ok(no_token(0)), tokens);
}

fn non_literal_token<'a>(kind: TokenKind, lexeme: &'a str, column: usize) -> Token<'a> {
    Token {
        kind,
        lexeme,
        literal: None,
        location: Location::new(0, column),
    }
}

#[test]
fn test_single_token() {
    for (lexeme, kind) in &LEXEME_KINDS {
        let tokens = Scanner::get_tokens(lexeme);
        let expected_token = non_literal_token(*kind, lexeme, 0);
        assert_eq!(Ok(one_token(expected_token)), tokens);
    }
}

#[test]
fn test_line_comment() {
    let tokens = Scanner::get_tokens("// a line comment");
    assert_eq!(Ok(no_token(17)), tokens);
}

#[test]
fn test_multiple_tokens() {
    let tokens = Scanner::get_tokens("(){},.-+;*!=!%===<=</>>=//this should be ignored");
    let expected_tokens = vec![
        non_literal_token(LeftParen, "(", 0),
        non_literal_token(RightParen, ")", 1),
        non_literal_token(LeftBrace, "{", 2),
        non_literal_token(RightBrace, "}", 3),
        non_literal_token(Comma, ",", 4),
        non_literal_token(Dot, ".", 5),
        non_literal_token(Minus, "-", 6),
        non_literal_token(Plus, "+", 7),
        non_literal_token(Semicolon, ";", 8),
        non_literal_token(Star, "*", 9),
        non_literal_token(BangEqual, "!=", 10),
        non_literal_token(Bang, "!", 12),
        non_literal_token(Percent, "%", 13),
        non_literal_token(EqualEqual, "==", 14),
        non_literal_token(Equal, "=", 16),
        non_literal_token(LessEqual, "<=", 17),
        non_literal_token(Less, "<", 19),
        non_literal_token(Slash, "/", 20),
        non_literal_token(Greater, ">", 21),
        non_literal_token(GreaterEqual, ">=", 22),
        Token::eof(Location::new(0, 48)),
    ];
    assert_eq!(Ok(expected_tokens), tokens);
}
