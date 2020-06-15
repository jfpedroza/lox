use crate::location::Location;
use std::fmt::{Display, Formatter, Result as FmtResult};

/// Enum representing lexeme types
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum TokenKind {
    // Single-character tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    Percent,

    // One or two character tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals
    Identifier,
    String,
    Number(NumberKind),

    // Keywords
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    EOF,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum NumberKind {
    Integer,
    Float,
}

enum Literal {
    Integer(u64),
    Float(f64),
    String(String),
}

pub struct Token<'a> {
    kind: TokenKind,
    lexeme: &'a str,
    literal: Option<Literal>,
    location: Location,
}

pub struct Scanner<'a> {
    input: &'a str,
    iter: std::iter::Peekable<std::str::Chars<'a>>,
    start: usize,
    current: usize,
    line: usize,
    column: usize,
}

#[derive(Debug, PartialEq, Fail)]
pub enum ScanningError {
    #[fail(display = "[{}] Unrecognized character '{}'", location, character)]
    UnrecognizedCharacter { character: char, location: Location },
}

type TokenRes<'a> = Result<Token<'a>, ScanningError>;
type ScanningRes<'a> = Result<Vec<Token<'a>>, ScanningError>;

impl<'a> Token<'a> {
    fn eof(location: Location) -> Self {
        Token {
            kind: TokenKind::EOF,
            lexeme: "",
            literal: None,
            location,
        }
    }
}

impl Display for Token<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "<{:?}, {}, {}>", self.kind, self.lexeme, self.location)
    }
}

impl<'a> Scanner<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            iter: input.chars().peekable(),
            start: 0,
            current: 0,
            line: 0,
            column: 0,
        }
    }

    pub fn scan_tokens(&mut self) -> ScanningRes {
        let mut tokens: Vec<Token<'a>> = vec![];
        let mut token_res = self.scan_token();

        loop {
            match token_res {
                Ok(Some(Token {
                    kind: TokenKind::EOF,
                    ..
                })) => break,
                Ok(Some(token)) => {
                    tokens.push(token);
                    token_res = self.scan_token();
                }
                Ok(None) => {
                    token_res = self.scan_token();
                }
                Err(error) => {
                    return Err(error);
                }
            }
        }

        Ok(tokens)
    }

    fn scan_token(&mut self) -> Result<Option<Token<'a>>, ScanningError> {
        use TokenKind::*;
        self.skip_whitespace();
        self.start = self.current;

        if self.is_at_end() {
            return Ok(Some(Token::eof(self.location())));
        }

        let token = match self.advance().unwrap() {
            '(' => Some(self.create_token(LeftParen)),
            ')' => Some(self.create_token(RightParen)),
            '{' => Some(self.create_token(LeftBrace)),
            '}' => Some(self.create_token(RightBrace)),
            ',' => Some(self.create_token(Comma)),
            '.' => Some(self.create_token(Dot)),
            '-' => Some(self.create_token(Minus)),
            '+' => Some(self.create_token(Plus)),
            ';' => Some(self.create_token(Semicolon)),
            '*' => Some(self.create_token(Star)),
            '%' => Some(self.create_token(Percent)),
            '!' => {
                let kind = if self.matches('=') { BangEqual } else { Bang };
                Some(self.create_token(kind))
            }
            '=' => {
                let kind = if self.matches('=') { EqualEqual } else { Equal };
                Some(self.create_token(kind))
            }
            '<' => {
                let kind = if self.matches('=') { LessEqual } else { Less };
                Some(self.create_token(kind))
            }
            '>' => {
                let kind = if self.matches('=') {
                    GreaterEqual
                } else {
                    Greater
                };
                Some(self.create_token(kind))
            }
            '/' => {
                if self.matches('/') {
                    while let Some(character) = self.peek() {
                        if character == '\n' {
                            break;
                        } else {
                            self.advance();
                        }
                    }
                    None
                } else {
                    Some(self.create_token(Slash))
                }
            }
            character => return Err(unrecognized_character(&self, character)),
        };

        Ok(token)
    }

    fn skip_whitespace(&mut self) {
        while let Some(character) = self.peek() {
            if !character.is_ascii_whitespace() {
                break;
            }

            self.advance();

            if character == '\n' {
                self.line += 1;
                self.column = 0;
            }
        }
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.input.len()
    }

    fn advance(&mut self) -> Option<char> {
        self.current += 1;
        self.column += 1;
        self.iter.next()
    }

    fn peek(&mut self) -> Option<char> {
        self.iter.peek().cloned()
    }

    fn matches(&mut self, expected: char) -> bool {
        if let Some(_) = self.iter.next_if_eq(&expected) {
            self.current += 1;
            self.column += 1;
            true
        } else {
            false
        }
    }

    fn location(&self) -> Location {
        Location::new(self.line, self.column)
    }

    fn create_token(&self, kind: TokenKind) -> Token<'a> {
        Token {
            kind,
            lexeme: &self.input[self.start..self.current],
            literal: None,
            location: self.location(),
        }
    }
}

fn unrecognized_character(scanner: &Scanner, character: char) -> ScanningError {
    ScanningError::UnrecognizedCharacter {
        character,
        location: scanner.location(),
    }
}
