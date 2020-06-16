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

#[derive(PartialEq, Debug)]
pub enum Literal {
    Integer(u64),
    Float(f64),
    String(String),
}

#[derive(PartialEq, Debug)]
pub struct Token<'a> {
    pub kind: TokenKind,
    pub lexeme: &'a str,
    pub literal: Option<Literal>,
    pub location: Location,
}

pub struct Scanner<'a> {
    input: &'a str,
    chars: std::str::Chars<'a>,
    start: usize,
    current: usize,
    start_location: Location,
    current_location: Location,
}

#[derive(Debug, PartialEq, Fail)]
pub enum ScanningError {
    #[fail(display = "[{}] Unrecognized character '{}'", location, character)]
    UnrecognizedCharacter { character: char, location: Location },
    #[fail(display = "[{}] Unterminated string ", location)]
    UnterminatedString { location: Location },
}

type TokenRes<'a> = Result<Token<'a>, ScanningError>;
type ScanningRes<'a> = Result<Vec<Token<'a>>, ScanningError>;

impl Literal {
    pub fn string(string: &str) -> Self {
        Literal::String(String::from(string))
    }
}

impl<'a> Token<'a> {
    pub fn eof(location: Location) -> Self {
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
            chars: input.chars(),
            start: 0,
            current: 0,
            start_location: Default::default(),
            current_location: Default::default(),
        }
    }

    pub fn scan_tokens(&mut self) -> ScanningRes<'a> {
        let mut tokens: Vec<Token<'a>> = vec![];
        let mut token_res = self.scan_token();

        loop {
            match token_res {
                Ok(Some(token)) => {
                    tokens.push(token);
                    token_res = self.scan_token();
                }
                Ok(None) => {
                    if self.is_at_end() {
                        break;
                    }
                    token_res = self.scan_token();
                }
                Err(error) => {
                    // TODO: Accept multiple scanning errors
                    return Err(error);
                }
            }
        }

        tokens.push(Token::eof(self.current_location));
        Ok(tokens)
    }

    pub fn get_tokens(input: &'a str) -> ScanningRes<'a> {
        let mut scanner = Scanner::new(input);
        scanner.scan_tokens()
    }

    fn scan_token(&mut self) -> Result<Option<Token<'a>>, ScanningError> {
        use TokenKind::*;
        self.skip_whitespace();
        self.start = self.current;
        self.start_location = self.current_location;

        if self.is_at_end() {
            return Ok(None);
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
            '"' => Some(self.recognize_string()?),
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
                self.current_location.new_line();
            }
        }
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.input.len()
    }

    fn advance(&mut self) -> Option<char> {
        self.current += 1;
        self.current_location.advance();
        self.chars.next()
    }

    fn peek(&self) -> Option<char> {
        self.chars.clone().nth(0)
    }

    fn peek_next(&self) -> Option<char> {
        self.chars.clone().nth(1)
    }

    fn matches(&mut self, expected: char) -> bool {
        if self.peek() == Some(expected) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn recognize_string(&mut self) -> TokenRes<'a> {
        while let Some(character) = self.peek() {
            match character {
                '"' => break,
                '\\' => {
                    self.advance();
                    self.advance();
                }
                '\n' => {
                    self.advance();
                    self.current_location.new_line();
                }
                _ => {
                    self.advance();
                }
            }
        }

        if self.is_at_end() {
            return Err(unterminated_string(&self));
        }

        self.advance();

        let value = unescape_string(&self.input[self.start + 1..self.current - 1]);
        let literal = Literal::string(&value);
        Ok(self.create_literal_token(TokenKind::String, literal))
    }

    fn create_token(&self, kind: TokenKind) -> Token<'a> {
        Token {
            kind,
            lexeme: &self.input[self.start..self.current],
            literal: None,
            location: self.start_location,
        }
    }

    fn create_literal_token(&self, kind: TokenKind, literal: Literal) -> Token<'a> {
        Token {
            kind,
            lexeme: &self.input[self.start..self.current],
            literal: Some(literal),
            location: self.start_location,
        }
    }
}

fn unrecognized_character(scanner: &Scanner, character: char) -> ScanningError {
    ScanningError::UnrecognizedCharacter {
        character,
        location: scanner.current_location,
    }
}

fn unterminated_string(scanner: &Scanner) -> ScanningError {
    ScanningError::UnterminatedString {
        location: scanner.current_location,
    }
}

fn unescape_string(input: &str) -> String {
    let mut chars = input.chars().peekable();
    let mut output = String::with_capacity(input.len());
    while let Some(c) = chars.next() {
        let new_char = if c == '\\' {
            let next_char = chars.next().expect("Strings cannot end in a back-slash");
            match next_char {
                'n' => '\n',
                'r' => '\r',
                't' => '\t',
                '0' => '\0',
                _ => next_char,
            }
        } else {
            c
        };
        output.push(new_char);
    }

    output
}
