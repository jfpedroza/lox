use crate::eval::RuntimeError;
use crate::lexer::ScanningError;
use crate::parser::ParsingError;
use crate::resolver::ResolutionError;
use ansi_term::Color::Red;
use failure::{Error, Fail};
use std::fmt::{Display, Formatter, Result as FmtResult};

pub fn print_err(err: &Error) {
    let mut fail = err.as_fail();
    eprintln!("{}: {}", Red.bold().paint(error_type(err)), fail);
    while let Some(cause) = fail.cause() {
        eprintln!("> {}", cause);
        fail = cause;
    }
}

fn is_type<T: Fail>(err: &Error) -> bool {
    err.downcast_ref::<T>().is_some()
}

fn is_syntax_err(err: &Error) -> bool {
    is_type::<ScanningError>(err) || is_type::<ParsingError>(err) || is_type::<ResolutionError>(err)
}

fn error_type(err: &Error) -> &'static str {
    if is_syntax_err(err) {
        "SyntaxError"
    } else if is_type::<RuntimeError>(err) {
        "RuntimeError"
    } else {
        "Error"
    }
}

pub fn exit_code(err: &Error) -> i32 {
    if is_syntax_err(err) {
        65
    } else if is_type::<RuntimeError>(err) {
        70
    } else {
        1
    }
}

impl Display for ScanningError {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        use ScanningError::*;
        match self {
            UnrecognizedCharacter(character, loc) => {
                write!(f, "[{}] Unrecognized character '{}'", loc, character)
            }
            UnterminatedString(loc) => write!(f, "[{}] Unterminated string", loc),
            InvalidNumber(number, loc) => write!(f, "[{}] Invalid number {}", loc, number),
            UnterminatedBlockComment(loc) => write!(f, "[{}] Unterminated block comment", loc),
            Multiple(errors) => {
                let error_string: String =
                    errors.iter().map(|error| format!("\n{}", error)).collect();
                write!(f, "Multiple errors encountered{}", error_string)
            }
        }
    }
}

impl Display for ParsingError {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        use ParsingError::*;
        match self {
            ExpectedExpression(loc, got) => write!(f, "[{}] Expected expression. Got {}", loc, got),
            ExpectedOpenParen(loc, after, got) => {
                write!(f, "[{}] Expected '(' after {}. Got {}", loc, after, got)
            }
            ExpectedCloseParen(loc, after, got) => {
                write!(f, "[{}] Expected ')' after {}. Got {}", loc, after, got)
            }
            ExpectedOpenBrace(loc, before, got) => {
                write!(f, "[{}] Expected '{{' before {}. Got {}", loc, before, got)
            }
            ExpectedCloseBrace(loc, got) => {
                write!(f, "[{}] Expected '}}' after block. Got {}", loc, got)
            }
            ExpectedColon(loc, got) => write!(
                f,
                "[{}] Expected ':' for conditional expression. Got {}",
                loc, got
            ),
            ExpectedSemicolon(loc, after, got) => {
                write!(f, "[{}] Expected ';' after {}. Got {}", loc, after, got)
            }
            ExpectedName(loc, kind, got) => {
                write!(f, "[{}] Expected {} name. Got {}", loc, kind, got)
            }
            InvalidAssignmentTarget(loc) => write!(f, "[{}] Invalid assignment target", loc),
            MaximumArgumentsExceeded(loc, kind) => {
                write!(f, "[{}] Cannot have more than 255 {}", loc, kind)
            }
            Multiple(errors) => {
                let error_string: String =
                    errors.iter().map(|error| format!("\n{}", error)).collect();
                write!(f, "Multiple errors encountered{}", error_string)
            }
        }
    }
}

impl Display for ResolutionError {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        use ResolutionError::*;
        match self {
            VarInInitalizer(loc) => write!(
                f,
                "[{}] Cannot read local variable in its own initializer",
                loc
            ),
            VarAlreadyInScope(loc, name) => write!(
                f,
                "[{}] Variable '{}' already declared in this scope",
                loc, name
            ),
            DuplicateArgumentName(loc, name) => write!(
                f,
                "[{}] Duplicate argument '{}' in function definition",
                loc, name
            ),
            Multiple(errors) => {
                let error_string: String =
                    errors.iter().map(|error| format!("\n{}", error)).collect();
                write!(f, "Multiple errors encountered{}", error_string)
            }
        }
    }
}
