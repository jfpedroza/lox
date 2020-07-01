use crate::eval::RuntimeError;
use crate::lexer::ScanningError;
use crate::parser::ParsingError;
use failure::Error;
use std::fmt::{Display, Formatter, Result as FmtResult};

pub fn print_err(err: &Error) {
    let mut fail = err.as_fail();
    eprintln!("\x1b[31;1m{}\x1b[0m: {}", error_type(err), fail);
    while let Some(cause) = fail.cause() {
        eprintln!("> {}", cause);
        fail = cause;
    }
}

fn error_type(err: &Error) -> &'static str {
    if err.downcast_ref::<ScanningError>().is_some() {
        "SyntaxError"
    } else if err.downcast_ref::<ParsingError>().is_some() {
        "SyntaxError"
    } else if err.downcast_ref::<RuntimeError>().is_some() {
        "RuntimeError"
    } else {
        "Error"
    }
}

pub fn exit_code(err: &Error) -> i32 {
    if err.downcast_ref::<ScanningError>().is_some() {
        65
    } else if err.downcast_ref::<ParsingError>().is_some() {
        65
    } else if err.downcast_ref::<RuntimeError>().is_some() {
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
            ExpectedCloseParen(loc, got) => {
                write!(f, "[{}] Expected ')' after expression. Got {}", loc, got)
            }
            ExpectedColon(loc, got) => write!(
                f,
                "[{}] Expected ':' for conditional expression. Got {}",
                loc, got
            ),
            ExpectedSemicolon(loc, after, got) => {
                write!(f, "[{}] Expected ';' after {}. Got {}", loc, after, got)
            }
            ExpectedVarName(loc, got) => write!(f, "[{}] Expected variable name. Got {}", loc, got),
            InvalidAssignmentTarget(loc) => write!(f, "[{}] Invalid assignment target", loc),
            Multiple(errors) => {
                let error_string: String =
                    errors.iter().map(|error| format!("\n{}", error)).collect();
                write!(f, "Multiple errors encountered{}", error_string)
            }
        }
    }
}
