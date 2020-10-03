use crate::eval::RuntimeError;
use crate::lexer::ScanningError;
use crate::location::Loc;
use crate::parser::ParsingError;
use crate::resolver::ResolutionError;
use ansi_term::Color::{Red, Yellow};
use failure::{Error, Fail};
use std::fmt::{Display, Formatter, Result as FmtResult};

#[derive(Debug, PartialEq)]
pub enum Warning {
    UnusedVariable(Loc, String),
}

pub fn print_err(err: &Error) {
    let mut fail = err.as_fail();
    eprintln!("{}: {}", Red.bold().paint(error_type(err)), fail);
    while let Some(cause) = fail.cause() {
        eprintln!("> {}", cause);
        fail = cause;
    }
}

fn print_warn(warn: &Warning) {
    eprintln!("{}: {}", Yellow.bold().paint("Warning"), warn);
}

pub fn print_warns(warns: &[Warning]) {
    for warn in warns {
        print_warn(warn);
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
    } else if let Some(err) = err.downcast_ref::<RuntimeError>() {
        match err {
            RuntimeError::ExpectedType(_, _, _) => "TypeError",
            _ => "RuntimeError",
        }
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
            ExpectedCloseBrace(loc, after, got) => {
                write!(f, "[{}] Expected '}}' after {}. Got {}", loc, after, got)
            }
            ExpectedCloseBracket(loc, after, got) => {
                write!(f, "[{}] Expected ']' after {}. Got {}", loc, after, got)
            }
            ExpectedColon(loc, got) => write!(
                f,
                "[{}] Expected ':' for conditional expression. Got {}",
                loc, got
            ),
            ExpectedSemicolon(loc, after, got) => {
                write!(f, "[{}] Expected ';' after {}. Got {}", loc, after, got)
            }
            ExpectedDot(loc, after, got) => {
                write!(f, "[{}] Expected '.' after {}. Got {}", loc, after, got)
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

impl Display for RuntimeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        use RuntimeError::*;
        match self {
            UnsupportedOperand(loc, op, val_type) => write!(
                f,
                "[{}] Unsupported operand for {}: '{}'",
                loc, op, val_type
            ),
            UnsupportedOperands(loc, op, left_type, right_type) => write!(
                f,
                "[{}] Unsupported operands for {}: '{}' and '{}'",
                loc, op, left_type, right_type
            ),
            DivisionByZero(loc) => write!(f, "[{}] Division or modulo by zero", loc),
            UndefinedVariable(loc, name) => write!(f, "[{}] Undefined variable '{}'", loc, name),
            NotACallable(loc, val_type) => {
                write!(f, "[{}] Type '{}' is not callable", loc, val_type)
            }
            MismatchingArity(loc, expected, got) => write!(
                f,
                "[{}] Expected {} arguments but got {}",
                loc, expected, got
            ),
            NoProperties(loc, val_type) => {
                write!(f, "[{}] Type '{}' doesn't have properties", loc, val_type)
            }
            UndefinedProperty(loc, name) => write!(f, "[{}] Undefined property '{}'", loc, name),
            NoFields(loc, val_type) => {
                write!(f, "[{}] Type '{}' doesn't have fields", loc, val_type)
            }
            ExpectedType(loc, expected, got) => {
                write!(f, "[{}] Expected type '{}'. Got '{}'", loc, expected, got)
            }
            SuperclassIsNotClass(loc, val_type) => write!(
                f,
                "[{}] Superclass must be a class. Got '{}'",
                loc, val_type
            ),
            IndexOutOfBounds(loc, index, size) => {
                write!(f, "[{}] Index {} out of bounds. Size {}", loc, index, size)
            }
            NotAScriptable(loc, val_type) => {
                write!(f, "[{}] Type '{}' is not scriptable", loc, val_type)
            }
            ArrayIndexNotInteger(loc, val_type) => write!(
                f,
                "[{}] Array indices must be integers. Got '{}'",
                loc, val_type
            ),
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
            DuplicateMethod(loc, class_name, mtype, name) => write!(
                f,
                "[{}] Class '{}' already has {} called '{}'",
                loc, class_name, mtype, name
            ),
            ReturnOutsideFun(loc) => write!(f, "[{}] Cannot return from top-level code", loc),
            ThisOutsideClass(loc) => write!(f, "[{}] Cannot use 'this' outside of a class", loc),
            ReturnInInitializer(loc) => {
                write!(f, "[{}] Cannot return a value from an initializer", loc)
            }
            ThisInStaticMethod(loc) => write!(f, "[{}] Cannot use 'this' in a static method", loc),
            ClassInheritsItself(loc, name) => write!(
                f,
                "[{}] [class {}] A class cannot inherit from itself",
                loc, name
            ),
            SuperOutsideClass(loc) => write!(f, "[{}] Cannot use 'super' outside of a class", loc),
            SuperNoInSubclass(loc) => write!(
                f,
                "[{}] Cannot use 'super' in a class with no superclass",
                loc
            ),
            BreakOutsideLoop(loc) => write!(f, "[{}] Cannot use 'break' outside of a loop", loc),
            Multiple(errors) => {
                let error_string: String =
                    errors.iter().map(|error| format!("\n{}", error)).collect();
                write!(f, "Multiple errors encountered{}", error_string)
            }
        }
    }
}

impl Display for Warning {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        use Warning::*;
        match self {
            UnusedVariable(loc, name) => write!(f, "[{}] Unused variable '{}'", loc, name),
        }
    }
}
