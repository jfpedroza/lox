extern crate failure;
#[macro_use]
extern crate failure_derive;

pub mod error;
mod eval;
mod expr;
mod lexer;
mod location;
mod parser;
mod utils;
mod value;

use eval::Interpreter;
use failure::Fallible;
use lexer::Scanner;
use parser::Parser;

pub fn run(input: &str) -> Fallible<()> {
    let mut scanner = Scanner::new(input);
    let tokens = scanner.scan_tokens()?;

    let mut parser = Parser::new(&tokens);
    let expr = parser.parse()?;

    let mut interpreter = Interpreter::new();
    interpreter.interpret(expr)?;

    Ok(())
}
