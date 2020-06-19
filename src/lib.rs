extern crate failure;
#[macro_use]
extern crate failure_derive;

mod expr;
mod lexer;
mod location;
mod parser;
mod utils;

use failure::Fallible;
use lexer::Scanner;
use parser::Parser;

pub fn run(input: &str) -> Fallible<()> {
    let mut scanner = Scanner::new(input);
    let tokens = scanner.scan_tokens()?;

    let mut parser = Parser::new(&tokens);
    let expr = parser.parse()?;

    println!("{:?}", expr);

    Ok(())
}
