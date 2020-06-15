#![feature(peekable_next_if)]
extern crate failure;
#[macro_use]
extern crate failure_derive;

mod lexer;
mod location;

use failure::Fallible;
use lexer::Scanner;

pub fn run(input: &str) -> Fallible<()> {
    let mut scanner = Scanner::new(input);
    let tokens = scanner.scan_tokens()?;

    for token in tokens {
        println!("{}", token);
    }

    Ok(())
}
