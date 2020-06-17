extern crate failure;
#[macro_use]
extern crate failure_derive;

pub mod lexer;
pub mod location;
pub mod utils;

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
