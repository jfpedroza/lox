extern crate failure;
#[macro_use]
extern crate failure_derive;

pub mod error;
mod eval;
mod expr;
mod lexer;
mod location;
mod parser;
mod stmt;
#[cfg(test)]
mod test_utils;
mod utils;
mod value;

use error::print_err;
use eval::Interpreter;
use failure::{Fallible, ResultExt};
use lexer::Scanner;
use parser::Parser;
use rustyline::{config::Configurer, error::ReadlineError, Editor};
use std::ffi::OsStr;
use std::io::{stdin, Read};
use std::path::Path;

pub struct Lox {
    inter: Interpreter,
}

impl Lox {
    pub fn new() -> Self {
        Lox {
            inter: Interpreter::new(),
        }
    }

    pub fn run(&mut self, input: &str) -> Fallible<()> {
        let mut scanner = Scanner::new(input);
        let tokens = scanner.scan_tokens()?;

        let mut parser = Parser::new(&tokens);
        let stmts = parser.parse()?;

        self.inter.interpret(stmts)?;

        Ok(())
    }

    pub fn run_file(&mut self, path: &OsStr) -> Fallible<()> {
        let content = if path == "-" {
            let mut content = String::new();
            stdin()
                .lock()
                .read_to_string(&mut content)
                .context("Could not read from stdin")?;
            content
        } else {
            let path = Path::new(path);
            let context = format!("Could not read '{}'", path.display());
            std::fs::read_to_string(path).context(context)?
        };

        self.run(&content)
    }

    pub fn run_prompt(&mut self) -> Fallible<()> {
        let mut rl = Editor::<()>::new();
        rl.set_auto_add_history(true);

        println!("Lox 0.0.1");
        println!("Press Ctrl+D to exit\n");

        loop {
            match rl.readline("\x1b[1;34mlox\x1b[0m> ") {
                Ok(line) if line.is_empty() => (),
                Ok(line) => match self.run(&line) {
                    Ok(()) => (),
                    Err(err) => print_err(&err),
                },
                Err(ReadlineError::Interrupted) => (),
                Err(ReadlineError::Eof) => break,
                Err(err) => return Err(err.into()),
            }
        }

        Ok(())
    }
}
