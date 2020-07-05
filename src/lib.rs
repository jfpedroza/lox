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

use ansi_term::Color::{Blue, Cyan, Green, Purple};
use error::print_err;
use eval::Interpreter;
use failure::{Fallible, ResultExt};
use lexer::Scanner;
use parser::Parser;
use rustyline::{config::Configurer, error::ReadlineError, Editor};
use std::ffi::OsStr;
use std::io::{stdin, Read};
use std::path::Path;
use stmt::{Stmt, StmtKind};
use value::Value;

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

        println!("Lox 0.0.2");
        println!("Press Ctrl+D to exit\n");

        let prompt = format!("{}> ", Blue.bold().paint("lox"));

        loop {
            match rl.readline(&prompt) {
                Ok(line) if line.is_empty() => (),
                Ok(line) => match self.run_prompt_line(&line) {
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

    fn run_prompt_line(&mut self, input: &str) -> Fallible<()> {
        let mut scanner = Scanner::new(input);
        let tokens = scanner.scan_tokens()?;

        let mut parser = Parser::new(&tokens);
        parser.allow_expression = true;
        let mut stmts = parser.parse()?;

        if stmts.len() == 1 {
            match stmts.pop().unwrap() {
                Stmt {
                    kind: StmtKind::Expression(expr),
                    ..
                } => {
                    let val = self.inter.evaluate(expr)?;
                    let output = match val {
                        Value::Integer(int) => Blue.paint(int.to_string()),
                        Value::Float(float) => Cyan.paint(float.to_string()),
                        Value::Str(string) => {
                            Green.paint(format!("\"{}\"", utils::escape_string(&string)))
                        }
                        Value::Boolean(boolean) => Purple.paint(boolean.to_string()),
                        Value::Nil => Purple.paint("nil"),
                    };
                    println!("=> {}", output);
                }
                stmt => {
                    self.inter.execute(stmt)?;
                }
            }
        } else {
            self.inter.interpret(stmts)?;
        }

        Ok(())
    }
}
