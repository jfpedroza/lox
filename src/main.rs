extern crate lox;

use failure::Fallible;
use lox::error::*;
use lox::*;

fn program() -> Fallible<()> {
    let args = std::env::args_os().skip(1).collect::<Vec<_>>();
    let mut lox = Lox::new();
    match args.len() {
        0 => lox.run_prompt(),
        1 => lox.run_file(&args[0]),
        _ => {
            eprintln!("Usage: lox [script]");
            std::process::exit(64);
        }
    }
}

fn main() {
    match program() {
        Ok(()) => (),
        Err(ref err) => {
            print_err(err);
            std::process::exit(exit_code(err));
        }
    }
}
