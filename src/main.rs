extern crate lox;

use lox::*;
use rustyline::{config::Configurer, error::ReadlineError, Editor};
use std::ffi::OsStr;
use std::path::Path;

fn main() {
    let args = std::env::args_os().skip(1).collect::<Vec<_>>();

    if args.len() > 1 {
        eprintln!("Usage: lox [script]");
        std::process::exit(64);
    } else if args.len() == 1 {
        run_file(&args[0]).unwrap();
    } else {
        run_prompt();
    }
}

fn run_file(path: &OsStr) -> Result<(), std::io::Error> {
    let path = Path::new(path);
    let content = std::fs::read_to_string(path)?;
    Ok(run(&content))
}

fn run_prompt() {
    let mut rl = Editor::<()>::new();
    rl.set_auto_add_history(true);

    println!("Lox 0.0.1");
    println!("Press Ctrl+D to exit\n");

    loop {
        match rl.readline("\x1b[1;34mlox\x1b[0m> ") {
            Ok(line) => {
                if !line.is_empty() {
                    run(&line);
                }
            }
            Err(ReadlineError::Interrupted) => {}
            Err(ReadlineError::Eof) => break,
            Err(err) => {
                eprintln!("Error: {:?}", err);
                break;
            }
        }
    }
}
