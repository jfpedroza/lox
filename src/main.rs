extern crate lox;

use failure::{Error, Fallible, ResultExt};
use lox::*;
use rustyline::{config::Configurer, error::ReadlineError, Editor};
use std::ffi::OsStr;
use std::io::{stdin, Read};
use std::path::Path;

fn main() {
    let args = std::env::args_os().skip(1).collect::<Vec<_>>();
    let res = match args.len() {
        0 => run_prompt(),
        1 => run_file(&args[0]),
        _ => {
            eprintln!("Usage: lox [script]");
            std::process::exit(64);
        }
    };

    match res {
        Ok(()) => (),
        Err(err) => {
            print_err(err);
            std::process::exit(65);
        }
    }
}

fn run_file(path: &OsStr) -> Fallible<()> {
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

    run(&content)
}

fn run_prompt() -> Fallible<()> {
    let mut rl = Editor::<()>::new();
    rl.set_auto_add_history(true);

    println!("Lox 0.0.1");
    println!("Press Ctrl+D to exit\n");

    loop {
        match rl.readline("\x1b[1;34mlox\x1b[0m> ") {
            Ok(line) if line.is_empty() => (),
            Ok(line) => match run(&line) {
                Ok(()) => (),
                Err(err) => print_err(err),
            },
            Err(ReadlineError::Interrupted) => (),
            Err(ReadlineError::Eof) => break,
            Err(err) => return Err(err.into()),
        }
    }

    Ok(())
}

fn print_err(err: Error) {
    let mut fail = err.as_fail();
    eprintln!("Error: {}", fail);
    while let Some(cause) = fail.cause() {
        eprintln!("> {}", cause);
        fail = cause;
    }
}
