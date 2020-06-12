extern crate failure;

use failure::Fallible;

pub fn run(input: &str) -> Fallible<()> {
    println!("Got: {}", input);
    Ok(())
}
