#![allow(dead_code, unused_macros)]

mod default_environment;
mod interpreter;
mod model;
mod parser;
mod utils;
#[macro_use]
mod macros;

use rust_lisp::start_repl;

fn main() {
    start_repl(None);
}
