
#![allow(dead_code,unused_macros)]

mod parser;
mod default_environment;
mod interpreter;
mod utils;
mod model;
#[macro_use]
mod macros;

use rust_lisp::start_repl;

fn main() {
  start_repl(None);
}