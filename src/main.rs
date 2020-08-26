
#![allow(dead_code)]

mod parser;
mod default_environment;
mod interpreter;
mod utils;
mod model;

use rust_lisp::start_repl;

fn main() {
  start_repl(None);
}