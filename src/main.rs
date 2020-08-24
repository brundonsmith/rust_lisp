
#![allow(dead_code)]

mod parser;
mod default_environment;
mod interpreter;
mod utils;
mod model;

use std::{cell::RefCell, rc::Rc, io};
use std::io::Write;

use rust_lisp::{default_env, parse, eval};

fn main() {
  // create a base environment
  let env = Rc::new(RefCell::new(default_env()));

  let mut buf = String::new();
  loop {
    print!("> ");
    io::stdout().flush().unwrap();
    io::stdin().read_line(&mut buf).unwrap();

    for expr in parse(&buf).unwrap() {
      println!("{}", eval(env.clone(), &expr).unwrap());
    }
  }
}