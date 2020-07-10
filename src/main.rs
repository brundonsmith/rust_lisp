
mod parser;
mod default_environment;
mod interpreter;
mod utils;
mod model;

use parser::parse;
use default_environment::default_env;
use interpreter::eval;
use std::{cell::RefCell, rc::Rc, io};
use std::io::Write;

fn main() {
  let env = Rc::new(RefCell::new(default_env()));

  let mut buf = String::new();
  loop {
    print!("> ");
    io::stdout().flush().unwrap();
    io::stdin().read_line(&mut buf).unwrap();

    println!("{}", eval(env.clone(), &parse(&buf).unwrap()).unwrap());
  }
}