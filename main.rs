
mod model;
mod parser;
mod interpreter;
mod utils;

use crate::parser::parse;
use interpreter::{standard_env, eval};
use std::{cell::RefCell, rc::Rc};

fn main() {
  let env = Rc::new(RefCell::new(standard_env()));
  // let ast = parse("(+ (* 1 2) (/ 6 3))");
  let ast = parse("( (lambda (a) (* a 2)) 3)");

  // println!("{:?}", &ast);
  println!("{}", &ast);

  println!("{}", eval(env, &ast));
}
