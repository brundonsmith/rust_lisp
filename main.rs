
mod model;
mod parser;
mod interpreter;

use crate::parser::parse;
use interpreter::{standard_env, eval};

fn main() {
  let mut env = standard_env();
  let ast = parse("(+ (* 1 2) (/ 6 3))");

  // println!("{:?}", &ast);
  println!("{}", &ast);

  println!("{}", eval(&mut env, &ast));
}
