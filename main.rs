
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
  let ast = parse("
    (begin
      (define fib 
        (lambda (n)
          (cond
            ((== n 0) 0)
            ((== n 1) 1)
            (T (+ (fib (- n 1)) (fib (- n 2)) )))))
      (print (fib 0))
      (print (fib 1))
      (print (fib 2))
      (print (fib 3))
      (print (fib 4))
      (print (fib 5))
      (print (fib 6))
      (print (fib 7))
      (print (fib 8)))");

  // println!("{:?}", &ast);
  println!("{}", &ast);

  println!("{}", eval(env, &ast));
}
