
mod model;
mod parser;
mod interpreter;
mod utils;
mod default_environment;

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

    println!("{}", eval(env.clone(), &parse(&buf)));
  }
}


#[cfg(test)]
mod tests {
  use crate::model::Value;
  use crate::utils::vec_to_cons;
  use crate::{default_environment::default_env, parser::parse, interpreter::eval};
  use std::{cell::RefCell, rc::Rc};

  #[test]
  fn test_basic_expression() {
    let source = "(+ (* 1 2) (/ 6 3))";
    let ast = parse(source);

    let env = Rc::new(RefCell::new(default_env()));
    let result = eval(env, &ast);

    assert_eq!(result, Value::Int(4));
  }

  #[test]
  fn test_fib() {
    let source = "
      (begin
        (define fib 
          (lambda (n)
            (cond           ;; some comment
              ((== n 0) 0)
              ((== n 1) 1)
              (T (+ (fib (- n 1)) (fib (- n 2)) ))))) ;;another comment

        (list (fib 0) (fib 1) (fib 2) (fib 3) (fib 4) (fib 5) (fib 6) (fib 7) (fib 8)))";
    let ast = parse(source);

    let env = Rc::new(RefCell::new(default_env()));
    let result = eval(env, &ast);

    assert_eq!(result, vec_to_cons(&vec![ Value::Int(0), Value::Int(1), Value::Int(1), Value::Int(2), Value::Int(3), Value::Int(5), Value::Int(8), Value::Int(13), Value::Int(21) ]));
  }

  #[test]
  fn test_merge_sort() {
    let source = "
      (begin
        (define 
          list-head 
          (lambda (lst n) 
            (if (== n 0) 
              (list) 
              (cons (car lst) (list-head (cdr lst) (- n 1))))))

        (define
          list-tail 
          (lambda (lst n) 
            (if (== n 0) 
              lst 
              (list-tail (cdr lst) (- n 1)))))

        (define 
          merge 
          (lambda (lst-a lst-b)
            (cond ((not lst-a) lst-b)
                  ((not lst-b) lst-a)
                  ((< (car lst-a) (car lst-b)) (cons (car lst-a) (merge (cdr lst-a) lst-b)))
                  (T (cons (car lst-b) (merge lst-a (cdr lst-b)))))))

        (define 
          mergesort 
          (lambda (lst)
            (if (== (length lst) 1)
              lst
              (merge (mergesort (list-head lst (truncate (length lst) 2)))
                    (mergesort (list-tail lst (truncate (length lst) 2)))))))

        (mergesort (list 7 2 5 0 1 5)))";
    let ast = parse(source);

    let env = Rc::new(RefCell::new(default_env()));
    let result = eval(env, &ast);

    assert_eq!(result, vec_to_cons(&vec![ Value::Int(0), Value::Int(1), Value::Int(2), Value::Int(5), Value::Int(5), Value::Int(7) ]));
  }

}
