
use std::{cell::RefCell, rc::Rc, time::SystemTime};
use rust_lisp::model::Value;
use rust_lisp::parse;
use rust_lisp::eval;
use rust_lisp::default_env;
use rust_lisp::utils::vec_to_cons;

#[test]
fn one() {
  let source = "(car (list 1 2 3))";
  let ast = parse(source).unwrap();

  let env = Rc::new(RefCell::new(default_env()));
  let result = eval(env, &ast, false, false);

  assert_eq!(result, Value::Int(1));
}

#[test]
fn two() {
  let source = "(car (list (list 1 2 3) 4 5 6))";
  let ast = parse(source).unwrap();

  let env = Rc::new(RefCell::new(default_env()));
  let result = eval(env, &ast, false, false);

  assert_eq!(result, vec_to_cons(&vec![ Value::Int(1), Value::Int(2), Value::Int(3) ]));
}

#[test]
#[should_panic]
fn three() {
  let source = "(car (list))";
  let ast = parse(source).unwrap();

  let env = Rc::new(RefCell::new(default_env()));
  eval(env, &ast, false, false);
}

#[test]
fn four() {
  let source = "(car (car (list (list \"hotdogs\") \"and\")))";
  let ast = parse(source).unwrap();

  let env = Rc::new(RefCell::new(default_env()));
  let result = eval(env, &ast, false, false);

  assert_eq!(result, Value::String(String::from("hotdogs")));
}

#[test]
fn five() {
  let source = "(cdr (list 1 2 3))";
  let ast = parse(source).unwrap();

  let env = Rc::new(RefCell::new(default_env()));
  let result = eval(env, &ast, false, false);

  assert_eq!(result, vec_to_cons(&vec![ Value::Int(2), Value::Int(3) ]));
}

#[test]
#[should_panic]
fn six() {
  let source = "(cons (list 1 2 3) 4)";
  let ast = parse(source).unwrap();

  let env = Rc::new(RefCell::new(default_env()));
  eval(env, &ast, false, false);
}


#[test]
fn seven() {
  let source = "(cons 4 (list 1 2 3))";
  let ast = parse(source).unwrap();

  let env = Rc::new(RefCell::new(default_env()));
  let result = eval(env, &ast, false, false);

  assert_eq!(result, vec_to_cons(&vec![ Value::Int(4), Value::Int(1), Value::Int(2), Value::Int(3) ]));
}