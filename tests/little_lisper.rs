use rust_lisp::default_env;
use rust_lisp::eval;
use rust_lisp::parse;
use rust_lisp::{
    lisp,
    model::{List, Value},
};
use std::{cell::RefCell, rc::Rc};

#[test]
fn one() {
    assert_eq!(eval_str("(car (list 1 2 3))"), Value::from_int(1));
}

#[test]
fn two() {
    assert_eq!(
        eval_str("(car (list (list 1 2 3) 4 5 6))"),
        lisp! { (1 2 3) }
    );
}

#[test]
#[should_panic]
fn three() {
    eval_str("(car (list))");
}

#[test]
fn four() {
    assert_eq!(
        eval_str("(car (car (list (list \"hotdogs\") \"and\")))"),
        Value::String(String::from("hotdogs"))
    );
}

#[test]
fn five() {
    assert_eq!(eval_str("(cdr (list 1 2 3))"), lisp! { (2 3) });
}

#[test]
#[should_panic]
fn six() {
    eval_str("(cons (list 1 2 3) 4)");
}

#[test]
fn seven() {
    assert_eq!(eval_str("(cons 4 (list 1 2 3))"), lisp! { (4 1 2 3) });
}

fn eval_str(source: &str) -> Value {
    let ast = parse(source).next().unwrap().unwrap();
    let env = Rc::new(RefCell::new(default_env()));
    return eval(env, &ast).unwrap();
}
