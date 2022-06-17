use rust_lisp::{
    default_env,
    interpreter::eval,
    lisp,
    model::{IntType, RuntimeError, Value},
    parser::parse,
};
use std::{cell::RefCell, rc::Rc};

#[test]
fn one() {
    assert_eq!(
        eval_str("(car (list 1 2 3))"),
        Ok(Value::from(Into::<IntType>::into(1)))
    );
}

#[test]
fn two() {
    assert_eq!(
        eval_str("(car (list (list 1 2 3) 4 5 6))"),
        Ok(lisp! { (1 2 3) })
    );
}

#[test]
fn three() {
    assert_eq!(
        eval_str("(car (list))"),
        Err(RuntimeError {
            msg: "Attempted to apply car on nil".to_owned()
        })
    );
}

#[test]
fn four() {
    assert_eq!(
        eval_str("(car (car (list (list \"hotdogs\") \"and\")))"),
        Ok(lisp! { "hotdogs" })
    );
}

#[test]
fn five() {
    assert_eq!(eval_str("(cdr (list 1 2 3))"), Ok(lisp! { (2 3) }));
}

#[test]
fn six() {
    assert_eq!(
        eval_str("(cons (list 1 2 3) 4)"),
        Err(RuntimeError {
            msg: "\"cons\" requires argument 2 to be a list; got 4".to_owned()
        })
    );
}

#[test]
fn seven() {
    assert_eq!(eval_str("(cons 4 (list 1 2 3))"), Ok(lisp! { (4 1 2 3) }));
}

#[cfg(test)]
fn eval_str(source: &str) -> Result<Value, RuntimeError> {
    let ast = parse(source).next().unwrap().unwrap();
    let env = Rc::new(RefCell::new(default_env()));
    return eval(env, &ast);
}
