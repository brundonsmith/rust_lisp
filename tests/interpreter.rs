use rust_lisp::default_env;
use rust_lisp::eval;
use rust_lisp::parse;
use rust_lisp::{
    lisp,
    model::{List, Value},
};
use std::{cell::RefCell, rc::Rc};

#[test]
fn eval_basic_expression() {
    let result = eval_str("(+ (* 1 2) (/ 6 3))");

    assert_eq!(result, Value::from_int(4));
}

#[test]
fn eval_quote_1() {
    let result = eval_str("(quote \"stuff\")");

    assert_eq!(result, Value::String(String::from("stuff")));
}

#[test]
fn eval_quote_2() {
    let result = eval_str("(quote (1 2 3))");

    assert_eq!(result, lisp! { (1 2 3) });
}

#[test]
fn eval_quote_tick_list() {
    let result = eval_str("'(1 2 3)");

    assert_eq!(result, lisp! { (1 2 3) });
}

#[test]
fn eval_quote_tick_atom() {
    let result = eval_str("(nth 0 (list '12))");

    assert_eq!(result, Value::from_int(12));
}

#[test]
fn eval_quote_tick_symbol() {
    let result = eval_str("(nth 0 (list 'foo))");

    assert_eq!(result, Value::Symbol(String::from("foo")));
}

#[test]
fn eval_let() {
    let result = eval_str(
        "
    (let ((foo 12)
          (bar (+ 4 3))
          (blah \"stuff\"))
      (print foo)
      (print bar)
      (print blah)
      (list (* foo bar) (+ blah \" also\")))",
    );

    assert_eq!(result, lisp! { (84 "stuff also") });
}

#[test]
#[should_panic]
fn eval_let_scope() {
    let result = eval_str(
        "
    (begin
      (let ((foo 12)
            (bar (+ 4 3))
            (blah \"stuff\"))
        (print foo)
        (print bar)
        (print blah))

      (* foo bar))",
    );

    assert_eq!(result, lisp! { (84 "stuff also") });
}

#[test]
fn eval_set_global() {
    let result = eval_str(
        "
    (begin
      (define foo 12)

      (let ((bar 25))
        (set foo 13))

      foo)",
    );

    assert_eq!(result, Value::from_int(13));
}

#[test]
fn eval_set_local() {
    let result = eval_str(
        "
    (begin
      (define foo 12)

      (let ((bar 25))
        (set bar 13))

      foo)",
    );

    assert_eq!(result, Value::from_int(12));
}

#[test]
#[should_panic]
fn eval_set_undefined() {
    eval_str(
        "
    (begin
      (let ((bar 25))
        (set foo 13)))",
    );
}

#[test]
fn eval_fib() {
    let result = eval_str(
        "
    (begin
      (define fib 
        (lambda (n)
          (cond           ;; some comment
            ((== n 0) 0)
            ((== n 1) 1)
            (T (+ (fib (- n 1)) (fib (- n 2)) ))))) ;;another comment

      (list (fib 0) (fib 1) (fib 2) (fib 3) (fib 4) (fib 5) (fib 6) (fib 7) (fib 8)))",
    );

    assert_eq!(result, lisp! { (0 1 1 2 3 5 8 13 21) });
}

#[test]
fn eval_merge_sort() {
    let result = eval_str(
        "
    (begin

      (defun list-head (lst n) 
        (if (== n 0) 
          (list) 
          (cons (car lst) (list-head (cdr lst) (- n 1)))))

      (defun list-tail (lst n) 
        (if (== n 0) 
          lst 
          (list-tail (cdr lst) (- n 1))))

      (defun merge (lst-a lst-b)
        (cond ((not lst-a) lst-b)
              ((not lst-b) lst-a)
              ((< (car lst-a) (car lst-b)) (cons (car lst-a) (merge (cdr lst-a) lst-b)))
              (T (cons (car lst-b) (merge lst-a (cdr lst-b))))))

      (defun mergesort (lst)
        (if (== (length lst) 1)
          lst
          (merge (mergesort (list-head lst (truncate (length lst) 2)))
                (mergesort (list-tail lst (truncate (length lst) 2))))))

      (mergesort (list 7 2 5 0 1 5)))",
    );

    assert_eq!(result, lisp! { (0 1 2 5 5 7) });
}

#[test]
fn tail_call_test() {
    let result = eval_str(
        "
    (begin
      (defun recurse-test (n)
          (if (> n 0) 
            (begin
              (print n)
              (recurse-test (- n 1)))
            n))
      
      (recurse-test 1000))
  ",
    );

    assert_eq!(result, Value::from_int(0));
}

#[test]
fn rest_parameters_test() {
    let result = eval_str(
        "
    (begin
      (defun foo (a b ...)
        ...)
      
      (foo 1 2 3 4 5))",
    );

    assert_eq!(result, lisp! { (3 4 5) });
}

#[test]
fn calling_empty_fun() {
    let result = eval_str(
        "
    (begin
      (defun foo () ())
      
      (foo))",
    );

    assert_eq!(result, lisp! { () });
}

#[test]
fn closure() {
    let result = eval_str(
        "
    (map (let ((x 3)) (lambda (y) (+ x y))) (list 0 1 2 3 4))
        ",
    );

    assert_eq!(result, lisp! {(3 4 5 6 7)});
}

#[test]
fn map_list_of_lists() {
    let result = eval_str(
        "
        (map (lambda (x) x) (list (list 0 1) (list 2 3)))
      ",
    );

    assert_eq!(result, lisp! {((0 1) (2 3))});
}

#[test]
fn filter_list_of_lists() {
    let result = eval_str(
        "
        (filter (lambda (x) (> (length x) 2))
          (list
            (list 0)
            (list 0 1)
            (list 0 1 2)
            (list 0 1 2 3)))
      ",
    );

    println!("{}", result);

    assert_eq!(result, lisp! {((0 1 2) (0 1 2 3))});
}

#[cfg(test)]
fn eval_str(source: &str) -> Value {
    let ast = parse(source).next().unwrap().unwrap();
    let env = Rc::new(RefCell::new(default_env()));
    return eval(env, &ast).unwrap();
}

// #[bench]
// #[test]
// fn bench_merge_sort() {

//   // let mut v = vec![];
//   // loop {
//   //   v.push("Stuff");
//   // }

//   let source = "
//     (begin
//       (define
//         list-head
//         (lambda (lst n)
//           (if (== n 0)
//             (list)
//             (cons (car lst) (list-head (cdr lst) (- n 1))))))

//       (define
//         list-tail
//         (lambda (lst n)
//           (if (== n 0)
//             lst
//             (list-tail (cdr lst) (- n 1)))))

//       (define
//         merge
//         (lambda (lst-a lst-b)
//           (cond ((not lst-a) lst-b)
//                 ((not lst-b) lst-a)
//                 ((< (car lst-a) (car lst-b)) (cons (car lst-a) (merge (cdr lst-a) lst-b)))
//                 (T (cons (car lst-b) (merge lst-a (cdr lst-b)))))))

//       (define
//         mergesort
//         (lambda (lst)
//           (if (== (length lst) 1)
//             lst
//             (merge (mergesort (list-head lst (truncate (length lst) 2)))
//                   (mergesort (list-tail lst (truncate (length lst) 2)))))))

//       (mergesort (list 75 10 45 26 34 36 10 97 34 64 24 20 24 30 39 39 1 53 20 57 6 24 37 41 51 49 12 78 11 71 16 12 60 42 76 63 64 84 11 31 8 48 41 62 98 2 31 53 37 57 14 85 74 75 98 65 69 37 60 96 33 5 95 21 85 7 31 37 2 78 51 88 70 36 1 18 41 68 1 6 16 61 32 80 63 60 92 61 89 91 33 34 61 21 32 14 64 63 8 91)))";
//   let ast = parse(source).unwrap();

//   let env = Rc::new(RefCell::new(default_env()));

//   let start = SystemTime::now();
//   // for _ in 0..10000 {
//     eval(env.clone(), &ast);
//   // }
//   let end = SystemTime::now();

//   println!("Took {}ms", end.duration_since(start).unwrap().as_millis());
// }
