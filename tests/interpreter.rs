
use std::{cell::RefCell, rc::Rc, time::SystemTime};
use rust_lisp::model::Value;
use rust_lisp::parse;
use rust_lisp::eval;
use rust_lisp::default_env;
use rust_lisp::utils::vec_to_cons;

#[test]
fn eval_basic_expression() {
  let source = "(+ (* 1 2) (/ 6 3))";
  let ast = parse(source).unwrap();

  let env = Rc::new(RefCell::new(default_env()));
  let result = eval(env, &ast).unwrap();

  assert_eq!(result, Value::Int(4));
}

#[test]
fn eval_let() {
  let source = "(let ((foo 12)
                            (bar (+ 4 3))
                            (blah \"stuff\"))
                        (print foo)
                        (print bar)
                        (print blah)
                        (list (* foo bar) (+ blah \" also\")))";
  let ast = parse(source).unwrap();

  let env = Rc::new(RefCell::new(default_env()));
  let result = eval(env, &ast).unwrap();

  assert_eq!(result, vec_to_cons(&vec![ Value::Int(84), Value::String(String::from("stuff also")) ]));
}

#[test]
#[should_panic]
fn eval_let_scope() {
  let source = "(begin
    (let ((foo 12)
          (bar (+ 4 3))
          (blah \"stuff\"))
      (print foo)
      (print bar)
      (print blah))

    (* foo bar))";
  let ast = parse(source).unwrap();

  let env = Rc::new(RefCell::new(default_env()));
  let result = eval(env, &ast).unwrap();

  assert_eq!(result, vec_to_cons(&vec![ Value::Int(84), Value::String(String::from("stuff also")) ]));
}

#[test]
fn eval_fib() {
  let source = "
    (begin
      (define fib 
        (lambda (n)
          (cond           ;; some comment
            ((== n 0) 0)
            ((== n 1) 1)
            (#t (+ (fib (- n 1)) (fib (- n 2)) ))))) ;;another comment

      (list (fib 0) (fib 1) (fib 2) (fib 3) (fib 4) (fib 5) (fib 6) (fib 7) (fib 8)))";
  let ast = parse(source).unwrap();

  let env = Rc::new(RefCell::new(default_env()));
  let result = eval(env, &ast).unwrap();

  assert_eq!(result, vec_to_cons(&vec![ Value::Int(0), Value::Int(1), Value::Int(1), Value::Int(2), Value::Int(3), Value::Int(5), Value::Int(8), Value::Int(13), Value::Int(21) ]));
}

#[test]
fn eval_fib_deep() {
  let source = "
    (begin
      (define fib-normal
        (lambda (n)
          (+ (fib (- n 1)) (fib (- n 2)) )))

      (define fib 
        (lambda (n)
          (cond           ;; some comment
            ((== n 0) 0)
            ((== n 1) 1)
            (#t (fib-normal n))))) ;;another comment

      (fib 10))";
  let ast = parse(source).unwrap();

  let env = Rc::new(RefCell::new(default_env()));
  let result = eval(env, &ast).unwrap();
  println!("result: {}", result);

  // assert_eq!(result, Value::Int(55));
}

#[test]
fn eval_merge_sort() {
  let source = "
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
                (#t (cons (car lst-b) (merge lst-a (cdr lst-b))))))

      (defun mergesort (lst)
          (if (== (length lst) 1)
            lst
            (merge (mergesort (list-head lst (truncate (length lst) 2)))
                  (mergesort (list-tail lst (truncate (length lst) 2))))))

      (mergesort (list 7 2 5 0 1 5)))";
  let ast = parse(source).unwrap();

  let env = Rc::new(RefCell::new(default_env()));
  let result = eval(env, &ast).unwrap();

  assert_eq!(result, vec_to_cons(&vec![ Value::Int(0), Value::Int(1), Value::Int(2), Value::Int(5), Value::Int(5), Value::Int(7) ]));
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
//                 (#t (cons (car lst-b) (merge lst-a (cdr lst-b)))))))

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

// #[test]
// fn tail_call_test() {
//   let code = String::from("
//     (begin
//       (define 
//         infinite 
//         (lambda (n)
//           (infinite (print (+ n 1)))))
      
//       (infinite 0))
//   ");
//   let env = Rc::new(RefCell::new(default_env()));
//   eval(env.clone(), &parse(&code));
// }
