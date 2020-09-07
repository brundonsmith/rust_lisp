
use std::rc::Rc;
use rust_lisp::{parse, model::{Value}};

#[test]
fn parse_basic_expression() {
  let source = "
  (list 
    (* 1 2)  ;; a comment
    (/ 6 3 \"foo\"))";
  let ast = parse(source).next().unwrap().unwrap();

  // assert_eq!(ast, Value::List(Rc::new(ConsCell {
  //   car: Value::Symbol(String::from("list")),
  //   cdr: Some(Rc::new(ConsCell {
  //     car: Value::List(Rc::new(ConsCell {
  //       car: Value::Symbol(String::from("*")),
  //       cdr: Some(Rc::new(ConsCell {
  //         car: Value::Int(1),
  //         cdr: Some(Rc::new(ConsCell {
  //           car: Value::Int(2),
  //           cdr: None
  //         }))
  //       }))
  //     })),
  //     cdr: Some(Rc::new(ConsCell {
  //       car: Value::List(Rc::new(ConsCell {
  //         car: Value::Symbol(String::from("/")),
  //         cdr: Some(Rc::new(ConsCell {
  //           car: Value::Int(6),
  //           cdr: Some(Rc::new(ConsCell {
  //             car: Value::Int(3),
  //             cdr: Some(Rc::new(ConsCell {
  //               car: Value::String(String::from("foo")),
  //               cdr: None
  //             }))
  //           }))
  //         }))
  //       })),
  //       cdr: None
  //     }))
  //   }))
  // })));
}

#[test]
fn parse_nil() {
  let source = "()";
  let ast = parse(source).next().unwrap().unwrap();

  assert_eq!(ast, Value::NIL);
}

#[test]
fn parse_multiple_lines() {
  let source = "
    (print 1)
    (print 2)
    (print 3)";
  let ast = parse(source).map(|res| res.unwrap()).collect::<Vec<_>>();

  // assert_eq!(ast, vec![
  //   Value::List(Rc::new(ConsCell {
  //     car: Value::Symbol(String::from("print")),
  //     cdr: Some(Rc::new(ConsCell {
  //       car: Value::Int(1),
  //       cdr: None
  //     }))
  //   })),

  //   Value::List(Rc::new(ConsCell {
  //     car: Value::Symbol(String::from("print")),
  //     cdr: Some(Rc::new(ConsCell {
  //       car: Value::Int(2),
  //       cdr: None
  //     }))
  //   })),

  //   Value::List(Rc::new(ConsCell {
  //     car: Value::Symbol(String::from("print")),
  //     cdr: Some(Rc::new(ConsCell {
  //       car: Value::Int(3),
  //       cdr: None
  //     }))
  //   }))
  // ]);
}

// #[test]
// fn test_fib() {
//   let source = "
//     (begin
//       (define fib 
//         (lambda (n)
//           (cond           ;; some comment
//             ((== n 0) 0)
//             ((== n 1) 1)
//             (T (+ (fib (- n 1)) (fib (- n 2)) ))))) ;;another comment

//       (list (fib 0) (fib 1) (fib 2) (fib 3) (fib 4) (fib 5) (fib 6) (fib 7) (fib 8)))";
//   let ast = parse(source);

//   let env = Rc::new(RefCell::new(default_env()));
//   let result = eval(env, &ast);

//   assert_eq!(result, vec_to_cons(&vec![ Value::Int(0), Value::Int(1), Value::Int(1), Value::Int(2), Value::Int(3), Value::Int(5), Value::Int(8), Value::Int(13), Value::Int(21) ]));
// }

// #[test]
// fn test_merge_sort() {
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

//       (mergesort (list 7 2 5 0 1 5)))";
//   let ast = parse(source);

//   let env = Rc::new(RefCell::new(default_env()));
//   let result = eval(env, &ast);

//   assert_eq!(result, vec_to_cons(&vec![ Value::Int(0), Value::Int(1), Value::Int(2), Value::Int(5), Value::Int(5), Value::Int(7) ]));
// }
