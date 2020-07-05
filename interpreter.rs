
use crate::{model::{Value, Env, RuntimeError, Lambda}};
use std::{collections::HashMap, rc::Rc, cell::RefCell};

pub fn eval(env: Rc<RefCell<Env>>, expression: &Value) -> Value {
  // println!("eval {}", &expression);
  // println!("{}", &env.borrow());

  match expression {

    // look up symbol
    Value::Symbol(symbol) => match env.borrow_mut().find(&symbol) {
      Some(expr) => expr.clone(),
      None => panic!(format!("\"{}\" is not defined", symbol))
    },

    // s-expression
    Value::List(list) => {
      match &list.car {

        // special forms
        Value::Symbol(symbol) if symbol == "define" => {
          // println!("{}", &list);
          let symbol = list.cdr.clone().map(|cdr| cdr.car.as_symbol()).unwrap().unwrap();
          let value_expr = list.cdr.clone().unwrap().cdr.clone().unwrap().car.clone();
          let value = eval(env.clone(), &value_expr);

          // println!("defined {}", &symbol);
          env.borrow_mut().entries.insert(symbol, value.clone());
          // println!("{}", &env.borrow());

          return value;
        },

        Value::Symbol(symbol) if symbol == "lambda" => {
          // println!("{}", &list);
          let argnames = Rc::new(list.cdr.clone().map(|cdr| cdr.car.clone()).unwrap());
          // println!("{}", &argnames);

          let body = Rc::new(list.cdr.clone().map(|cdr| cdr.cdr.clone().map(|body| Value::List(body))).unwrap().unwrap());
          // println!("{}", &body);

          Value::Lambda(Lambda {
            env: env.clone(),
            argnames,
            body
          })
        },

        Value::Symbol(symbol) if symbol == "begin" => {
          let body = list.cdr.clone().unwrap();

          let mut result = None;
          for line in body.into_iter() {
            result = Some(eval(env.clone(), &line));
          }

          return result.unwrap_or(Value::Nil);
        },

        Value::Symbol(symbol) if symbol == "cond" => {
          let clauses = list.cdr.as_ref().unwrap();

          for clause in clauses.into_iter().map(|clause| clause.as_list().unwrap()) {
            let condition = &clause.car;
            let result = &clause.cdr.as_ref().unwrap().car;

            if eval(env.clone(), condition).is_truthy() {
              return eval(env.clone(), result);
            }
          }

          return Value::Nil;
        },

        Value::Symbol(symbol) if symbol == "if" => {
          let remaining = list.cdr.clone().unwrap();
          let mut list_iter = remaining.into_iter();
          let condition = list_iter.nth(0).unwrap();
          let then_result = list_iter.nth(0).unwrap();
          let else_result = list_iter.nth(0);

          if eval(env.clone(), condition).is_truthy() {
            return eval(env.clone(), then_result);
          } else {
            return match else_result {
              Some(v) => eval(env.clone(), v),
              None => Value::Nil
            };
          }
        },


        // function call
        _ => {
          let func = eval(env.clone(), &list.car);
          // println!("{}", &list);
          let args = list.into_iter().skip(1)
            .map(|car| eval(env.clone(), car));

          let result = match func {

            // call native function
            Value::NativeFunc(func) => func(env.clone(), &args.collect()),

            // call lambda function
            Value::Lambda(lamb) => {
              let argnames = lamb.argnames.as_list().unwrap();
    
              // bind args
              let mut entries: HashMap<String,Value> = HashMap::new();
              
              for (arg_name, arg_value) in argnames.into_iter().zip(args) {
                let name = arg_name.as_symbol().unwrap();
                entries.insert(name, arg_value.clone());
              }
    
              let arg_env = Rc::new(RefCell::new(Env {
                parent: Some(env.clone()),
                entries
              }));
                  
              // evaluate each line of body
              let mut result = None;
              for line in lamb.body.as_list().unwrap().into_iter() {
                result = Some(eval(arg_env.clone(), &line));
              }

              return result.unwrap_or(Value::Nil);
            }
            _ => Err(RuntimeError { msg: String::from("Argument 0 is not callable") })
          };
    
          match result {
            Ok(expr) => expr,
            Err(e) => panic!(e.msg),
          }
        }
      }
    },

    // plain value
    _ => expression.clone(),
  }
}

#[cfg(test)]
mod tests {
  use crate::model::Value;
  use crate::utils::vec_to_cons;
  use crate::{default_environment::default_env, parser::parse, interpreter::eval};
  use std::{cell::RefCell, rc::Rc, time::SystemTime};

  #[test]
  fn eval_basic_expression() {
    let source = "(+ (* 1 2) (/ 6 3))";
    let ast = parse(source).unwrap();

    let env = Rc::new(RefCell::new(default_env()));
    let result = eval(env, &ast);

    assert_eq!(result, Value::Int(4));
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
              (T (+ (fib (- n 1)) (fib (- n 2)) ))))) ;;another comment

        (list (fib 0) (fib 1) (fib 2) (fib 3) (fib 4) (fib 5) (fib 6) (fib 7) (fib 8)))";
    let ast = parse(source).unwrap();

    let env = Rc::new(RefCell::new(default_env()));
    let result = eval(env, &ast);

    assert_eq!(result, vec_to_cons(&vec![ Value::Int(0), Value::Int(1), Value::Int(1), Value::Int(2), Value::Int(3), Value::Int(5), Value::Int(8), Value::Int(13), Value::Int(21) ]));
  }

  #[test]
  fn eval_merge_sort() {
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
    let ast = parse(source).unwrap();

    let env = Rc::new(RefCell::new(default_env()));
    let result = eval(env, &ast);

    assert_eq!(result, vec_to_cons(&vec![ Value::Int(0), Value::Int(1), Value::Int(2), Value::Int(5), Value::Int(5), Value::Int(7) ]));
  }

  // #[bench]
  #[test]
  fn bench_merge_sort() {

    // let mut v = vec![];
    // loop {
    //   v.push("Stuff");
    // }

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

        (mergesort (list 75 10 45 26 34 36 10 97 34 64 24 20 24 30 39 39 1 53 20 57 6 24 37 41 51 49 12 78 11 71 16 12 60 42 76 63 64 84 11 31 8 48 41 62 98 2 31 53 37 57 14 85 74 75 98 65 69 37 60 96 33 5 95 21 85 7 31 37 2 78 51 88 70 36 1 18 41 68 1 6 16 61 32 80 63 60 92 61 89 91 33 34 61 21 32 14 64 63 8 91)))";
    let ast = parse(source).unwrap();

    let env = Rc::new(RefCell::new(default_env()));

    let start = SystemTime::now();
    // for _ in 0..10000 {
      eval(env.clone(), &ast);
    // }
    let end = SystemTime::now();

    println!("Took {}ms", end.duration_since(start).unwrap().as_millis());
  }

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

}
