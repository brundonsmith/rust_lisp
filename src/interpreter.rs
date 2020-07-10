
use crate::{model::{Value, Env, RuntimeError, Lambda}};
use std::{collections::HashMap, rc::Rc, cell::RefCell};

fn evaluate_block(env: Rc<RefCell<Env>>, body: &Value) -> Result<Value,RuntimeError> {
  let mut result = None;

  for line in body.as_list().unwrap().into_iter() {
    result = Some(eval(env.clone(), &line));
  }

  return Ok(result.unwrap_or(Value::Nil));
}

pub fn eval(env: Rc<RefCell<Env>>, expression: &Value) -> Value {
  // println!("eval {}", &expression);
  // println!("{}", &env.borrow());

  let result: Result<Value,RuntimeError> = match expression {

    // look up symbol
    Value::Symbol(symbol) => match env.borrow().find(&symbol) {
      Some(expr) => Ok(expr.clone()),
      None => Err(RuntimeError { msg: format!("\"{}\" is not defined", symbol) }),
    },

    // s-expression
    Value::List(list) => {
      match &list.car {

        // special forms
        Value::Symbol(symbol) if symbol == "define" => {
          let symbol = list.cdr.clone().map(|cdr| cdr.car.as_symbol()).unwrap().unwrap();
          let value_expr = list.cdr.clone().unwrap().cdr.clone().unwrap().car.clone();
          let value = eval(env.clone(), &value_expr);

          env.borrow_mut().entries.insert(symbol, value.clone());

          Ok(value)
        },

        Value::Symbol(symbol) if symbol == "let" => {
          let let_env = Rc::new(RefCell::new(Env {
            parent: Some(env.clone()),
            entries: HashMap::new()
          }));
          let declarations = list.cdr.clone().map(|c| c.car.clone()).unwrap();

          for decl in declarations.as_list().unwrap().into_iter() {
            let decl_cons = decl.as_list().unwrap();
            let mut decl_iter = decl_cons.into_iter();
            let symbol = decl_iter.nth(0).unwrap().as_symbol().unwrap();
            let expr = decl_iter.nth(0).unwrap();

            let result = eval(let_env.clone(), &expr);
            let_env.borrow_mut().entries.insert(symbol, result);
          }

          let body = Value::List(list.cdr.clone().unwrap().cdr.clone().unwrap());

          evaluate_block(let_env.clone(), &body)
        },

        Value::Symbol(symbol) if symbol == "lambda" => {
          // println!("{}", &list);
          let argnames = Rc::new(list.cdr.clone().map(|cdr| cdr.car.clone()).unwrap());
          // println!("{}", &argnames);

          let body = Rc::new(list.cdr.clone().map(|cdr| cdr.cdr.clone().map(|body| Value::List(body))).unwrap().unwrap());
          // println!("{}", &body);

          Ok(Value::Lambda(Lambda {
            env: env.clone(),
            argnames,
            body
          }))
        },

        Value::Symbol(symbol) if symbol == "begin" => {
          let body = Value::List(list.cdr.clone().unwrap());

          evaluate_block(env.clone(), &body)
        },

        Value::Symbol(symbol) if symbol == "cond" => {
          let clauses = list.cdr.as_ref().unwrap();
          let mut result = Value::Nil;

          for clause in clauses.into_iter().map(|clause| clause.as_list().unwrap()) {
            let condition = &clause.car;
            let then = &clause.cdr.as_ref().unwrap().car;

            if eval(env.clone(), condition).is_truthy() {
              result = eval(env.clone(), then);
              break;
            }
          }

          Ok(result)
        },

        Value::Symbol(symbol) if symbol == "if" => {
          let remaining = list.cdr.clone().unwrap();
          let mut list_iter = remaining.into_iter();
          let condition = list_iter.nth(0).unwrap();
          let then_result = list_iter.nth(0).unwrap();
          let else_result = list_iter.nth(0);

          if eval(env.clone(), condition).is_truthy() {
            Ok(eval(env.clone(), then_result))
          } else {
            Ok(match else_result {
              Some(v) => eval(env.clone(), v),
              None => Value::Nil
            })
          }
        },

        Value::Symbol(symbol) if symbol == "and" => {
          let remaining = list.cdr.clone().unwrap();
          let mut list_iter = remaining.into_iter();
          let a = list_iter.nth(0).unwrap();
          let b = list_iter.nth(0).unwrap();

          Ok(Value::from_truth(
              eval(env.clone(), a).is_truthy() 
              && eval(env.clone(), b).is_truthy()
          ))
        },

        Value::Symbol(symbol) if symbol == "or" => {
          let remaining = list.cdr.clone().unwrap();
          let mut list_iter = remaining.into_iter();
          let a = list_iter.nth(0).unwrap();
          let b = list_iter.nth(0).unwrap();

          Ok(Value::from_truth(
              eval(env.clone(), a).is_truthy() 
              || eval(env.clone(), b).is_truthy()
          ))
        },

        Value::TailCall(v) => Ok(eval(env.clone(), &v)),

        // function call
        _ => {
          let func = eval(env.clone(), &list.car);
          // println!("{}", &list);
          let args = list.into_iter().skip(1)
            .map(|car| eval(env.clone(), car));

          // if within_function_call && is_return_value && !tail_position_found {
          //   let expr = Value::TailCall(Rc::new(Value::List(Rc::new(ConsCell {
          //     car: func,
          //     cdr: Some(vec_to_cons(&args.collect()).as_list().unwrap())
          //   }))));
          //   println!("tail-calling: {}", &expr);

          //   Ok(expr)
          // } else {
          match func {

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
              evaluate_block(arg_env.clone(), &lamb.body)
            }
            _ => Err(RuntimeError { msg: String::from("Argument 0 is not callable") })
          }
        }
      }
    },

    // plain value
    _ => Ok(expression.clone()),
  };

  match result {
    Ok(val) => {
      if let Value::TailCall(expr) = &val {
        return eval(env.clone(), expr.as_ref());
      } else {
        return val;
      }
    },
    Err(e) => panic!(e.msg)
  };
}
