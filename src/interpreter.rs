
use crate::model::{Value, Env, RuntimeError, Lambda};
use std::{collections::HashMap, rc::Rc, cell::RefCell};

pub fn eval(env: Rc<RefCell<Env>>, expression: &Value, tail_position_found: bool, within_function_call: bool) -> Value {
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
          let value = eval(env.clone(), &value_expr, tail_position_found, within_function_call);

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
            result = Some(eval(env.clone(), &line, tail_position_found, within_function_call));
          }

          return result.unwrap_or(Value::Nil);
        },

        Value::Symbol(symbol) if symbol == "cond" => {
          let clauses = list.cdr.as_ref().unwrap();

          for clause in clauses.into_iter().map(|clause| clause.as_list().unwrap()) {
            let condition = &clause.car;
            let result = &clause.cdr.as_ref().unwrap().car;

            if eval(env.clone(), condition, tail_position_found, within_function_call).is_truthy() {
              return eval(env.clone(), result, tail_position_found, within_function_call);
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

          if eval(env.clone(), condition, tail_position_found, within_function_call).is_truthy() {
            return eval(env.clone(), then_result, tail_position_found, within_function_call);
          } else {
            return match else_result {
              Some(v) => eval(env.clone(), v, tail_position_found, within_function_call),
              None => Value::Nil
            };
          }
        },

        Value::Symbol(symbol) if symbol == "and" => {
          let remaining = list.cdr.clone().unwrap();
          let mut list_iter = remaining.into_iter();
          let a = list_iter.nth(0).unwrap();
          let b = list_iter.nth(0).unwrap();

          return Value::from_truth(
              eval(env.clone(), a, tail_position_found, within_function_call).is_truthy() 
              && eval(env.clone(), b, tail_position_found, within_function_call).is_truthy()
          );
        },

        Value::Symbol(symbol) if symbol == "or" => {
          let remaining = list.cdr.clone().unwrap();
          let mut list_iter = remaining.into_iter();
          let a = list_iter.nth(0).unwrap();
          let b = list_iter.nth(0).unwrap();

          return Value::from_truth(
              eval(env.clone(), a, tail_position_found, within_function_call).is_truthy() 
              || eval(env.clone(), b, tail_position_found, within_function_call).is_truthy()
          );
        },


        // function call
        _ => {
          let func = eval(env.clone(), &list.car, true, within_function_call);
          // println!("{}", &list);
          let args = list.into_iter().skip(1)
            .map(|car| eval(env.clone(), car, true, within_function_call))
            .map(|arg_value| match arg_value {
              Value::TailCall(v) => eval(env.clone(), &v, true, within_function_call),
              _ => arg_value
            });

          // if !tail_position_found && within_function_call {
          //   println!("tail-calling: {}", expression);
          //   return Value::TailCall(Rc::new(Value::List(Rc::new(ConsCell {
          //     car: func,
          //     cdr: Some(vec_to_cons(&args.collect()).as_list().unwrap())
          //   }))));
          // }

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
                result = Some(eval(arg_env.clone(), &line, tail_position_found, true));
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
