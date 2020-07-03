
use crate::{utils::vec_to_cons, model::{Value, Env, RuntimeError, ConsCell, Lambda}};
use std::{collections::HashMap, rc::Rc, cell::RefCell};

pub fn standard_env() -> Env {
  let mut entries = HashMap::new();

  entries.insert(
    String::from("car"),
    Value::NativeFunc(
      |_env, args| {
        let list = args.get(0).and_then(|a| a.as_list());

        return match list {
          Some(cons_cell) => Ok(cons_cell.car.clone()),
          None => Err(RuntimeError { msg: String::from("Arg must be list") })
        };
      }));
    
  entries.insert(
    String::from("cdr"),
    Value::NativeFunc(
      |_env, args| {
        let list = args.get(0).and_then(|a| a.as_list());

        return match list {
          Some(cons_cell) => Ok(
            match cons_cell.cdr.as_ref() {
              Some(cell) => Value::List(cell.clone()),
              None => Value::Nil
            }
          ),
          None => Err(RuntimeError { msg: String::from("Arg must be list") })
        };
      }));
    
  entries.insert(
    String::from("cons"),
    Value::NativeFunc(
      |_env, args| {
        let car: Option<&Value> = args.get(0);
        let cdr: Option<Rc<ConsCell>> = args.get(1).and_then(|a| a.as_list());

        return match car {
          Some(car) => match cdr {
            Some(cdr) => Ok(Value::List(Rc::new(ConsCell {
              car: car.clone(),
              cdr: Some(cdr.clone())
            }))),
            None => Err(RuntimeError { msg: String::from("Arg 2 must be a list") })
          },
          None => Err(RuntimeError { msg: String::from("Requires 2 args") })
        };
      }));
    
  entries.insert(
    String::from("list"),
    Value::NativeFunc(
      |_env, args| Ok(vec_to_cons(args))));

  entries.insert(
    String::from("+"), 
    Value::NativeFunc(
      |_env, args| {
        let a = args.get(0).and_then(|a| a.as_int());
        let b = args.get(1).and_then(|a| a.as_int());

        if a.is_some() && b.is_some() {
          return Ok(Value::Int(a.unwrap() + b.unwrap()));
        }

        let a = args.get(0).and_then(|a| a.as_float());
        let b = args.get(1).and_then(|a| a.as_float());

        if a.is_some() && b.is_some() {
          return Ok(Value::Float(a.unwrap() + b.unwrap()));
        }

        return Err(RuntimeError { msg: String::from("Args must be numbers") });
      }));

    
  entries.insert(
    String::from("*"), 
    Value::NativeFunc(
      |_env, args| {
        let a = args.get(0).and_then(|a| a.as_int());
        let b = args.get(1).and_then(|a| a.as_int());

        if a.is_some() && b.is_some() {
          return Ok(Value::Int(a.unwrap() * b.unwrap()));
        }

        let a = args.get(0).and_then(|a| a.as_float());
        let b = args.get(1).and_then(|a| a.as_float());

        if a.is_some() && b.is_some() {
          return Ok(Value::Float(a.unwrap() * b.unwrap()));
        }

        return Err(RuntimeError { msg: String::from("Args must be numbers") });
      }));

    
  entries.insert(
    String::from("/"), 
    Value::NativeFunc(
      |_env, args| {
        let a = args.get(0).and_then(|a| a.as_int());
        let b = args.get(1).and_then(|a| a.as_int());

        if a.is_some() && b.is_some() {
          return Ok(Value::Int(a.unwrap() / b.unwrap()));
        }

        let a = args.get(0).and_then(|a| a.as_float());
        let b = args.get(1).and_then(|a| a.as_float());

        if a.is_some() && b.is_some() {
          return Ok(Value::Float(a.unwrap() / b.unwrap()));
        }

        return Err(RuntimeError { msg: String::from("Args must be numbers") });
      }));

  Env {
    parent: None,
    entries
  }
}

pub fn eval(env: Rc<RefCell<Env>>, expression: &Value) -> Value {
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
        Value::Symbol(symbol) if symbol == "lambda" => {
          let argnames = Rc::new(list.cdr.clone().map(|cdr| cdr.car.clone()).unwrap());

          let body = Rc::new(list.cdr.clone().map(|cdr| cdr.cdr.clone().map(|body| Value::List(body))).unwrap().unwrap());

          Value::Lambda(Lambda {
            env: env.clone(),
            argnames,
            body
          })
        },

        // function call
        _ => {
          let func = eval(env.clone(), &list.car);

          let args = list.as_ref().cdr.as_ref().unwrap().into_iter()
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
