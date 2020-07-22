
use std::{rc::Rc, collections::HashMap};
use crate::{utils::{require_list_parameter, vec_to_cons, require_parameter, require_int_parameter, vec_refs_to_cons}, model::{Value, Env, RuntimeError, ConsCell}, interpreter::eval};

/// Initialize an instance of `Env` with several core Lisp functions implemented
/// in Rust. **Without this, you will only have access to the functions you 
/// implement yourself.**
pub fn default_env() -> Env {
  let mut entries = HashMap::new();

  entries.insert(
    String::from("print"),
    Value::NativeFunc(
      |_env, args| {
        let expr = require_parameter("print", args, 0)?;

        println!("{}", &expr);
        return Ok(expr.clone());
      }));

  entries.insert(
    String::from("null?"),
    Value::NativeFunc(
      |_env, args| {
        let val = require_parameter("null?", args, 0)?;

        Ok(Value::from_truth(*val == Value::Nil))
      }));
    
  entries.insert(
    String::from("number?"),
    Value::NativeFunc(
      |_env, args| {
        let val = require_parameter("number?", args, 0)?;

        Ok(match val {
          Value::Int(_) => Value::True,
          Value::Float(_) => Value::True,
          _ => Value::Nil,
        })
      }));
  
  entries.insert(
    String::from("symbol?"),
    Value::NativeFunc(
      |_env, args| {
        let val = require_parameter("symbol?", args, 0)?;

        Ok(match val {
          Value::Symbol(_) => Value::True,
          _ => Value::Nil,
        })
      }));

  entries.insert(
    String::from("boolean?"),
    Value::NativeFunc(
      |_env, args| {
        let val = require_parameter("boolean?", args, 0)?;

        Ok(match val {
          Value::True => Value::True,
          Value::False => Value::True,
          _ => Value::Nil,
        })
      }));
    
  entries.insert(
    String::from("procedure?"),
    Value::NativeFunc(
      |_env, args| {
        let val = require_parameter("procedure?", args, 0)?;

        Ok(match val {
          Value::Lambda(_) => Value::True,
          Value::NativeFunc(_) => Value::True,
          _ => Value::Nil,
        })
      }));

  entries.insert(
    String::from("pair?"),
    Value::NativeFunc(
      |_env, args| {
        let val = require_parameter("pair?", args, 0)?;

        Ok(match val {
          Value::List(_) => Value::True,
          _ => Value::Nil,
        })
      }));

  entries.insert(
    String::from("car"),
    Value::NativeFunc(
      |_env, args| {
        let list = require_list_parameter("car", args, 0)?;

        return match list {
          Value::List(c) => Ok(c.car.clone()),
          Value::Nil => Err(RuntimeError { msg: String::from("Attempted to apply car on nil") }),
          _ => panic!("Argument validation didn't work properly"),
        };
      }));
    
  entries.insert(
    String::from("cdr"),
    Value::NativeFunc(
      |_env, args| {
        let list = require_list_parameter("cdr", args, 0)?;

        return Ok(match list {
          Value::List(c) => match &c.cdr {
            Some(c) => Value::List(c.clone()),
            None => Value::Nil,
          },
          Value::Nil => Value::Nil,
          _ => panic!("Argument validation didn't work properly"),
        });
      }));
    
  entries.insert(
    String::from("cons"),
    Value::NativeFunc(
      |_env, args| {
        let car = require_parameter("cons", args, 0)?;
        let cdr = require_list_parameter("cons", args, 1)?;

        return Ok(Value::List(Rc::new(ConsCell {
          car: car.clone(),
          cdr: match cdr {
            Value::List(c) => Some(c.clone()),
            Value::Nil => None,
            _ => panic!("Argument validation didn't work properly"),
          }
        })));
      }));
    
  entries.insert(
    String::from("list"),
    Value::NativeFunc(
      |_env, args| Ok(vec_to_cons(args))));
  
  entries.insert(
    String::from("nth"),
    Value::NativeFunc(
      |_env, args| {
        let index = require_int_parameter("nth", args, 0)?;
        let list = require_list_parameter("nth", args, 1)?;

        return Ok(match list {
          Value::List(cons) => match cons.into_iter().nth(index as usize) {
            Some(v) => v.clone(),
            None => Value::Nil
          },
          Value::Nil => Value::Nil,
          _ => panic!("Argument validation didn't work properly"),
        });
      }));

  entries.insert(
    String::from("sort"),
    Value::NativeFunc(
      |_env, args| {
        let list = require_list_parameter("sort", args, 0)?;

        return Ok(match list {
          Value::List(cons) => {
            let mut v: Vec<&Value> = cons.into_iter().collect();

            v.sort();

            vec_refs_to_cons(&v)
          },
          Value::Nil => Value::Nil,
          _ => panic!("Argument validation didn't work properly"),
        });
      }));
    

  entries.insert(
    String::from("reverse"),
    Value::NativeFunc(
      |_env, args| {
        let list = require_list_parameter("reverse", args, 0)?;

        return Ok(match list {
          Value::List(cons) => {
            let mut v: Vec<&Value> = cons.into_iter().collect();

            v.reverse();

            vec_refs_to_cons(&v)
          },
          Value::Nil => Value::Nil,
          _ => panic!("Argument validation didn't work properly"),
        });
      }));
  
  entries.insert(
    String::from("map"),
    Value::NativeFunc(
      |env, args| {
        let func = require_parameter("map", args, 0)?;
        let list = require_list_parameter("map", args, 1)?;

        Ok(match list {
          Value::List(cons) => {
            let mut result_vec = vec![];

            for v in cons.into_iter() {
              let expr = Value::List(Rc::new(ConsCell {
                car: func.clone(),
                cdr: Some(Rc::new(ConsCell {
                  car: v.clone(),
                  cdr: None
                }))
              }));

              result_vec.push(eval(env.clone(), &expr)?);
            }

            vec_to_cons(&result_vec)
          },
          Value::Nil => list.clone(),
          _ => panic!("Argument validation didn't work properly"),
        })
      }));

    
  entries.insert(
    String::from("filter"),
    Value::NativeFunc(
      |env, args| {
        let func = require_parameter("filter", args, 0)?;
        let list = require_list_parameter("filter", args, 1)?;

        Ok(match list {
          Value::List(cons) => {
            let mut result_vec = vec![];

            for v in cons.into_iter() {
              let expr = Value::List(Rc::new(ConsCell {
                car: func.clone(),
                cdr: Some(Rc::new(ConsCell {
                  car: v.clone(),
                  cdr: None
                }))
              }));

              if eval(env.clone(), &expr)?.is_truthy() {
                result_vec.push(v.clone());
              }
            }

            vec_to_cons(&result_vec)
          },
          Value::Nil => list.clone(),
          _ => panic!("Argument validation didn't work properly"),
        })
      }));

  entries.insert(
    String::from("length"),
    Value::NativeFunc(
      |_env, args| {
        let list = require_list_parameter("length", args, 0)?;

        return match list {
          Value::List(cons) => Ok(Value::Int(cons.into_iter().len() as i32)),
          Value::Nil => Ok(Value::Int(0)),
          _ => panic!("Argument validation didn't work properly"),
        };
      }));

  entries.insert(
    String::from("range"),
    Value::NativeFunc(
      |_env, args| {
        let start = require_int_parameter("range", args, 0)?;
        let end = require_int_parameter("range", args, 1)?;

        Ok(vec_to_cons(&(start..end).map(|i| Value::Int(i)).collect()))
      }));
    
  entries.insert(
    String::from("+"), 
    Value::NativeFunc(
      |_env, args| {
        let a = require_parameter("+", args, 0)?;
        let b = require_parameter("+", args, 1)?;

        match (a.as_int(), b.as_int()) {
          (Some(a), Some(b)) => return Ok(Value::Int(a + b)),
          _ => ()
        };

        match (a.as_float(), b.as_float()) {
          (Some(a), Some(b)) => return Ok(Value::Float(a + b)),
          _ => ()
        };

        match (a.as_string(), b.as_string()) {
          (Some(a), Some(b)) => return Ok(Value::String(String::from(a) + b)),
          _ => ()
        };

        return Err(RuntimeError { msg: String::from("Function \"+\" requires arguments to be numbers or strings") });
      }));
    
  entries.insert(
    String::from("-"), 
    Value::NativeFunc(
      |_env, args| {
        let a = require_parameter("-", args, 0)?;
        let b = require_parameter("-", args, 1)?;

        match (a.as_int(), b.as_int()) {
          (Some(a), Some(b)) => return Ok(Value::Int(a - b)),
          _ => ()
        };

        match (a.as_float(), b.as_float()) {
          (Some(a), Some(b)) => return Ok(Value::Float(a - b)),
          _ => ()
        };

        return Err(RuntimeError { msg: String::from("Function \"-\" requires arguments to be numbers") });
      }));
    
  entries.insert(
    String::from("*"), 
    Value::NativeFunc(
      |_env, args| {
        let a = require_parameter("*", args, 0)?;
        let b = require_parameter("*", args, 1)?;

        match (a.as_int(), b.as_int()) {
          (Some(a), Some(b)) => return Ok(Value::Int(a * b)),
          _ => ()
        };

        match (a.as_float(), b.as_float()) {
          (Some(a), Some(b)) => return Ok(Value::Float(a * b)),
          _ => ()
        };

        return Err(RuntimeError { msg: String::from("Function \"*\" requires arguments to be numbers") });
      }));

  entries.insert(
    String::from("/"), 
    Value::NativeFunc(
      |_env, args| {
        let a = require_parameter("/", args, 0)?;
        let b = require_parameter("/", args, 1)?;

        match (a.as_int(), b.as_int()) {
          (Some(a), Some(b)) => return Ok(Value::Int(a / b)),
          _ => ()
        };

        match (a.as_float(), b.as_float()) {
          (Some(a), Some(b)) => return Ok(Value::Float(a / b)),
          _ => ()
        };

        return Err(RuntimeError { msg: String::from("Function \"/\" requires arguments to be numbers") });
      }));

  entries.insert(
    String::from("truncate"),
    Value::NativeFunc(
      |_env, args| {
        let a = require_parameter("truncate", args, 0)?;
        let b = require_parameter("truncate", args, 1)?;

        match (a.as_int(), b.as_int()) {
          (Some(a), Some(b)) => return Ok(Value::Int(a / b)),
          _ => ()
        };

        return Err(RuntimeError { msg: String::from("Function \"truncate\" requires arguments to be integers") });
      }));
    
  entries.insert(
    String::from("not"), 
    Value::NativeFunc(
      |_env, args| {
        let a = require_parameter("not", args, 0)?;

        Ok(Value::from_truth(!a.is_truthy()))
      }));

  entries.insert(
    String::from("=="), 
    Value::NativeFunc(
      |_env, args| {
        let a = require_parameter("==", args, 0)?;
        let b = require_parameter("==", args, 1)?;

        Ok(Value::from_truth(a == b))
      }));

  entries.insert(
    String::from("!="), 
    Value::NativeFunc(
      |_env, args| {
        let a = require_parameter("!=", args, 0)?;
        let b = require_parameter("!=", args, 1)?;

        Ok(Value::from_truth(a != b))
      }));

  entries.insert(
    String::from("<"), 
    Value::NativeFunc(
      |_env, args| {
        let a = require_parameter("<", args, 0)?;
        let b = require_parameter("<", args, 1)?;

        Ok(Value::from_truth(a < b))
      }));

  entries.insert(
    String::from("<="), 
    Value::NativeFunc(
      |_env, args| {
        let a = require_parameter("<=", args, 0)?;
        let b = require_parameter("<=", args, 1)?;

        Ok(Value::from_truth(a <= b))
      }));

  entries.insert(
    String::from(">"), 
    Value::NativeFunc(
      |_env, args| {
        let a = require_parameter(">", args, 0)?;
        let b = require_parameter(">", args, 1)?;

        Ok(Value::from_truth(a > b))
      }));

  entries.insert(
    String::from(">="), 
    Value::NativeFunc(
      |_env, args| {
        let a = require_parameter(">=", args, 0)?;
        let b = require_parameter(">=", args, 1)?;

        Ok(Value::from_truth(a >= b))
      }));

  entries.insert(
    String::from("eval"), 
    Value::NativeFunc(
      |env, args| {
        let expr = require_parameter("eval", args, 0)?;

        eval(env, expr)
      }));

  entries.insert(
    String::from("apply"),
    Value::NativeFunc(
      |env, args| {
        let func = require_parameter("apply", args, 0)?;
        let params = require_list_parameter("apply", args, 1)?;

        eval(env.clone(), &Value::List(Rc::new(ConsCell {
          car: func.clone(),
          cdr: match params {
            Value::List(cons) => Some(cons.clone()),
            Value::Nil => None,
            _ => panic!("Argument validation didn't work properly"),
          }
        })))
      }));
    
  Env {
    parent: None,
    entries
  }
}