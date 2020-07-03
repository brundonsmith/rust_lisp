
use std::{rc::Rc, collections::HashMap};
use crate::{utils::vec_to_cons, model::{Value, Env, RuntimeError, ConsCell}};

pub fn default_env() -> Env {
  let mut entries = HashMap::new();

  entries.insert(
    String::from("print"),
    Value::NativeFunc(
      |_env, args| {
        let expr = args.get(0).unwrap();

        println!("{}", &expr);
        return Ok(expr.clone());
      }));

  entries.insert(
    String::from("null"),
    Value::NativeFunc(
      |_env, args| {
        Ok(Value::from_truth(*args.get(0).unwrap() == Value::Nil))
      }));

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
          Some(car) =>
            Ok(Value::List(Rc::new(ConsCell {
              car: car.clone(),
              cdr: cdr.map(|c| c.clone())
            }))),
          None => Err(RuntimeError { msg: String::from("Requires 2 args") })
        };
      }));
    
  entries.insert(
    String::from("list"),
    Value::NativeFunc(
      |_env, args| Ok(vec_to_cons(args))));

  entries.insert(
    String::from("length"),
    Value::NativeFunc(
      |_env, args| {
        let list = &args[0];

        return match list {
          Value::List(cons) => Ok(Value::Int(cons.into_iter().len() as i32)),
          Value::Nil => Ok(Value::Int(0)),
          _ => Err(RuntimeError { msg: format!("Can't take length of {}", list) }),
        };
      }));

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

        let a = args.get(0).and_then(|a| a.as_string());
        let b = args.get(1).and_then(|a| a.as_string());

        if a.is_some() && b.is_some() {
          return Ok(Value::String(a.unwrap().to_owned() + b.unwrap()));
        }

        return Err(RuntimeError { msg: String::from("Args must be numbers or strings") });
      }));
    
  entries.insert(
    String::from("-"), 
    Value::NativeFunc(
      |_env, args| {
        let a = args.get(0).and_then(|a| a.as_int());
        let b = args.get(1).and_then(|a| a.as_int());

        if a.is_some() && b.is_some() {
          return Ok(Value::Int(a.unwrap() - b.unwrap()));
        }

        let a = args.get(0).and_then(|a| a.as_float());
        let b = args.get(1).and_then(|a| a.as_float());

        if a.is_some() && b.is_some() {
          return Ok(Value::Float(a.unwrap() - b.unwrap()));
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

  entries.insert(
    String::from("truncate"),
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
    
  entries.insert(
    String::from("and"), 
    Value::NativeFunc(
      |_env, args| {
        Ok(Value::from_truth(args.get(0).unwrap().is_truthy() && args.get(1).unwrap().is_truthy()))
      }));

  entries.insert(
    String::from("or"), 
    Value::NativeFunc(
      |_env, args| {
        Ok(Value::from_truth(args.get(0).unwrap().is_truthy() || args.get(1).unwrap().is_truthy()))
      }));
    
  entries.insert(
    String::from("not"), 
    Value::NativeFunc(
      |_env, args| {
        Ok(Value::from_truth(!args.get(0).unwrap().is_truthy()))
      }));

  entries.insert(
    String::from("=="), 
    Value::NativeFunc(
      |_env, args| {
        Ok(Value::from_truth(args.get(0) == args.get(1)))
      }));

  entries.insert(
    String::from("!="), 
    Value::NativeFunc(
      |_env, args| {
        Ok(Value::from_truth(args.get(0) != args.get(1)))
      }));

  entries.insert(
    String::from("<"), 
    Value::NativeFunc(
      |_env, args| {
        Ok(Value::from_truth(args.get(0) < args.get(1)))
      }));
  entries.insert(
    String::from("<="), 
    Value::NativeFunc(
      |_env, args| {
        Ok(Value::from_truth(args.get(0) <= args.get(1)))
      }));
  entries.insert(
    String::from(">"), 
    Value::NativeFunc(
      |_env, args| {
        Ok(Value::from_truth(args.get(0) > args.get(1)))
      }));
  entries.insert(
    String::from(">="), 
    Value::NativeFunc(
      |_env, args| {
        Ok(Value::from_truth(args.get(0) >= args.get(1)))
      }));

  Env {
    parent: None,
    entries
  }
}