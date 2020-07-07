
use std::{rc::Rc, collections::HashMap};
use crate::{utils::{require_list_parameter, vec_to_cons, require_parameter}, model::{Value, Env, RuntimeError, ConsCell}};

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
    String::from("null"),
    Value::NativeFunc(
      |_env, args| {
        let val = require_parameter("null", args, 0)?;

        Ok(Value::from_truth(*val == Value::Nil))
      }));

  entries.insert(
    String::from("car"),
    Value::NativeFunc(
      |_env, args| {
        let list = require_list_parameter("car", args, 0)?;

        return Ok(match list {
          Some(c) => c.car.clone(),
          None => Value::Nil
        });
      }));
    
  entries.insert(
    String::from("cdr"),
    Value::NativeFunc(
      |_env, args| {
        let list = require_list_parameter("cdr", args, 0)?;

        return match list.as_ref().map(|l| l.cdr.clone()) {
          Some(cell) => match cell {
            Some(c) => Ok(Value::List(c.clone())),
            None => Ok(Value::Nil),
          },
          None => Ok(Value::Nil)
        };
      }));
    
  entries.insert(
    String::from("cons"),
    Value::NativeFunc(
      |_env, args| {
        let car = require_parameter("cons", args, 0)?;
        let cdr = require_list_parameter("cons", args, 1)?;

        return Ok(Value::List(Rc::new(ConsCell {
          car: car.clone(),
          cdr: cdr.map(|c| c.clone())
        })));
      }));
    
  entries.insert(
    String::from("list"),
    Value::NativeFunc(
      |_env, args| Ok(vec_to_cons(args))));

  entries.insert(
    String::from("length"),
    Value::NativeFunc(
      |_env, args| {
        let list = require_list_parameter("length", args, 0)?;

        return match list {
          Some(cons) => Ok(Value::Int(cons.into_iter().len() as i32)),
          None => Ok(Value::Int(0)),
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