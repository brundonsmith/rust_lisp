
use crate::model::{Value, Env, Func, RuntimeError};
use std::{collections::HashMap};

pub fn standard_env() -> Env {
  let mut entries = HashMap::new();

  entries.insert(
    String::from("car"),
    Value::Func(Func::Native(
      |_env, args| {
        let list = args.get(0).and_then(|a| a.as_list());

        return match list {
          Some(cons_cell) => Ok(cons_cell.car.clone()),
          None => Err(RuntimeError { msg: String::from("Arg must be list") })
        };
      })));
    
  entries.insert(
    String::from("cdr"),
    Value::Func(Func::Native(
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
      })));

  entries.insert(
    String::from("+"), 
    Value::Func(Func::Native(
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
      })));

    
  entries.insert(
    String::from("*"), 
    Value::Func(Func::Native(
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
      })));

    
  entries.insert(
    String::from("/"), 
    Value::Func(Func::Native(
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
      })));

  Env {
    parent: None,
    entries
  }
}

pub fn eval(env: &mut Env, expression: &Value) -> Value {
  match expression {
    Value::Symbol(symbol) => match env.find(&symbol) {
      Some(expr) => expr.clone(),
      None => panic!(format!("\"{}\" is not defined", symbol))
    },
    Value::List(list) => {
      let func = eval(env, &list.car);

      let mut args = vec![];
      {
        let mut cons = list.cdr.as_ref().map(|c| c.clone());
        while cons.is_some() {
          let c = cons.as_ref().unwrap();
          args.push(eval(env, &c.car));
          cons = c.cdr.clone();
        }
      }

      let result = match func {
        Value::Func(func) => match func {
          Func::Native(func) => func(env, args.as_ref()),
          // Func::Lambda(_) => Err(RuntimeError { msg: String::from("Argument 0 is not callable") })
        }
        _ => unimplemented!()
      };

      match result {
        Ok(expr) => expr,
        Err(e) => panic!(e.msg),
      }
    },
    _ => expression.clone(),
  }
}
