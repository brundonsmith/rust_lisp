
use crate::model::{Expression, Env, Func, Number, Atom, RuntimeError, float, int, func, NIL};
use std::{rc::Rc, collections::HashMap};

pub fn standard_env() -> Env {
  let mut entries = HashMap::new();

  entries.insert(
    String::from("+"), 
    Rc::new(func(
      |_env, expressions| {
        let a = expressions.get(0).and_then(|a| a.as_int());
        let b = expressions.get(1).and_then(|a| a.as_int());

        if a.is_some() && b.is_some() {
          return Ok(int(a.unwrap() + b.unwrap()));
        }

        let a = expressions.get(0).and_then(|a| a.as_float());
        let b = expressions.get(1).and_then(|a| a.as_float());

        if a.is_some() && b.is_some() {
          return Ok(float(a.unwrap() + b.unwrap()));
        }

        return Err(RuntimeError { msg: String::from("Args must be numbers") });
      })));

    
  entries.insert(
    String::from("*"), 
    Rc::new(func(
      |_env, expressions| {
        let a = expressions.get(0).and_then(|a| a.as_int());
        let b = expressions.get(1).and_then(|a| a.as_int());

        if a.is_some() && b.is_some() {
          return Ok(int(a.unwrap() * b.unwrap()));
        }

        let a = expressions.get(0).and_then(|a| a.as_float());
        let b = expressions.get(1).and_then(|a| a.as_float());

        if a.is_some() && b.is_some() {
          return Ok(float(a.unwrap() * b.unwrap()));
        }

        return Err(RuntimeError { msg: String::from("Args must be numbers") });
      })));

    
  entries.insert(
    String::from("/"), 
    Rc::new(func(
      |_env, expressions| {
        let a = expressions.get(0).and_then(|a| a.as_int());
        let b = expressions.get(1).and_then(|a| a.as_int());

        if a.is_some() && b.is_some() {
          return Ok(int(a.unwrap() / b.unwrap()));
        }

        let a = expressions.get(0).and_then(|a| a.as_float());
        let b = expressions.get(1).and_then(|a| a.as_float());

        if a.is_some() && b.is_some() {
          return Ok(float(a.unwrap() / b.unwrap()));
        }

        return Err(RuntimeError { msg: String::from("Args must be numbers") });
      })));

  Env {
    parent: None,
    entries
  }
}

pub fn eval(env: &mut Env, expression: Rc<Expression>) -> Rc<Expression> {
  match expression.as_ref() {
    Expression::Atom(atom) => match atom {
      Atom::Symbol(symbol) => match env.find(&symbol) {
        Some(expr) => expr,
        None => panic!(format!("\"{}\" is not defined", symbol))
      },
      Atom::Number(_) => expression,
      Atom::String(_) => expression,
      Atom::Nil => expression,
    },
    Expression::List(list) => {
      let func = eval(env, list.car.clone());

      let mut args = vec![];
      {
        let mut cons = list.cdr.as_ref().map(|c| c.clone());
        while cons.is_some() {
          let c = cons.as_ref().unwrap();
          args.push(eval(env, c.car.clone()));
          cons = c.cdr.clone();
        }
      }

      let result = match func.as_ref() {
        Expression::Func(func) => match func {
          Func::Native(func) => func(env, args.as_ref()),
          Func::Lambda(_) => Err(RuntimeError { msg: String::from("Argument 0 is not callable") })
        }
        _ => unimplemented!()
      };

      match result {
        Ok(expr) => Rc::new(expr),
        Err(e) => panic!(e.msg),
      }
    },
    Expression::Func(func) => expression
  }
}
