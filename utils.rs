
use crate::model::{ConsCell, Value, RuntimeError};
use std::rc::Rc;

// pub struct ArgumentError {
//   msg: String,
//   index: usize,
// }

pub fn require_parameter<'a>(func_name: &str, args: &'a Vec<Value>, index: usize) -> Result<&'a Value,RuntimeError> {
  match args.get(index) {
    Some(val) => Ok(val),
    None => Err(RuntimeError {
      msg: format!("Function \"{}\" requires an argument {}", func_name, index + 1),
    })
  }
}

pub fn require_int_parameter(func_name: &str, args: &Vec<Value>, index: usize) -> Result<i32,RuntimeError> {
  match require_parameter(func_name, args, index) {
    Ok(val) => match val.as_int() {
      Some(x) => Ok(x),
      None => Err(RuntimeError {
        msg: format!("Function \"{}\" requires argument {} to be an integer", func_name, index),
      })
    },
    Err(err) => Err(err)
  }
}

pub fn require_float_parameter(func_name: &str, args: &Vec<Value>, index: usize) -> Result<f32,RuntimeError> {
  match require_parameter(func_name, args, index) {
    Ok(val) => match val.as_float() {
      Some(x) => Ok(x),
      None => Err(RuntimeError {
        msg: format!("Function \"{}\" requires argument {} to be a float", func_name, index),
      })
    },
    Err(err) => Err(err)
  }
}

pub fn require_string_parameter<'a>(func_name: &str, args: &'a Vec<Value>, index: usize) -> Result<&'a str,RuntimeError> {
  match require_parameter(func_name, args, index) {
    Ok(val) => match val.as_string() {
      Some(x) => Ok(x),
      None => Err(RuntimeError {
        msg: format!("Function \"{}\" requires argument {} to be a string", func_name, index),
      })
    },
    Err(err) => Err(err)
  }
}

pub fn require_list_parameter(func_name: &str, args: &Vec<Value>, index: usize) -> Result<Option<Rc<ConsCell>>,RuntimeError> {
  match require_parameter(func_name, args, index) {
    Ok(val) => match val {
      Value::List(x) => Ok(Some(x.clone())),
      Value::Nil => Ok(None),
      _ => Err(RuntimeError {
        msg: format!("Function \"{}\" requires argument {} to be a list", func_name, index),
      })
    },
    Err(err) => Err(err)
  }
}


pub fn vec_to_cons(vec: &Vec<Value>) -> Value {
  let mut cons: Option<ConsCell> = None;

  for val in vec.iter().rev() {          
    cons = Some(ConsCell {
      car: val.clone(),
      cdr: cons.map(|cons_cell| Rc::new(cons_cell)),
    });
  }

  return match cons {
    Some(cons) => Value::List(Rc::new(cons)),
    None => Value::Nil,
  };
}