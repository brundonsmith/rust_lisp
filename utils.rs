
use crate::model::{ConsCell, Value};
use std::rc::Rc;

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