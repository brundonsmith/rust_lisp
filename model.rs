use std::rc::Rc;
use std::{fmt::Display, collections::HashMap};

#[derive(Debug,Clone)]
pub enum Value {
  Nil,
  True,
  Int(i32),
  Float(f32),
  String(String),
  Symbol(String),
  List(Rc<ConsCell>),
  Func(Func),
}

impl Display for Value {
  fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
    match self {
      Value::Nil => write!(formatter, "NIL"),
      Value::True => write!(formatter, "T"),
      Value::Func(_n) => write!(formatter, "<function>"),
      Value::String(n) => write!(formatter, "\"{}\"", n),
      Value::List(n) => write!(formatter, "({})", n),
      Value::Int(n) => write!(formatter, "{}", n),
      Value::Float(n) => write!(formatter, "{}", n),
      Value::Symbol(n) => write!(formatter, "{}", n),
    }
  }
}


#[derive(Debug)]
pub struct ConsCell {
  pub car: Value,
  pub cdr: Option<Rc<ConsCell>>,
}

impl Display for ConsCell {
  fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
    match self.cdr.as_ref() {
      Some(cdr) => write!(formatter, "{} {}", self.car, cdr),
      None => write!(formatter, "{}", self.car)
    }
  }
}


#[derive(Clone)]
pub enum Func {
  // Lambda(Box<Lambda>),
  Native(NativeFunc),
}

#[derive(Debug)]
pub struct Lambda {
  pub env: Rc<Box<Env>>,
  pub argnames: Vec<String>,
  pub body: Value
}

type NativeFunc = fn(&mut Env, &Vec<Value>) -> Result<Value, RuntimeError>;


impl std::fmt::Debug for Func {
  fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
    write!(formatter, "<function>")
  }
}

pub struct RuntimeError {
  pub msg: String,
}

impl Value {

  pub fn as_int(&self) -> Option<i32> {
    match self {
      Value::Int(n) => Some(*n),
      _ => None
    }
  }

  pub fn as_float(&self) -> Option<f32> {
    match self {
      Value::Float(n) => Some(*n),
      _ => None
    }
  }

  pub fn as_list(&self) -> Option<Rc<ConsCell>> {
    match self {
      Value::List(list) => Some(list.clone()),
      _ => None,
    }
  }
}


#[derive(Debug)]
pub struct Env {
  pub entries: HashMap<String,Value>,
  pub parent: Option<Rc<Env>>
}

impl Env {
  pub fn find(&self, symbol: &str) -> Option<Value> {
    if self.entries.contains_key(symbol) {
      return self.entries.get(symbol).map(|v| v.clone()); // clone the Rc
    } else if self.parent.is_some() {
      return self.parent.as_ref().unwrap().find(symbol);
    } else {
      return None;
    }
  }
}
