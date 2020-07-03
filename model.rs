use std::rc::Rc;
use std::{fmt::{Debug, Display}, collections::HashMap};
use std::{cell::RefCell, borrow::Borrow};

#[derive(Clone)]
pub enum Value {
  Nil,
  True,
  Int(i32),
  Float(f32),
  String(String),
  Symbol(String),
  List(Rc<ConsCell>),
  NativeFunc(NativeFunc),
  Lambda(Lambda),
}

impl Display for Value {
  fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
    match self {
      Value::NativeFunc(_) => write!(formatter, "<native_function>"),
      Value::Nil => write!(formatter, "NIL"),
      Value::True => write!(formatter, "T"),
      Value::Lambda(n) => {
        let body_str = format!("{}", &n.body);
        return write!(formatter, "<func:(lambda {} {})>", n.argnames, &body_str[1..body_str.chars().count() - 1]);
      },
      Value::String(n) => write!(formatter, "\"{}\"", n),
      Value::List(n) => write!(formatter, "({})", n),
      Value::Int(n) => write!(formatter, "{}", n),
      Value::Float(n) => write!(formatter, "{}", n),
      Value::Symbol(n) => write!(formatter, "{}", n),
    }
  }
}

impl Debug for Value {
  fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
    match self {
      Value::NativeFunc(_) => write!(formatter, "<native_function>"),
      Value::Nil => write!(formatter, "Value::Nil"),
      Value::True => write!(formatter, "Value::True"),
      Value::Lambda(n) => write!(formatter, "Value::Lambda({:?})", n),
      Value::String(n) => write!(formatter, "Value::String({:?})", n),
      Value::List(n) => write!(formatter, "Value::List({:?})", n),
      Value::Int(n) => write!(formatter, "Value::Int({:?})", n),
      Value::Float(n) => write!(formatter, "Value::Float({:?})", n),
      Value::Symbol(n) => write!(formatter, "Value::Symbol({:?})", n),
    }
  }
}


#[derive(Debug)]
pub struct ConsCell {
  pub car: Value,
  pub cdr: Option<Rc<ConsCell>>,
}

impl<'a> ConsCell {
  pub fn into_iter(cell: &'a ConsCell) -> ConsIterator<'a> {
    ConsIterator(Some(cell))
  }
}

impl Display for ConsCell {
  fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
    match self.cdr.as_ref() {
      Some(cdr) => write!(formatter, "{} {}", self.car, cdr),
      None => write!(formatter, "{}", self.car)
    }
  }
}

impl<'a> IntoIterator for &'a ConsCell {
  type Item = &'a Value;
  type IntoIter = ConsIterator<'a>;

  fn into_iter(self) -> Self::IntoIter {
    ConsIterator(Some(self))
  }
}

pub struct ConsIterator<'a>(Option<&'a ConsCell>);

impl<'a> Iterator for ConsIterator<'a> {
  type Item = &'a Value;

  fn next(&mut self) -> Option<Self::Item> {
    self.0.map(|cons| {
      let val = &cons.car;

      self.0 = cons.cdr.as_ref().map(|rc: &Rc<ConsCell>| &*rc.borrow());

      return val;
    })
  }
}

#[derive(Debug,Clone)]
pub struct Lambda {
  pub env: Rc<RefCell<Env>>,
  pub argnames: Rc<Value>,
  pub body: Rc<Value>
}


type NativeFunc = fn(Rc<RefCell<Env>>, &Vec<Value>) -> Result<Value, RuntimeError>;


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

  pub fn as_lambda(&self) -> Option<Lambda> {
    match self {
      Value::Lambda(lambda) => Some(lambda.clone()),
      _ => None
    }
  }

  pub fn as_symbol(&self) -> Option<String> {
    match self {
      Value::Symbol(name) => Some(name.clone()),
      _ => None
    }
  }
}


#[derive(Debug)]
pub struct Env {
  pub parent: Option<Rc<RefCell<Env>>>,
  pub entries: HashMap<String,Value>,
}

impl Env {
  pub fn find(&self, symbol: &str) -> Option<Value> {
    if self.entries.contains_key(symbol) {
      return self.entries.get(symbol).map(|v| v.clone()); // clone the Rc
    } else if self.parent.is_some() {
      return self.parent.as_ref().unwrap().borrow_mut().find(symbol);
    } else {
      return None;
    }
  }
}

impl Display for Env {
  fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
    let mut output = String::new();
    
    output.push_str("Env: ");
    display_one_env_level(self, &mut output, 0);

    return write!(formatter, "{}", &output);
  }
}

fn display_one_env_level(env: &Env, output: &mut String, depth: i32) {
  let indent = &(0..depth).map(|_| "  ").collect::<String>();

  output.push_str(indent);
  output.push_str("{ ");

  for (symbol, value) in &env.entries {
    output.push_str(format!("\n{}  {}: {}", indent, symbol, value).as_str());
  }

  match &env.parent {
    Some(parent) => {
      output.push_str("\n\n");
      display_one_env_level(&parent.as_ref().borrow(), output, depth + 1);
    },
    None => (),
  }

  output.push_str("\n");
  output.push_str(indent);
  output.push_str("}");
}