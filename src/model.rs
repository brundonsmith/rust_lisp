use std::rc::Rc;
use std::{fmt::{Debug, Display}, collections::HashMap};
use std::{cell::{Ref, RefCell}, borrow::Borrow, cmp::Ordering};

#[derive(Clone)]
pub enum Value {
  Nil,
  True,
  False,
  Int(i32),
  Float(f32),
  String(String),
  Symbol(String),
  List(Rc<ConsCell>),
  NativeFunc(NativeFunc),
  Lambda(Lambda),
  TailCall(Rc<Value>),
}

impl Value {

  pub fn type_name(&self) -> &str {
    match self {
      Value::NativeFunc(_) => "function",
      Value::Lambda(_) => "function",
      Value::Nil => "()",
      Value::True => "#t",
      Value::False => "#f",
      Value::String(_) => "string",
      Value::List(_) => "list",
      Value::Int(_) => "integer",
      Value::Float(_) => "float",
      Value::Symbol(_) => "symbol",
      Value::TailCall(_) => "tail call",
    }
  }

  pub fn from_truth(b: bool) -> Value {
    match b {
      true => Value::True,
      false => Value::False,
    }
  }

  pub fn is_truthy(&self) -> bool {
    match self {
      Value::Nil => false,
      Value::False => false,
      _ => true,
    }
  }

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

  pub fn as_string(&self) -> Option<&str> {
    match self {
      Value::String(n) => Some(n),
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

impl Display for Value {
  fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
    match self {
      Value::NativeFunc(_) => write!(formatter, "<native_function>"),
      Value::Nil => write!(formatter, "NIL"),
      Value::True => write!(formatter, "#t"),
      Value::False => write!(formatter, "#f"),
      Value::Lambda(n) => {
        let body_str = format!("{}", &n.body);
        return write!(formatter, "<func:(lambda {} {})>", n.argnames, &body_str[1..body_str.chars().count() - 1]);
      },
      Value::String(n) => write!(formatter, "\"{}\"", n),
      Value::List(n) => write!(formatter, "({})", n),
      Value::Int(n) => write!(formatter, "{}", n),
      Value::Float(n) => write!(formatter, "{}", n),
      Value::Symbol(n) => write!(formatter, "{}", n),
      Value::TailCall(n) => write!(formatter, "<tail-call:{}>", n),
    }
  }
}

impl Debug for Value {
  fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
    match self {
      Value::NativeFunc(_) => write!(formatter, "<native_function>"),
      Value::Nil => write!(formatter, "Value::Nil"),
      Value::True => write!(formatter, "Value::True"),
      Value::False => write!(formatter, "Value::False"),
      Value::Lambda(n) => write!(formatter, "Value::Lambda({:?})", n),
      Value::String(n) => write!(formatter, "Value::String({:?})", n),
      Value::List(n) => write!(formatter, "Value::List({:?})", n),
      Value::Int(n) => write!(formatter, "Value::Int({:?})", n),
      Value::Float(n) => write!(formatter, "Value::Float({:?})", n),
      Value::Symbol(n) => write!(formatter, "Value::Symbol({:?})", n),
      Value::TailCall(n) => write!(formatter, "Value::TailCall({:?})", n),
    }
  }
}

impl PartialEq for Value {
  fn eq(&self, other: &Self) -> bool {
    match self {
      Value::NativeFunc(_) => false,
      Value::Nil =>  match *other { Value::Nil => true, _ => false },
      Value::True => match *other { Value::True => true, _ => false },
      Value::False => match *other { Value::False => true, _ => false },
      Value::Lambda(n) =>      match other { Value::Lambda(o) =>      n == o, _ => false },
      Value::String(n) =>      match other { Value::String(o) =>      n == o, _ => false },
      Value::List(n) =>  match other { Value::List(o) =>  n == o, _ => false },
      Value::Int(n) =>            match other { Value::Int(o) =>            n == o, _ => false },
      Value::Float(n) =>          match other { Value::Float(o) =>          n == o, _ => false },
      Value::Symbol(n) =>      match other { Value::Symbol(o) =>      n == o, _ => false },
      Value::TailCall(n) => match other { Value::TailCall(o) => n == o, _ => false },
    }
  }
}


impl PartialOrd for Value {
  fn partial_cmp(&self, other: &Value) -> Option<Ordering> {
    match self {
      Value::Nil => {
        if other.is_truthy() {
          return Some(Ordering::Less);
        } else {
          return Some(Ordering::Equal);
        }
      },
      Value::True => {
        if other.is_truthy() {
          return Some(Ordering::Equal);
        } else {
          return Some(Ordering::Greater);
        }
      },
      Value::False => {
        if !other.is_truthy() {
          return Some(Ordering::Equal);
        } else {
          return Some(Ordering::Greater);
        }
      },
      Value::String(n) => if let Value::String(s) = other { n.partial_cmp(s) } else { None },
      Value::Symbol(n) => if let Value::Symbol(s) = other { n.partial_cmp(s) } else { None },
      Value::Int(n) => match other {
        Value::Int(o) => n.partial_cmp(o),
        Value::Float(o) => n.partial_cmp(&(o.round() as i32)),
        _ => None
      },
      Value::Float(n) => match other {
        Value::Int(o) => n.partial_cmp(&(*o as f32)),
        Value::Float(o) => n.partial_cmp(o),
        _ => None
      },
      Value::NativeFunc(_) => None,
      Value::Lambda(_) => None,
      Value::List(_) => None,
      Value::TailCall(_) => None,
    }
  }
}

#[derive(Debug,PartialEq)]
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

impl<'a> ExactSizeIterator for ConsIterator<'a> {
  fn len(&self) -> usize {
    let mut cons = self.0;
    let mut length = 0;

    while cons.is_some() {
      cons = cons.unwrap().cdr.as_ref().map(|rc: &Rc<ConsCell>| &*rc.borrow());
      length += 1;
    }

    return length;
  }
}

#[derive(Debug,Clone)]
pub struct Lambda {
  pub env: Rc<RefCell<Env>>,
  pub argnames: Rc<Value>,
  pub body: Rc<Value>
}

impl PartialEq for Lambda {
  fn eq(&self, other: &Self) -> bool {
    false // TODO
  }
}

type NativeFunc = fn(Rc<RefCell<Env>>, &Vec<Value>) -> Result<Value, RuntimeError>;


pub struct RuntimeError {
  pub msg: String,
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