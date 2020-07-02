use std::rc::Rc;
use std::{fmt::Display, collections::HashMap};

// types
#[derive(Debug,PartialEq,Clone)]
pub enum Number {
  Int(i32),
  Float(f32),
}

impl Display for Number {
  fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
    match self {
      Number::Int(n) => write!(formatter, "{}", n),
      Number::Float(n) => write!(formatter, "{}", n),
    }
  }
}

#[derive(Debug,PartialEq,Clone)]
pub enum Atom {
  Number(Number),
  String(String),
  Symbol(String),
  Nil,
}

impl Display for Atom {
  fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
    match self {
      Atom::Number(n) => write!(formatter, "{}", n),
      Atom::String(n) => write!(formatter, "\"{}\"", n),
      Atom::Symbol(n) => write!(formatter, "{}", n),
      Atom::Nil => write!(formatter, "NIL"),
    }
  }
}

#[derive(Debug)]
pub struct ConsCell {
  pub car: Rc<Expression>,
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

pub enum Func {
  Lambda(Box<Lambda>),
  Native(NativeFunc),
}

pub struct RuntimeError {
  pub msg: String,
}

impl std::fmt::Debug for Func {
  fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
    write!(formatter, "<function>")
  }
}

impl std::cmp::PartialEq for Func {
  fn eq(&self, other: &Func) -> bool {
    false
    /*
    match self {
      Func::Native(f) => match other {
        Func::Native(other_f) => f == *other_f,
        Func::Lambda(_) => false
      },
      Func::Lambda(lam) => match other {
        Func::Native(_) => false,
        Func::Lambda(other_lam) => lam == other_lam,
      }
    }*/
  }
}


#[derive(Debug)]
pub enum Expression {
  Atom(Atom),
  List(Rc<ConsCell>),
  Func(Func)
}

impl Display for Expression {
  fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
    match self {
      Expression::Atom(n) => write!(formatter, "{}", n),
      Expression::List(n) => write!(formatter, "({})", n),
      Expression::Func(_) => write!(formatter, "<function>"),
    }
  }
}

impl Expression {

  pub fn as_int(&self) -> Option<i32> {
    match self {
      Expression::Atom(atom) => match atom {
        Atom::Number(num) => match num {
          Number::Int(val) => Some(*val),
          _ => None
        },
        _ => None
      },
      _ => None
    }
  }

  pub fn as_float(&self) -> Option<f32> {
    match self {
      Expression::Atom(atom) => match atom {
        Atom::Number(num) => match num {
          Number::Float(val) => Some(*val),
          _ => None
        },
        _ => None
      },
      _ => None
    }
  }

  pub fn as_func(&self) -> Option<&Func> {
    match self {
      Expression::Func(f) => Some(f),
      _ => None
    }
  }
}


// convenience
pub const NIL: Expression = Expression::Atom(Atom::Nil);
pub fn int(i: i32) -> Expression {
  Expression::Atom(Atom::Number(Number::Int(i)))
}
pub fn float(f: f32) -> Expression {
  Expression::Atom(Atom::Number(Number::Float(f)))
}
pub fn symbol(str: String) -> Expression {
  Expression::Atom(Atom::Symbol(str))
}
pub fn func(f: NativeFunc) -> Expression {
  Expression::Func(Func::Native(f))
}




#[derive(Debug)]
pub struct Env {
  pub entries: HashMap<String,Rc<Expression>>,
  pub parent: Option<Rc<Env>>
}

impl Env {
  pub fn find(&self, symbol: &str) -> Option<Rc<Expression>> {
    if self.entries.contains_key(symbol) {
      return self.entries.get(symbol).map(|v| v.clone()); // clone the Rc
    } else if self.parent.is_some() {
      return self.parent.as_ref().unwrap().find(symbol);
    } else {
      return None;
    }
  }
}


#[derive(Debug)]
pub struct Lambda {
  pub env: Rc<Box<Env>>,
  pub argnames: Vec<String>,
  pub body: Expression
}

type NativeFunc = fn(&mut Env, &Vec<Rc<Expression>>) -> Result<Expression, RuntimeError>;

/*
impl Fn<(Vec<Expression>,)> for Lambda {
  type Output = Expression;
  extern "rust-call" fn call(&self, args: (Vec<Expression>,)) -> Expression {
    eval(
      self.body, 
      Env { 
        entries: self.argnames.iter().zip(args.iter()).collect(), 
        parent: self.env 
      }
    )
  }
}
*/
