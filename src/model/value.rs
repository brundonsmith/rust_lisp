use crate::lisp;
use cfg_if::cfg_if;
use std::rc::Rc;
use std::{cell::RefCell, cmp::Ordering};
use std::{collections::HashMap, fmt::Debug};

cfg_if! {
    if #[cfg(feature = "bigint")] {
        use num_bigint::BigInt;
        use num_traits::ToPrimitive;
    }
}

use super::{Env, FloatType, IntType, Lambda, List, RuntimeError, Symbol};

/// `Value` encompasses all possible Lisp values, including atoms, lists, and
/// others.
#[derive(Clone)]
pub enum Value {
    True,
    False,
    Int(IntType),
    Float(FloatType),
    String(String),
    Symbol(Symbol),
    List(List),
    HashMap(Rc<RefCell<HashMap<Value, Value>>>),
    NativeFunc(NativeFunc),
    Lambda(Lambda),
    TailCall { func: Rc<Value>, args: Vec<Value> },
}

/// The trait bound for any Rust function that is to be called from lisp code
type NativeFunc = fn(env: Rc<RefCell<Env>>, args: &Vec<Value>) -> Result<Value, RuntimeError>;

impl Value {
    pub const NIL: Value = Value::List(List::NIL);

    pub fn type_name(&self) -> &str {
        match self {
            Value::NativeFunc(_) => "function",
            Value::Lambda(_) => "function",
            Value::True => "T",
            Value::False => "F",
            Value::String(_) => "string",
            Value::List(List::NIL) => "nil",
            Value::List(_) => "list",
            Value::HashMap(_) => "hash map",
            Value::Int(_) => "integer",
            Value::Float(_) => "float",
            Value::Symbol(_) => "symbol",
            Value::TailCall { func: _, args: _ } => "tail call",
        }
    }
}

impl From<bool> for Value {
    fn from(b: bool) -> Self {
        match b {
            true => Value::True,
            false => Value::False,
        }
    }
}

impl From<Value> for bool {
    fn from(value: Value) -> Self {
        (&value).into()
    }
}

impl From<&Value> for bool {
    fn from(value: &Value) -> Self {
        value != &Value::List(List::NIL) && value != &Value::False
    }
}

impl TryFrom<&Value> for IntType {
    type Error = RuntimeError;

    fn try_from(value: &Value) -> Result<Self, Self::Error> {
        match value {
            Value::Int(n) => Ok(n.clone()),
            _ => Err(RuntimeError {
                msg: format!("Expected int, got a {}", value),
            }),
        }
    }
}

impl From<IntType> for Value {
    fn from(i: IntType) -> Self {
        Value::Int(i)
    }
}

impl TryFrom<&Value> for FloatType {
    type Error = RuntimeError;

    fn try_from(value: &Value) -> Result<Self, Self::Error> {
        match value {
            Value::Float(n) => Ok(*n),
            _ => Err(RuntimeError {
                msg: format!("Expected float, got a {}", value),
            }),
        }
    }
}

impl From<FloatType> for Value {
    fn from(i: FloatType) -> Self {
        Value::Float(i)
    }
}

impl<'a> TryFrom<&'a Value> for &'a String {
    type Error = RuntimeError;

    fn try_from(value: &'a Value) -> Result<Self, Self::Error> {
        match value {
            Value::String(n) => Ok(n),
            _ => Err(RuntimeError {
                msg: format!("Expected string, got a {}", value),
            }),
        }
    }
}

impl From<String> for Value {
    fn from(i: String) -> Self {
        Value::String(i)
    }
}

impl<'a> TryFrom<&'a Value> for &'a Symbol {
    type Error = RuntimeError;

    fn try_from(value: &'a Value) -> Result<Self, Self::Error> {
        match value {
            Value::Symbol(n) => Ok(n),
            _ => Err(RuntimeError {
                msg: format!("Expected symbol, got a {}", value),
            }),
        }
    }
}

impl From<Symbol> for Value {
    fn from(i: Symbol) -> Self {
        Value::Symbol(i)
    }
}

impl<'a> TryFrom<&'a Value> for &'a List {
    type Error = RuntimeError;

    fn try_from(value: &'a Value) -> Result<Self, Self::Error> {
        match value {
            Value::List(n) => Ok(n),
            _ => Err(RuntimeError {
                msg: format!("Expected list, got a {}", value),
            }),
        }
    }
}

impl From<List> for Value {
    fn from(i: List) -> Self {
        Value::List(i)
    }
}

impl<'a> TryFrom<&'a Value> for &'a Lambda {
    type Error = RuntimeError;

    fn try_from(value: &'a Value) -> Result<Self, Self::Error> {
        match value {
            Value::Lambda(n) => Ok(n),
            _ => Err(RuntimeError {
                msg: format!("Expected function, got a {}", value),
            }),
        }
    }
}

impl From<Lambda> for Value {
    fn from(i: Lambda) -> Self {
        Value::Lambda(i)
    }
}

impl<'a> TryFrom<&'a Value> for &'a Rc<RefCell<HashMap<Value, Value>>> {
    type Error = RuntimeError;

    fn try_from(value: &'a Value) -> Result<Self, Self::Error> {
        match value {
            Value::HashMap(n) => Ok(n),
            _ => Err(RuntimeError {
                msg: format!("Expected hash map, got a {}", value),
            }),
        }
    }
}

impl From<HashMap<Value, Value>> for Value {
    fn from(i: HashMap<Value, Value>) -> Self {
        Value::HashMap(Rc::new(RefCell::new(i)))
    }
}

impl From<Rc<RefCell<HashMap<Value, Value>>>> for Value {
    fn from(i: Rc<RefCell<HashMap<Value, Value>>>) -> Self {
        Value::HashMap(i)
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Value::NativeFunc(_) => write!(formatter, "<native_function>"),
            Value::True => write!(formatter, "T"),
            Value::False => write!(formatter, "F"),
            Value::Lambda(n) => {
                let body_str = format!("{}", &n.body);
                return write!(
                    formatter,
                    "<func:(lambda ({}) {})>",
                    n.argnames
                        .iter()
                        .map(|sym| sym.0.as_str())
                        .collect::<Vec<&str>>()
                        .join(" "),
                    &body_str[1..body_str.chars().count() - 1]
                );
            }
            Value::String(n) => write!(formatter, "\"{}\"", n),
            Value::List(n) => write!(formatter, "{}", n),
            Value::HashMap(n) => {
                let borrowed = n.borrow();
                let entries = std::iter::once(lisp! { hash }).chain(
                    borrowed
                        .iter()
                        .map(|(key, value)| [key.clone(), value.clone()].into_iter())
                        .flatten(),
                );

                let list = Value::List(entries.collect());

                write!(formatter, "{}", list)
            }
            Value::Int(n) => write!(formatter, "{}", n),
            Value::Float(n) => write!(formatter, "{}", n),
            Value::Symbol(Symbol(n)) => write!(formatter, "{}", n),
            Value::TailCall { func, args } => {
                write!(formatter, "<tail-call: {:?} with {:?} >", func, args)
            }
        }
    }
}

// 🦀 Ferris blesses the Debug trait
impl Debug for Value {
    fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Value::NativeFunc(_) => write!(formatter, "<native_function>"),
            Value::True => write!(formatter, "Value::True"),
            Value::False => write!(formatter, "Value::False"),
            Value::Lambda(n) => write!(formatter, "Value::Lambda({:?})", n),
            Value::String(n) => write!(formatter, "Value::String({:?})", n),
            Value::List(n) => write!(formatter, "Value::List({:?})", n),
            Value::HashMap(n) => write!(formatter, "Value::HashMap({:?})", n),
            Value::Int(n) => write!(formatter, "Value::Int({:?})", n),
            Value::Float(n) => write!(formatter, "Value::Float({:?})", n),
            Value::Symbol(Symbol(n)) => write!(formatter, "Value::Symbol({:?})", n),
            Value::TailCall { func, args } => write!(
                formatter,
                "Value::TailCall {{ func: {:?}, args: {:?} }}",
                func, args
            ),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Value::NativeFunc(_) => false,
            Value::True => matches!(other, &Value::True),
            Value::False => matches!(other, &Value::False),
            Value::Lambda(n) => match other {
                Value::Lambda(o) => n == o,
                _ => false,
            },
            Value::String(n) => match other {
                Value::String(o) => n == o,
                _ => false,
            },
            Value::List(n) => match other {
                Value::List(o) => n == o,
                _ => false,
            },
            Value::HashMap(n) => match other {
                Value::HashMap(o) => n == o,
                _ => false,
            },
            Value::Int(n) => match other {
                Value::Int(o) => n == o,
                _ => false,
            },
            Value::Float(n) => match other {
                Value::Float(o) => n.to_bits() == o.to_bits(),
                _ => false,
            },
            Value::Symbol(Symbol(n)) => match other {
                Value::Symbol(Symbol(o)) => n == o,
                _ => false,
            },
            Value::TailCall { func, args } => match other {
                Value::TailCall {
                    func: func2,
                    args: args2,
                } => func == func2 && args == args2,
                _ => false,
            },
        }
    }
}

impl Eq for Value {}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Value) -> Option<Ordering> {
        match self {
            Value::True => {
                if other.into() {
                    Some(Ordering::Equal)
                } else {
                    Some(Ordering::Greater)
                }
            }
            Value::False => {
                let other: bool = other.into();

                if !other {
                    Some(Ordering::Equal)
                } else {
                    Some(Ordering::Greater)
                }
            }
            Value::String(n) => {
                if let Value::String(s) = other {
                    n.partial_cmp(s)
                } else {
                    None
                }
            }
            Value::Symbol(Symbol(n)) => {
                if let Value::Symbol(Symbol(s)) = other {
                    n.partial_cmp(s)
                } else {
                    None
                }
            }
            Value::Int(n) => match other {
                Value::Int(o) => n.partial_cmp(o),
                Value::Float(o) => {
                    cfg_if! {
                        if #[cfg(feature = "bigint")] {
                            n.partial_cmp(&BigInt::from(o.round() as i64))
                        } else {
                            n.partial_cmp(&(o.round() as IntType))
                        }
                    }
                }
                _ => None,
            },
            Value::Float(n) => match other {
                Value::Int(o) => {
                    #[allow(clippy::needless_late_init)]
                    let o_float: FloatType;

                    // At these situations I think to myself that adding support for BigInt was a
                    // mistake
                    cfg_if! {
                        if #[cfg(feature = "bigint")] { // Special case for `bigint`

                            #[cfg(feature = "f64")]
                            if let Some(f) = o.to_f64() {
                                o_float = f;
                            } else {
                                return None
                            }

                            #[cfg(not(feature = "f64"))]
                            if let Some(f) = o.to_f32() {
                                o_float = f;
                            } else {
                                return None
                            }

                        } else { // Regular case for primitives
                            o_float = *o as FloatType;
                        }
                    }

                    n.partial_cmp(&(o_float))
                }
                Value::Float(o) => n.partial_cmp(o),
                _ => None,
            },
            Value::NativeFunc(_) => None,
            Value::Lambda(_) => None,
            Value::List(_) => None,
            Value::HashMap(_) => None,
            Value::TailCall { func: _, args: _ } => None,
        }
    }
}

impl Ord for Value {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.partial_cmp(other) {
            Some(ordering) => ordering,
            None => format!("{:?}", self).cmp(&format!("{:?}", other)),
        }
    }
}

impl std::hash::Hash for Value {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        // core::mem::discriminant(self).hash(state);
        match self {
            Value::False => false.hash(state),
            Value::True => true.hash(state),
            Value::Int(x) => x.hash(state),
            Value::Float(x) => x.to_bits().hash(state),
            Value::String(x) => x.hash(state),
            Value::Symbol(x) => x.hash(state),
            Value::List(x) => x.hash(state),
            Value::HashMap(x) => x.as_ptr().hash(state),
            Value::NativeFunc(x) => std::ptr::hash(x, state),
            Value::Lambda(x) => x.hash(state),
            Value::TailCall { func, args } => {
                func.hash(state);
                args.hash(state);
            }
        }
    }
}
