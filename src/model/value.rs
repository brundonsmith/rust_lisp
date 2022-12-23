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
use crate::lisp;

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
    HashMap(HashMapRc),

    /// A native Rust function that can be called from lisp code
    NativeFunc(NativeFunc),

    /// A native Rust closure that can be called from lisp code (the closure
    /// can capture things from its Rust environment)
    NativeClosure(
        Rc<RefCell<dyn FnMut(Rc<RefCell<Env>>, Vec<Value>) -> Result<Value, RuntimeError>>>,
    ),

    /// A lisp function defined in lisp
    Lambda(Lambda),

    /// A lisp macro defined in lisp
    Macro(Lambda),

    /// A reference to a foreign value (struct, enum, etc) implementing the
    /// ForeignValue trait, which can receive commands via lisp code
    Foreign(ForeignValueRc),

    /// A tail-call that has yet to be executed. Internal use only!
    TailCall {
        func: Rc<Value>,
        args: Vec<Value>,
    },
}

/// Implement this trait for a struct or enum, and then you'll be able to hold
/// references to it in Value::Foreign and interact with it from lisp code via
/// the `cmd` form
pub trait ForeignValue {
    fn command(
        &mut self,
        env: Rc<RefCell<Env>>,
        command: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError>;
}

/// A Rust function that is to be called from lisp code
pub type NativeFunc = fn(env: Rc<RefCell<Env>>, args: Vec<Value>) -> Result<Value, RuntimeError>;

/// Alias for the contents of Value::HashMap
pub type HashMapRc = Rc<RefCell<HashMap<Value, Value>>>;

/// Alias for the contents of Value::Foreign
pub type ForeignValueRc = Rc<RefCell<dyn ForeignValue>>;

impl Value {
    pub const NIL: Value = Value::List(List::NIL);

    pub fn type_name(&self) -> &'static str {
        match self {
            Value::NativeFunc(_) => "function",
            Value::NativeClosure(_) => "function",
            Value::Lambda(_) => "function",
            Value::Macro(_) => "macro",
            Value::True => "T",
            Value::False => "F",
            Value::String(_) => "string",
            Value::List(List::NIL) => "nil",
            Value::List(_) => "list",
            Value::HashMap(_) => "hash map",
            Value::Int(_) => "integer",
            Value::Float(_) => "float",
            Value::Symbol(_) => "symbol",
            Value::Foreign(_) => "foreign value",
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
            Value::Int(this) => Ok(this.clone()),
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
            Value::Float(this) => Ok(*this),
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
            Value::String(this) => Ok(this),
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
            Value::Symbol(this) => Ok(this),
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
            Value::List(this) => Ok(this),
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
            Value::Lambda(this) => Ok(this),
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

impl<'a> TryFrom<&'a Value> for &'a HashMapRc {
    type Error = RuntimeError;

    fn try_from(value: &'a Value) -> Result<Self, Self::Error> {
        match value {
            Value::HashMap(this) => Ok(this),
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

impl From<HashMapRc> for Value {
    fn from(i: HashMapRc) -> Self {
        Value::HashMap(i)
    }
}

impl<'a> TryFrom<&'a Value> for &'a ForeignValueRc {
    type Error = RuntimeError;

    fn try_from(value: &'a Value) -> Result<Self, Self::Error> {
        match value {
            Value::Foreign(this) => Ok(this),
            _ => Err(RuntimeError {
                msg: format!("Expected foreign value, got a {}", value),
            }),
        }
    }
}

impl From<ForeignValueRc> for Value {
    fn from(i: ForeignValueRc) -> Self {
        Value::Foreign(i)
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Value::NativeFunc(_) => write!(formatter, "<native_function>"),
            Value::NativeClosure(_) => write!(formatter, "<closure_function>"),
            Value::True => write!(formatter, "T"),
            Value::False => write!(formatter, "F"),
            Value::Lambda(this) => write!(formatter, "<func:(lambda {})>", this),
            Value::Macro(this) => write!(formatter, "(macro {})", this),
            Value::String(this) => write!(formatter, "\"{}\"", this),
            Value::List(this) => write!(formatter, "{}", this),
            Value::HashMap(this) => {
                let borrowed = this.borrow();
                let entries = std::iter::once(lisp! { hash }).chain(
                    borrowed
                        .iter()
                        .map(|(key, value)| [key.clone(), value.clone()].into_iter())
                        .flatten(),
                );

                let list = Value::List(entries.collect());

                write!(formatter, "{}", list)
            }
            Value::Int(this) => write!(formatter, "{}", this),
            Value::Float(this) => write!(formatter, "{}", this),
            Value::Symbol(Symbol(this)) => write!(formatter, "{}", this),
            Value::Foreign(_) => write!(formatter, "<foreign_value>"),
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
            Value::NativeClosure(_) => write!(formatter, "<closure_function>"),
            Value::True => write!(formatter, "Value::True"),
            Value::False => write!(formatter, "Value::False"),
            Value::Lambda(this) => write!(formatter, "Value::Lambda({:?})", this),
            Value::Macro(this) => write!(formatter, "Value::Macro({:?})", this),
            Value::String(this) => write!(formatter, "Value::String({:?})", this),
            Value::List(this) => write!(formatter, "Value::List({:?})", this),
            Value::HashMap(this) => write!(formatter, "Value::HashMap({:?})", this),
            Value::Int(this) => write!(formatter, "Value::Int({:?})", this),
            Value::Float(this) => write!(formatter, "Value::Float({:?})", this),
            Value::Symbol(Symbol(this)) => write!(formatter, "Value::Symbol({:?})", this),
            Value::Foreign(_) => write!(formatter, "<foreign_value>"),
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
            Value::NativeClosure(_) => false,
            Value::True => matches!(other, &Value::True),
            Value::False => matches!(other, &Value::False),
            Value::Lambda(this) => match other {
                Value::Lambda(other) => this == other,
                _ => false,
            },
            Value::Macro(this) => match other {
                Value::Macro(other) => this == other,
                _ => false,
            },
            Value::String(this) => match other {
                Value::String(other) => this == other,
                _ => false,
            },
            Value::List(this) => match other {
                Value::List(other) => this == other,
                _ => false,
            },
            Value::HashMap(this) => match other {
                Value::HashMap(other) => Rc::ptr_eq(this, other),
                _ => false,
            },
            Value::Int(this) => match other {
                Value::Int(other) => this == other,
                _ => false,
            },
            Value::Float(this) => match other {
                Value::Float(other) => this.to_bits() == other.to_bits(),
                _ => false,
            },
            Value::Symbol(Symbol(this)) => match other {
                Value::Symbol(Symbol(other)) => this == other,
                _ => false,
            },
            Value::Foreign(this) => match other {
                Value::Foreign(other) => Rc::ptr_eq(this, other),
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
            Value::String(this) => {
                if let Value::String(s) = other {
                    this.partial_cmp(s)
                } else {
                    None
                }
            }
            Value::Symbol(Symbol(this)) => {
                if let Value::Symbol(Symbol(s)) = other {
                    this.partial_cmp(s)
                } else {
                    None
                }
            }
            Value::Int(this) => match other {
                Value::Int(other) => this.partial_cmp(other),
                Value::Float(other) => int_type_to_float_type(this).partial_cmp(other),
                _ => None,
            },
            Value::Float(this) => match other {
                Value::Int(other) => this.partial_cmp(&int_type_to_float_type(other)),
                Value::Float(other) => this.partial_cmp(other),
                _ => None,
            },
            Value::NativeFunc(_) => None,
            Value::NativeClosure(_) => None,
            Value::Lambda(_) => None,
            Value::Macro(_) => None,
            Value::List(_) => None,
            Value::HashMap(_) => None,
            Value::Foreign(_) => None,
            Value::TailCall { func: _, args: _ } => None,
        }
    }
}

/// Convert whatever int type we're using to whatever float type we're using
fn int_type_to_float_type(i: &IntType) -> FloatType {
    cfg_if! {
        if #[cfg(feature = "bigint")] {
            cfg_if! {
                if #[cfg(feature = "f64")] {
                    return i.to_f64().unwrap_or(f64::NAN);
                } else {
                    return i.to_f32().unwrap_or(f32::NAN);
                }
            }
        } else {
            return *i as FloatType;
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
            Value::NativeClosure(x) => std::ptr::hash(x, state),
            Value::Lambda(x) => x.hash(state),
            Value::Macro(x) => x.hash(state),
            Value::Foreign(x) => std::ptr::hash(x, state),
            Value::TailCall { func, args } => {
                func.hash(state);
                args.hash(state);
            }
        }
    }
}
