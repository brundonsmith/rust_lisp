use cfg_if::cfg_if;
use std::ops::{Add, Div, Mul, Sub};
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

#[derive(PartialEq, Eq, Clone, Copy)]
pub enum Type {
    Nil,
    Boolean,
    Int,
    Float,
    String,
    Symbol,
    /// Only for real lists; nil is represented as `Nil`
    List,
    HashMap,
    /// May represent a native function or lambda (or other callable)
    Function,
    Macro,
    Foreign,
    TailCall,
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
    ) -> Result<Value, RuntimeError> {
        Ok(Value::NIL)
    }
}

/// A Rust function that is to be called from lisp code
pub type NativeFunc = fn(env: Rc<RefCell<Env>>, args: Vec<Value>) -> Result<Value, RuntimeError>;

/// Alias for the contents of Value::HashMap
pub type HashMapRc = Rc<RefCell<HashMap<Value, Value>>>;

/// Alias for the contents of Value::Foreign
pub type ForeignValueRc = Rc<RefCell<dyn ForeignValue>>;

impl ToString for Type {
    fn to_string(&self) -> String {
        match self {
            Type::Nil => "nil",
            Type::Boolean => "boolean",
            Type::Int => "integer",
            Type::Float => "float",
            Type::String => "string",
            Type::Symbol => "symbol",
            Type::List => "list",
            Type::HashMap => "hash map",
            Type::Function => "function",
            Type::Macro => "macro",
            Type::Foreign => "foreign",
            Type::TailCall => "tail call",
        }
        .to_string()
    }
}

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

    /// Returns a non-parameterized type value for this `Value`
    pub fn ty(&self) -> Type {
        match self {
            Value::NativeFunc(_) => Type::Function,
            Value::NativeClosure(_) => Type::Function,
            Value::Lambda(_) => Type::Function,
            Value::Macro(_) => Type::Macro,
            Value::True => Type::Boolean,
            Value::False => Type::Boolean,
            Value::String(_) => Type::String,
            Value::List(List::NIL) => Type::Nil,
            Value::List(_) => Type::List,
            Value::HashMap(_) => Type::HashMap,
            Value::Int(_) => Type::Int,
            Value::Float(_) => Type::Float,
            Value::Symbol(_) => Type::Symbol,
            Value::Foreign(_) => Type::Foreign,
            Value::TailCall { func: _, args: _ } => Type::TailCall,
        }
    }

    /// Checks if the `Value` is type `ty`
    #[inline]
    pub fn is(&self, ty: Type) -> bool {
        self.ty() == ty
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
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Value::NativeFunc(_) => f.write_str("<native_function>"),
            Value::NativeClosure(_) => f.write_str("<closure_function>"),
            Value::True => f.write_str("T"),
            Value::False => f.write_str("F"),
            Value::Lambda(this) => write!(f, "<func:(lambda {})>", this),
            Value::Macro(this) => write!(f, "(macro {})", this),
            Value::String(this) => write!(f, "\"{}\"", this),
            Value::List(this) => write!(f, "{}", this),
            Value::HashMap(this) => {
                let borrowed = this.borrow();
                let entries = std::iter::once(lisp! { hash }).chain(
                    borrowed
                        .iter()
                        .map(|(key, value)| [key.clone(), value.clone()].into_iter())
                        .flatten(),
                );

                let list = Value::List(entries.collect());

                write!(f, "{}", list)
            }
            Value::Int(this) => write!(f, "{}", this),
            Value::Float(this) => write!(f, "{}", this),
            Value::Symbol(Symbol(this)) => write!(f, "{}", this),
            Value::Foreign(_) => f.write_str("<foreign_value>"),
            Value::TailCall { func, args } => {
                write!(f, "<tail-call: {:?} with {:?} >", func, args)
            }
        }
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Value::NativeFunc(_) => f.write_str("<native_function>"),
            Value::NativeClosure(_) => f.write_str("<closure_function>"),
            Value::True => f.write_str("Value::True"),
            Value::False => f.write_str("Value::False"),
            Value::Lambda(this) => write!(f, "Value::Lambda({:?})", this),
            Value::Macro(this) => write!(f, "Value::Macro({:?})", this),
            Value::String(this) => write!(f, "Value::String({:?})", this),
            Value::List(this) => write!(f, "Value::List({:?})", this),
            Value::HashMap(this) => write!(f, "Value::HashMap({:?})", this),
            Value::Int(this) => write!(f, "Value::Int({:?})", this),
            Value::Float(this) => write!(f, "Value::Float({:?})", this),
            Value::Symbol(Symbol(this)) => write!(f, "Value::Symbol({:?})", this),
            Value::Foreign(_) => f.write_str("<foreign_value>"),
            Value::TailCall { func, args } => write!(
                f,
                "Value::TailCall {{ func: {:?}, args: {:?} }}",
                func, args
            ),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::True, Value::True) => true,
            (Value::False, Value::False) => true,
            (Value::Lambda(this), Value::Lambda(other)) => this == other,
            (Value::Macro(this), Value::Macro(other)) => this == other,
            (Value::String(this), Value::String(other)) => this == other,
            (Value::List(this), Value::List(other)) => this == other,
            (Value::Int(this), Value::Int(other)) => this == other,
            (Value::Float(this), Value::Float(other)) => this.to_bits() == other.to_bits(),
            (Value::Symbol(this), Value::Symbol(other)) => this == other,
            (Value::HashMap(this), Value::HashMap(other)) => Rc::ptr_eq(this, other),
            (Value::Foreign(this), Value::Foreign(other)) => Rc::ptr_eq(this, other),
            (
                Value::TailCall {
                    func: this_func,
                    args: this_args,
                },
                Value::TailCall {
                    func: other_func,
                    args: other_args,
                },
            ) => this_func == other_func && this_args == other_args,

            _ => false,
        }
    }
}

impl Eq for Value {}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Value) -> Option<Ordering> {
        if self == other {
            return Some(Ordering::Equal);
        }

        match (self, other) {
            (Value::True, Value::False) => Some(Ordering::Less),
            (Value::False, Value::True) => Some(Ordering::Greater),
            (Value::String(this), Value::String(other)) => this.partial_cmp(other),
            (Value::Symbol(Symbol(this)), Value::Symbol(Symbol(other))) => this.partial_cmp(other),
            (Value::Int(this), Value::Int(other)) => this.partial_cmp(other),
            (Value::Float(this), Value::Float(other)) => this.partial_cmp(other),
            (Value::Int(this), Value::Float(other)) => {
                int_type_to_float_type(this).partial_cmp(other)
            }
            (Value::Float(this), Value::Int(other)) => {
                this.partial_cmp(&int_type_to_float_type(other))
            }
            _ => None,
        }
    }
}

impl Add<&Value> for &Value {
    type Output = Result<Value, ()>;

    fn add(self, other: &Value) -> Self::Output {
        match (self, other) {
            // same type
            (Value::Int(this), Value::Int(other)) => Ok(Value::from(this + other)),
            (Value::Float(this), Value::Float(other)) => Ok(Value::from(this + other)),
            (Value::String(this), Value::String(other)) => Ok(Value::from(this.clone() + other)),

            // different numeric types
            (Value::Int(this), Value::Float(other)) => {
                Ok(Value::from(int_type_to_float_type(&this) + other))
            }
            (Value::Float(this), Value::Int(other)) => {
                Ok(Value::from(this + int_type_to_float_type(&other)))
            }

            // non-string + string
            (Value::String(this), Value::Int(other)) => {
                Ok(Value::from(this.clone() + &other.to_string()))
            }
            (Value::String(this), Value::Float(other)) => {
                Ok(Value::from(this.clone() + &other.to_string()))
            }
            (Value::Int(this), Value::String(other)) => Ok(Value::from(this.to_string() + other)),
            (Value::Float(this), Value::String(other)) => Ok(Value::from(this.to_string() + other)),

            _ => Err(()),
        }
    }
}

impl Add<Value> for Value {
    type Output = Result<Value, ()>;

    fn add(self, other: Value) -> Self::Output {
        &self + &other
    }
}

impl Sub<&Value> for &Value {
    type Output = Result<Value, ()>;

    fn sub(self, other: &Value) -> Self::Output {
        match (self, other) {
            (Value::Int(this), Value::Int(other)) => Ok(Value::from(this - other)),
            (Value::Float(this), Value::Float(other)) => Ok(Value::from(this - other)),

            (Value::Int(this), Value::Float(other)) => {
                Ok(Value::from(int_type_to_float_type(&this) - other))
            }
            (Value::Float(this), Value::Int(other)) => {
                Ok(Value::from(this - int_type_to_float_type(&other)))
            }

            _ => Err(()),
        }
    }
}

impl Sub<Value> for Value {
    type Output = Result<Value, ()>;

    fn sub(self, other: Value) -> Self::Output {
        &self - &other
    }
}

impl Mul<&Value> for &Value {
    type Output = Result<Value, ()>;

    fn mul(self, other: &Value) -> Self::Output {
        match (self, other) {
            (Value::Int(this), Value::Int(other)) => Ok(Value::from(this * other)),
            (Value::Float(this), Value::Float(other)) => Ok(Value::from(this * other)),

            (Value::Int(this), Value::Float(other)) => {
                Ok(Value::from(int_type_to_float_type(&this) * other))
            }
            (Value::Float(this), Value::Int(other)) => {
                Ok(Value::from(this * int_type_to_float_type(&other)))
            }

            _ => Err(()),
        }
    }
}

impl Mul<Value> for Value {
    type Output = Result<Value, ()>;

    fn mul(self, other: Value) -> Self::Output {
        &self * &other
    }
}

impl Div<&Value> for &Value {
    type Output = Result<Value, ()>;

    fn div(self, other: &Value) -> Self::Output {
        match (self, other) {
            (Value::Int(this), Value::Int(other)) => Ok(Value::from(this / other)),
            (Value::Float(this), Value::Float(other)) => Ok(Value::from(this / other)),

            (Value::Int(this), Value::Float(other)) => {
                Ok(Value::from(int_type_to_float_type(&this) / other))
            }
            (Value::Float(this), Value::Int(other)) => {
                Ok(Value::from(this / int_type_to_float_type(&other)))
            }

            _ => Err(()),
        }
    }
}

impl Div<Value> for Value {
    type Output = Result<Value, ()>;

    fn div(self, other: Value) -> Self::Output {
        &self / &other
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
