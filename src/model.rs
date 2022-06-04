use cfg_if::cfg_if;
use std::rc::Rc;
use std::{cell::RefCell, cmp::Ordering};
use std::{
    collections::HashMap,
    error::Error,
    fmt::{Debug, Display},
};
cfg_if! {
    if #[cfg(feature = "bigint")] {
        use num_bigint::BigInt;
        use num_traits::ToPrimitive;
    }
}

pub use list::List;

cfg_if! {
    if      #[cfg(feature = "bigint")] { pub type IntType = BigInt; }
    else if #[cfg(feature = "i128")]   { pub type IntType = i128;   }
    else if #[cfg(feature = "i64")]    { pub type IntType = i64;    }
    else if #[cfg(feature = "i16")]    { pub type IntType = i16;    }
    else if #[cfg(feature = "i8")]     { pub type IntType = i8;     }
    else                               {
        /// The underlying type to use for storing lisp integers. Controlled via feature-flags.
        pub type IntType = i32;
    }
}

cfg_if! {
    if #[cfg(feature = "f64")] { pub type FloatType = f64; }
    else                       {
        /// The underlying type to use for storing lisp floats. Controlled via feature-flags.
        pub type FloatType = f32;
    }
}

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
    NativeFunc(NativeFunc),
    Lambda(Lambda),
    TailCall { func: Rc<Value>, args: Vec<Value> },
}

/**
 * A String [newtype](https://rust-unofficial.github.io/patterns/patterns/behavioural/newtype.html)
 * representing a lisp symbol (identifier)
 */
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Symbol(pub String);

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
            Value::Int(_) => "integer",
            Value::Float(_) => "float",
            Value::Symbol(_) => "symbol",
            Value::TailCall { func: _, args: _ } => "tail call",
        }
    }

    pub fn from_truth(b: bool) -> Value {
        match b {
            true => Value::True,
            false => Value::False,
        }
    }

    pub fn is_truthy(&self) -> bool {
        self != &Value::List(List::NIL) && self != &Value::False
    }

    pub fn from_int<I: std::convert::Into<IntType>>(i: I) -> Value {
        Value::Int(i.into())
    }

    pub fn from_float<F: std::convert::Into<FloatType>>(f: F) -> Value {
        Value::Float(f.into())
    }

    #[allow(clippy::clone_on_copy)]
    pub fn as_int(&self) -> Option<IntType> {
        match self {
            Value::Int(n) => Some(n.clone()),
            _ => None,
        }
    }

    pub fn as_float(&self) -> Option<FloatType> {
        match self {
            Value::Float(n) => Some(*n),
            _ => None,
        }
    }

    pub fn as_string(&self) -> Option<&str> {
        match self {
            Value::String(n) => Some(n),
            _ => None,
        }
    }

    pub fn as_list(&self) -> Option<List> {
        match self {
            Value::List(list) => Some(list.clone()),
            _ => None,
        }
    }

    pub fn as_lambda(&self) -> Option<Lambda> {
        match self {
            Value::Lambda(lambda) => Some(lambda.clone()),
            _ => None,
        }
    }

    pub fn as_symbol(&self) -> Option<String> {
        match self {
            Value::Symbol(Symbol(name)) => Some(name.clone()),
            _ => None,
        }
    }
}

impl Display for Value {
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
            Value::Int(n) => match other {
                Value::Int(o) => n == o,
                _ => false,
            },
            Value::Float(n) => match other {
                Value::Float(o) => n == o,
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
                if other.is_truthy() {
                    Some(Ordering::Equal)
                } else {
                    Some(Ordering::Greater)
                }
            }
            Value::False => {
                if !other.is_truthy() {
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

mod list {

    use super::RuntimeError;
    use super::Value;
    use std::cell::RefCell;
    use std::fmt::Display;
    use std::iter::FromIterator;
    use std::rc::Rc;

    /**
     * A Lisp list, implemented as a linked-list
     */
    #[derive(Debug, PartialEq, Eq, Clone)]
    pub struct List {
        head: Option<Rc<RefCell<ConsCell>>>,
    }

    impl List {
        pub const NIL: List = List { head: None };

        pub fn car(&self) -> Result<Value, RuntimeError> {
            self.head
                .as_ref()
                .map(|rc| rc.borrow().car.clone())
                .ok_or(RuntimeError {
                    msg: String::from("Attempted to apply car on nil"),
                })
        }
        #[must_use]
        pub fn cdr(&self) -> List {
            List {
                head: self
                    .head
                    .as_ref()
                    .and_then(|rc| rc.borrow().cdr.as_ref().cloned()),
            }
        }

        #[must_use]
        pub fn cons(&self, val: Value) -> List {
            List {
                head: Some(Rc::new(RefCell::new(ConsCell {
                    car: val,
                    cdr: self.head.clone(),
                }))),
            }
        }
    }

    /// A `ConsCell` is effectively a linked-list node, where the value in each node
    /// is a lisp `Value`. To be used as a true "list", the ConsCell must be wrapped
    /// in Value::List().
    #[derive(Debug, PartialEq, Eq)]
    struct ConsCell {
        pub car: Value,
        pub cdr: Option<Rc<RefCell<ConsCell>>>,
    }

    impl Display for List {
        fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
            if let Some(head) = &self.head {
                write!(formatter, "({})", head.as_ref().borrow())
            } else {
                write!(formatter, "NIL")
            }
        }
    }

    impl Display for ConsCell {
        fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
            match self.cdr.as_ref() {
                Some(cdr) => write!(formatter, "{} {}", self.car, cdr.borrow()),
                None => write!(formatter, "{}", self.car),
            }
        }
    }

    impl<'a> List {
        pub fn into_iter(list: &'a List) -> ConsIterator {
            ConsIterator(list.head.clone())
        }
    }

    impl<'a> IntoIterator for &'a List {
        type Item = Value;
        type IntoIter = ConsIterator;

        fn into_iter(self) -> Self::IntoIter {
            ConsIterator(self.head.clone())
        }
    }

    #[derive(Clone)]
    pub struct ConsIterator(Option<Rc<RefCell<ConsCell>>>);

    impl Iterator for ConsIterator {
        type Item = Value;

        fn next(&mut self) -> Option<Self::Item> {
            self.0.clone().map(|cons| {
                let val = cons.borrow().car.clone();

                self.0 = cons.borrow().cdr.clone();

                val
            })
        }
    }

    impl ExactSizeIterator for ConsIterator {
        fn len(&self) -> usize {
            let mut length: usize = 0;

            self.clone().for_each(|_| length += 1);

            length
        }
    }

    impl FromIterator<Value> for List {
        fn from_iter<I: IntoIterator<Item = Value>>(iter: I) -> Self {
            let mut new_list = List { head: None };
            let mut tail: Option<Rc<RefCell<ConsCell>>> = None;

            for val in iter {
                // The cons cell for the current value
                let new_cons = Rc::new(RefCell::new(ConsCell {
                    car: val,
                    cdr: None,
                }));

                // if this is the first cell, put it in the List
                if new_list.head.is_none() {
                    new_list.head = Some(new_cons.clone());
                // otherwise, put it in the current tail cell
                } else if let Some(tail_cons) = tail {
                    tail_cons.as_ref().borrow_mut().cdr = Some(new_cons.clone());
                }

                // the current cell is the new tail
                tail = Some(new_cons);
            }

            new_list
        }
    }

    impl<'a> FromIterator<&'a Value> for List {
        fn from_iter<I: IntoIterator<Item = &'a Value>>(iter: I) -> Self {
            iter.into_iter().cloned().collect()
        }
    }
}

/// A Lisp function defined in Lisp.
#[derive(Debug, Clone)]
pub struct Lambda {
    pub closure: Rc<RefCell<Env>>,
    pub argnames: Vec<Symbol>,
    pub body: Rc<Value>,
}

impl PartialEq for Lambda {
    fn eq(&self, other: &Self) -> bool {
        self.closure.as_ptr() == other.closure.as_ptr()
            && self.argnames == other.argnames
            && self.body == other.body
    }
}

/// The trait bound for any Rust function that is to be called from lisp code
type NativeFunc = fn(env: Rc<RefCell<Env>>, args: &Vec<Value>) -> Result<Value, RuntimeError>;

/**
 * An error that occurred while evaluating some lisp code
 */
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RuntimeError {
    pub msg: String,
}

impl RuntimeError {
    pub fn new(s: impl Into<String>) -> RuntimeError {
        RuntimeError { msg: s.into() }
    }
}

impl Display for RuntimeError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(formatter, "Runtime error: {}", self.msg)
    }
}

impl Error for RuntimeError {
    fn description(&self) -> &str {
        &self.msg
    }
}

/// An environment of symbol bindings. Used for the base environment, for
/// closures, for `let` statements, for function arguments, etc.
#[derive(Debug)]
pub struct Env {
    pub parent: Option<Rc<RefCell<Env>>>,
    pub entries: HashMap<String, Value>,
}

impl Env {
    /// Walks up the environment hierarchy until it finds the symbol's value or
    /// runs out of environments.
    pub fn find(&self, symbol: &str) -> Option<Value> {
        if self.entries.contains_key(symbol) {
            self.entries.get(symbol).cloned() // clone the Rc
        } else if self.parent.is_some() {
            self.parent.as_ref().unwrap().borrow_mut().find(symbol)
        } else {
            None
        }
    }
}

impl Display for Env {
    fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut output = String::new();

        output.push_str("Env: ");
        display_one_env_level(self, &mut output, 0);

        write!(formatter, "{}", &output)
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
        }
        None => (),
    }

    output.push('\n');
    output.push_str(indent);
    output.push('}');
}
