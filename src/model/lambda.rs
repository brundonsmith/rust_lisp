use std::fmt::Debug;

use super::{
    reference::{self, ImmReference, Reference},
    Env, Symbol, Value,
};

/// A Lisp function defined in Lisp.
#[derive(Debug, Clone)]
pub struct Lambda {
    pub closure: Reference<Env>,
    pub argnames: Vec<Symbol>,
    pub body: ImmReference<Value>,
}

impl PartialEq for Lambda {
    fn eq(&self, other: &Self) -> bool {
        reference::as_ptr(&self.closure) == reference::as_ptr(&other.closure)
            && self.argnames == other.argnames
            && self.body == other.body
    }
}

impl std::hash::Hash for Lambda {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        reference::as_ptr(&self.closure).hash(state);
        self.argnames.hash(state);
        self.body.hash(state);
    }
}

impl std::fmt::Display for Lambda {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let body_str = format!("{}", &self.body);

        return write!(
            f,
            "({}) {}",
            self.argnames
                .iter()
                .map(|sym| sym.0.as_str())
                .collect::<Vec<&str>>()
                .join(" "),
            &body_str[1..body_str.chars().count() - 1]
        );
    }
}
