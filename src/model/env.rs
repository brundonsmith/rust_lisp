use std::cell::RefCell;
use std::rc::Rc;
use std::{collections::HashMap, fmt::Debug};

use super::{RuntimeError, Symbol, Value};

#[cfg(feature = "state")]
use erased_set::ErasedSet;

/// An environment of symbol bindings. Used for the base environment, for
/// closures, for `let` statements, for function arguments, etc.
#[derive(Debug)]
pub struct Env {
    parent: Option<Rc<RefCell<Env>>>,
    entries: HashMap<Symbol, Value>,
    #[cfg(feature = "state")]
    state: Rc<ErasedSet>,
}

impl<'a> Env {
    /// Create a new, empty environment
    pub fn new() -> Self {
        Self {
            parent: None,
            entries: HashMap::new(),
            #[cfg(feature = "state")]
            state: Rc::new(ErasedSet::new()),
        }
    }

    /// Create a new environment extending the given environment
    pub fn extend(parent: Rc<RefCell<Env>>) -> Self {
        #[cfg(feature = "state")]
        let state = parent.borrow().state.clone();
        Self {
            entries: HashMap::new(),
            parent: Some(parent),
            #[cfg(feature = "state")]
            state,
        }
    }

    /// Gets the singleton instance of `T` from the environment state if
    /// it's already set.
    #[cfg(feature = "state")]
    pub fn get_state<T: 'a + 'static>(&'a self) -> Option<&'a T> {
        self.state.get::<T>()
    }

    /// Sets (or replaces) the singleton instance of `T` from the environment
    /// state. **This will panic if the `Env` instance is currently being shared
    /// or extended.** Use `try_set_state()` instead if this is a concern.
    #[cfg(feature = "state")]
    pub fn set_state<T: 'static>(&mut self, v: T) {
        Rc::get_mut(&mut self.state).unwrap().insert(v);
    }

    /// Sets (or replaces) the singleton instance of `T` from the environment
    /// state. If the `Env` is being shared or has a child state, returns `false`.
    #[cfg(feature = "state")]
    pub fn try_set_state<T: 'static>(&mut self, v: T) -> bool {
        Rc::get_mut(&mut self.state)
            .and_then(|s| s.insert(v))
            .is_some()
    }

    /// Walks up the environment hierarchy until it finds the symbol's value or
    /// runs out of environments.
    pub fn get(&self, key: &Symbol) -> Option<Value> {
        if let Some(val) = self.entries.get(&key) {
            Some(val.clone()) // clone the Rc
        } else if let Some(parent) = &self.parent {
            parent.borrow().get(key)
        } else {
            None
        }
    }

    /// Define a new key in the current environment
    pub fn define(&mut self, key: Symbol, value: Value) {
        self.entries.insert(key, value);
    }

    /// Find the environment where this key is defined, and update its value.
    /// Returns an Err if the symbol has not been defined anywhere in the hierarchy.
    pub fn set(&mut self, key: Symbol, value: Value) -> Result<(), RuntimeError> {
        if self.entries.contains_key(&key) {
            self.entries.insert(key, value);
            Ok(())
        } else if let Some(parent) = &self.parent {
            parent.borrow_mut().set(key, value)
        } else {
            Err(RuntimeError {
                msg: format!("Tried to set value of undefined symbol \"{}\"", key),
            })
        }
    }

    /// Delete the nearest (going upwards) definition of this key
    pub fn undefine(&mut self, key: &Symbol) {
        if self.entries.contains_key(key) {
            self.entries.remove(key);
        } else if let Some(parent) = &self.parent {
            parent.borrow_mut().undefine(key);
        }
    }

    fn display_recursive(&self, output: &mut String, depth: i32) {
        let indent = &(0..depth).map(|_| "  ").collect::<String>();

        output.push_str(indent);
        output.push_str("{ ");

        for (symbol, value) in &self.entries {
            output.push_str(format!("\n{}  {}: {}", indent, symbol, value).as_str());
        }

        if let Some(parent) = &self.parent {
            output.push_str("\n\n");
            parent
                .as_ref()
                .borrow()
                .display_recursive(output, depth + 1);
        }

        output.push('\n');
        output.push_str(indent);
        output.push('}');
    }
}

impl std::fmt::Display for Env {
    fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut output = String::new();

        output.push_str("Env: ");
        self.display_recursive(&mut output, 0);

        write!(formatter, "{}", &output)
    }
}
