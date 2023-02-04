use std::fmt::Debug;
use std::fmt::Display;
use std::iter::FromIterator;

use super::reference;
use super::reference::Reference;
use super::{RuntimeError, Value};

/**
 * A Lisp list, implemented as a linked-list
 */
#[derive(Debug, Clone)]
pub struct List {
    head: Option<Reference<ConsCell>>,
}

impl PartialEq for List {
    fn eq(&self, other: &Self) -> bool {
        match (&self.head, &other.head) {
            (None, None) => true,
            (Some(this), Some(other)) => reference::eq(this, other),
            _ => false,
        }
    }
}

impl Eq for List {}

impl List {
    pub const NIL: List = List { head: None };

    pub fn is_nil(&self) -> bool {
        matches!(self.head, None)
    }

    pub fn car(&self) -> Result<Value, RuntimeError> {
        self.head
            .as_ref()
            .map(|rc| reference::borrow(rc).car.clone())
            .ok_or_else(|| RuntimeError {
                msg: String::from("Attempted to apply car on nil"),
            })
    }
    #[must_use]
    pub fn cdr(&self) -> List {
        List {
            head: self
                .head
                .as_ref()
                .and_then(|rc| reference::borrow(rc).cdr.as_ref().cloned()),
        }
    }

    #[must_use]
    pub fn cons(&self, val: Value) -> List {
        List {
            head: Some(reference::new(ConsCell {
                car: val,
                cdr: self.head.clone(),
            })),
        }
    }
}

impl<'a> List {
    pub fn into_iter(list: &'a List) -> ConsIterator {
        ConsIterator(list.head.clone())
    }
}

/// A `ConsCell` is effectively a linked-list node, where the value in each node
/// is a lisp `Value`. To be used as a true "list", the ConsCell must be wrapped
/// in Value::List().
#[derive(Debug)]
struct ConsCell {
    pub car: Value,
    pub cdr: Option<Reference<ConsCell>>,
}

impl PartialEq for ConsCell {
    fn eq(&self, other: &Self) -> bool {
        self.car == other.car
            && match (&self.cdr, &other.cdr) {
                (None, None) => true,
                (Some(this), Some(other)) => reference::eq(this, other),
                _ => false,
            }
    }
}

impl Eq for ConsCell {}

impl std::hash::Hash for ConsCell {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.car.hash(state);
        self.cdr.as_ref().map(reference::as_ptr).hash(state);
    }
}

impl Display for List {
    fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        if let Some(head) = &self.head {
            write!(formatter, "({})", reference::borrow(head))
        } else {
            write!(formatter, "NIL")
        }
    }
}

impl Display for ConsCell {
    fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.cdr.as_ref() {
            Some(cdr) => write!(formatter, "{} {}", self.car, reference::borrow(cdr)),
            None => write!(formatter, "{}", self.car),
        }
    }
}

impl std::hash::Hash for List {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.head.as_ref().map(reference::as_ptr).hash(state);
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
pub struct ConsIterator(Option<Reference<ConsCell>>);

impl Iterator for ConsIterator {
    type Item = Value;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.clone().map(|cons| {
            let val = reference::borrow(&cons).car.clone();

            self.0 = reference::borrow(&cons).cdr.clone();

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
        let mut tail: Option<Reference<ConsCell>> = None;

        for val in iter {
            // The cons cell for the current value
            let new_cons = reference::new(ConsCell {
                car: val,
                cdr: None,
            });

            // if this is the first cell, put it in the List
            if new_list.head.is_none() {
                new_list.head = Some(new_cons.clone());
            // otherwise, put it in the current tail cell
            } else if let Some(tail_cons) = tail {
                reference::borrow_mut(&tail_cons).cdr = Some(new_cons.clone());
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
