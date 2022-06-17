use std::fmt::Debug;

/// An error that occurred while evaluating some lisp code
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RuntimeError {
    pub msg: String,
}

impl std::fmt::Display for RuntimeError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(formatter, "Runtime error: {}", self.msg)
    }
}

impl std::error::Error for RuntimeError {
    fn description(&self) -> &str {
        &self.msg
    }
}
