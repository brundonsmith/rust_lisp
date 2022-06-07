/**
 * A String [newtype](https://rust-unofficial.github.io/patterns/patterns/behavioural/newtype.html)
 * representing a lisp symbol (identifier)
 */
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Symbol(pub String);

impl From<&str> for Symbol {
    fn from(s: &str) -> Self {
        Symbol(String::from(s))
    }
}

impl std::fmt::Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.0, f)
    }
}
