use crate::{
    lisp,
    model::{List, Value},
};
use std::fmt::Display;

/// A slightly more convenient data structure for building the parse tree, before
/// eventually converting it into proper s-expressions.
#[derive(Debug, Clone)]
enum ParseTree {
    Atom { atom: Value, quoted: bool },
    List { vec: Vec<ParseTree>, quoted: bool },
}

impl ParseTree {
    pub fn into_expression(self) -> Value {
        match self {
            ParseTree::Atom { atom, quoted } => {
                if quoted {
                    lisp! { (quote {atom}) }
                } else {
                    atom
                }
            }
            ParseTree::List { vec, quoted } => {
                let list = Value::List(
                    vec.into_iter()
                        .map(|parse_tree| parse_tree.into_expression())
                        .collect::<List>(),
                );

                if quoted {
                    lisp! { (quote {list}) }
                } else {
                    list
                }
            }
        }
    }
}

/// Tokenize Lisp code
fn tokenize<'a>(code: &'a str) -> impl Iterator<Item = &'a str> {
    let mut skip_to: Option<usize> = None;

    code.char_indices().filter_map(move |(index, ch)| {
        if skip_to
            .map(|destination| destination > index)
            .unwrap_or(false)
        {
            return None;
        } else {
            skip_to = None;
        }

        // whitespace
        if ch.is_whitespace() {
            return None;
        }

        // special tokens
        for special in &SPECIAL_TOKENS {
            if match_front(&code[index..], special) {
                skip_to = Some(index + special.len());
                return Some(*special);
            }
        }

        // strings
        if ch == '"' {
            let match_end = match_pred(&code[index + 1..], |c| c != '"');

            if let Some(contents_end_index) = match_end {
                let string_end_index = index + contents_end_index + 3;

                skip_to = Some(string_end_index);
                return Some(&code[index..string_end_index]);
            }
        }

        // comments
        if ch == ';' && code[index + 1..].chars().next().map_or(false, |c| c == ';') {
            let comment_end =
                index + 2 + match_pred(&code[index + 2..], |c| c != '\n').unwrap_or(0);

            skip_to = Some(comment_end + 1);
            return None;
        }

        // numbers
        if ch.is_numeric() {
            let front_end = index + match_pred(&code[index..], |c| c.is_numeric()).unwrap() + 1;

            if front_end < code.len() - 1 && &code[front_end..front_end + 1] == "." {
                let back_end = front_end
                    + 1
                    + match_pred(&code[front_end + 1..], |c| c.is_numeric()).unwrap()
                    + 1;

                skip_to = Some(back_end);
                return Some(&code[index..back_end]);
            } else {
                skip_to = Some(front_end);
                return Some(&code[index..front_end]);
            }
        }

        // symbols
        if is_symbol_start(ch) {
            let match_end = match_pred(&code[index..], is_symbolic);

            if let Some(symbol_last) = match_end {
                let symbol_end = index + symbol_last + 1;

                skip_to = Some(symbol_end);
                return Some(&code[index..symbol_end]);
            }
        }

        return None;
    })
}

const SPECIAL_TOKENS: [&str; 4] = ["(", ")", "'", "..."];

fn is_symbol_start(c: char) -> bool {
    !c.is_numeric()
        && !c.is_whitespace()
        && !SPECIAL_TOKENS
            .iter()
            .any(|t| t.chars().any(|other| other == c))
}

fn is_symbolic(c: char) -> bool {
    !c.is_whitespace()
        && !SPECIAL_TOKENS
            .iter()
            .any(|t| t.chars().any(|other| other == c))
}

fn match_front(code: &str, segment: &str) -> bool {
    segment.chars().zip(code.chars()).all(|(a, b)| a == b)
}

fn match_pred<F: Fn(char) -> bool>(code: &str, pred: F) -> Option<usize> {
    code.char_indices()
        .take_while(|(_, c)| pred(*c))
        .last()
        .map(|(i, _)| i)
}

#[test]
fn tokenize_simplest() {
    let source = "
  (1 2 3)";
    let tokens: Vec<&str> = tokenize(source).collect();

    assert_eq!(tokens, vec!["(", "1", "2", "3", ")"]);
}

#[test]
fn tokenize_basic_expression() {
    let source = "
  (list 
    (* 1 2)  ;; a comment
    (/ 6 3 \"foo\"))";
    let tokens: Vec<&str> = tokenize(source).collect();

    assert_eq!(
        tokens,
        vec!["(", "list", "(", "*", "1", "2", ")", "(", "/", "6", "3", "\"foo\"", ")", ")"]
    );
}

#[test]
fn tokenize_complex_expression() {
    let source = "
  (begin
    (define fib-normal
      (lambda (n)
        (+ (fib (- n 1)) (fib (- n 2)) )))

    (define fib 
      (lambda (n)
        (cond           ;; some comment
          ((== n 0) 0)
          ((== n 1) 1)
          (T (fib-normal n))))) ;;another comment";

    let tokens: Vec<&str> = tokenize(source).collect();

    assert_eq!(
        tokens,
        vec![
            "(",
            "begin",
            "(",
            "define",
            "fib-normal",
            "(",
            "lambda",
            "(",
            "n",
            ")",
            "(",
            "+",
            "(",
            "fib",
            "(",
            "-",
            "n",
            "1",
            ")",
            ")",
            "(",
            "fib",
            "(",
            "-",
            "n",
            "2",
            ")",
            ")",
            ")",
            ")",
            ")",
            "(",
            "define",
            "fib",
            "(",
            "lambda",
            "(",
            "n",
            ")",
            "(",
            "cond",
            "(",
            "(",
            "==",
            "n",
            "0",
            ")",
            "0",
            ")",
            "(",
            "(",
            "==",
            "n",
            "1",
            ")",
            "1",
            ")",
            "(",
            "T",
            "(",
            "fib-normal",
            "n",
            ")",
            ")",
            ")",
            ")",
            ")"
        ]
    );
}

#[test]
fn tokenize_identifier_with_digits() {
    let tokens: Vec<&str> = tokenize("(i32)").collect();

    assert_eq!(tokens, vec!["(", "i32", ")"]);
}

/// Parse tokens (created by `tokenize()`) into a series of s-expressions. There
/// are more than one when the base string has more than one independent
/// parenthesized lists at its root.
fn read<'a>(
    tokens: impl Iterator<Item = &'a str> + 'a,
) -> impl Iterator<Item = Result<Value, ParseError>> + 'a {
    let mut stack: Vec<ParseTree> = vec![];
    let mut parenths = 0;
    let mut quote_next = false;

    tokens.filter_map(move |token| {
        match token {
            "(" => {
                parenths += 1;

                stack.push(ParseTree::List {
                    vec: vec![],
                    quoted: quote_next,
                });
                quote_next = false;

                None
            }
            ")" => {
                parenths -= 1;

                if stack.len() == 0 {
                    Some(Err(ParseError {
                        msg: format!("Unexpected ')'"),
                    }))
                } else {
                    let mut finished = stack.pop().unwrap();

                    if parenths == 0 {
                        stack = vec![];
                        Some(Ok(finished.into_expression()))
                    } else {
                        let destination = stack.last_mut().unwrap();

                        // () is Nil
                        if let ParseTree::List { vec, quoted } = &finished {
                            if vec.len() == 0 {
                                finished = ParseTree::Atom {
                                    atom: Value::NIL,
                                    quoted: *quoted,
                                };
                            }
                        }

                        if let ParseTree::List { vec, quoted: _ } = destination {
                            vec.push(finished);
                        }

                        None
                    }
                }
            }
            "'" => {
                quote_next = true;

                None
            }
            _ => {
                // atom
                let expr = ParseTree::Atom {
                    atom: read_atom(&token),
                    quoted: quote_next,
                };
                quote_next = false;

                if stack.len() > 0 {
                    if let ParseTree::List { vec, quoted: _ } = stack.last_mut().unwrap() {
                        vec.push(expr);
                    }
                    None
                } else {
                    Some(Ok(expr.into_expression()))
                }
            }
        }
    })
}

fn read_atom(token: &str) -> Value {
    let token_lowercase = token.to_lowercase();

    if token_lowercase == "t" {
        return Value::True;
    }

    if token_lowercase == "f" {
        return Value::False;
    }

    if token_lowercase == "nil" {
        return Value::NIL;
    }

    let as_int = token.parse::<i32>();
    if as_int.is_ok() {
        return Value::Int(as_int.unwrap());
    }

    let as_float = token.parse::<f32>();
    if as_float.is_ok() {
        return Value::Float(as_float.unwrap());
    }

    if token.chars().next().map_or(false, |c| c == '"')
        && token.chars().nth_back(0).map_or(false, |c| c == '"')
    {
        return Value::String(String::from(&token[1..token.chars().count() - 1]));
    }

    return Value::Symbol(String::from(token));
}

/// Parse a string of Lisp code into a series of s-expressions. There
/// are more than one expressions when the base string has more than one
/// independent parenthesized lists at its root.
pub fn parse(code: &str) -> impl Iterator<Item = Result<Value, ParseError>> + '_ {
    read(tokenize(code))
}

#[derive(Debug)]
pub struct ParseError {
    pub msg: String,
    // pub line: i32,
}

impl Display for ParseError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        return write!(formatter, "Parse error: {}", self.msg);
    }
}
