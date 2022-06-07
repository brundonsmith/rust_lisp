use crate::{
    lisp,
    model::{FloatType, IntType, List, Symbol, Value},
};

use std::fmt::Display;

/// Parse a string of Lisp code into a series of s-expressions. There
/// are more than one expressions when the base string has more than one
/// independent parenthesized lists at its root.
pub fn parse(code: &str) -> impl Iterator<Item = Result<Value, ParseError>> + '_ {
    let mut index = 0;
    index = consume_whitespace_and_comments(code, index);

    std::iter::from_fn(move || {
        if let Some(res) = parse_expression(code, index) {
            if let Ok(res) = res {
                index = res.index;
                index = consume_whitespace_and_comments(code, index);

                Some(Ok(res.parsed.into_value()))
            } else {
                Some(Err(res.unwrap_err()))
            }
        } else {
            None // TODO: Err if we don't parse the whole input?
        }
    })
}

/// A slightly more convenient data structure for building the parse tree, before
/// eventually converting it into proper s-expressions.
#[derive(Debug, Clone)]
enum ParseTree {
    Atom(Value),
    List(Vec<ParseTree>),
    Quoted(Box<ParseTree>),
}

impl ParseTree {
    pub fn into_value(self) -> Value {
        match self {
            ParseTree::Atom(value) => value,
            ParseTree::List(vec) => Value::List(
                vec.into_iter()
                    .map(|parse_tree| parse_tree.into_value())
                    .collect::<List>(),
            ),
            ParseTree::Quoted(inner) => lisp! { (quote {inner.into_value()}) },
        }
    }
}

/**
 * An error that occurred while parsing a string as lisp code
 */
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseError {
    pub msg: String,
    // pub line: i32,
}

impl Display for ParseError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        return write!(formatter, "Parse error: {}", self.msg);
    }
}

#[derive(Clone, Debug)]
struct ParsedAndIndex {
    pub parsed: ParseTree,
    pub index: usize,
}

type ParseResult = Option<Result<ParsedAndIndex, ParseError>>;
type ConsumeResult = Option<usize>;

fn parse_expression(code: &str, index: usize) -> ParseResult {
    for func in [parse_list, parse_atom] {
        let res = func(code, index);

        if res.is_some() {
            return res;
        }
    }

    None
}

fn parse_list(code: &str, index: usize) -> ParseResult {
    let mut index = consume(code, index, "(")?;
    let mut members = vec![];

    index = consume_whitespace_and_comments(code, index);

    while let Some(res) = parse_expression(code, index) {
        if let Ok(res) = res {
            index = res.index;
            members.push(res.parsed);
            index = consume_whitespace_and_comments(code, index);
        } else {
            return Some(res);
        }
    }

    if let Some(index) = consume(code, index, ")") {
        Some(Ok(ParsedAndIndex {
            parsed: ParseTree::List(members),
            index,
        }))
    } else {
        Some(Err(ParseError {
            msg: format!("Unclosed list at index {}", index),
        }))
    }
}

fn parse_atom(code: &str, index: usize) -> ParseResult {
    for func in [
        parse_quoted,
        parse_nil,
        parse_false,
        parse_true,
        parse_number,
        parse_string,
        parse_symbol,
    ] {
        let res = func(code, index);

        if res.is_some() {
            return res;
        }
    }

    None
}

fn parse_quoted(code: &str, index: usize) -> ParseResult {
    let index = consume(code, index, "'")?;
    let res = parse_expression(code, index)?;

    if let Ok(ParsedAndIndex { parsed, index }) = res {
        Some(Ok(ParsedAndIndex {
            parsed: ParseTree::Quoted(Box::new(parsed)),
            index,
        }))
    } else {
        Some(res)
    }
}

fn parse_nil(code: &str, index: usize) -> ParseResult {
    let index = consume(code, index, "nil")?;

    if next_char_is_break(code, index) {
        Some(Ok(ParsedAndIndex {
            parsed: ParseTree::Atom(Value::NIL),
            index,
        }))
    } else {
        None
    }
}

fn parse_false(code: &str, index: usize) -> ParseResult {
    let index = consume(code, index, "f")?;

    if next_char_is_break(code, index) {
        Some(Ok(ParsedAndIndex {
            parsed: ParseTree::Atom(Value::False),
            index,
        }))
    } else {
        None
    }
}

fn parse_true(code: &str, index: usize) -> ParseResult {
    let index = consume(code, index, "t")?;

    if next_char_is_break(code, index) {
        Some(Ok(ParsedAndIndex {
            parsed: ParseTree::Atom(Value::True),
            index,
        }))
    } else {
        None
    }
}

fn parse_number(code: &str, index: usize) -> ParseResult {
    let (front_last_index, front_last_char) = consume_while(code, index, |(index, ch)| {
        (index == 0 && ch == '-') || ch.is_numeric()
    })?;

    if front_last_char.is_numeric() {
        let front_last_index = front_last_index + 1;

        let back_end = consume_while(code, front_last_index, |(index, ch)| {
            (index == 0 && ch == '.') || ch.is_numeric()
        });

        if let Some((back_last_index, _)) = back_end {
            let back_last_index = back_last_index + 1;

            if back_last_index >= front_last_index + 2 {
                if next_char_is_break(code, back_last_index) {
                    if let Ok(float) = code
                        .get(index..back_last_index)
                        .unwrap_or("")
                        .parse::<FloatType>()
                    {
                        return Some(Ok(ParsedAndIndex {
                            parsed: ParseTree::Atom(Value::Float(float)),
                            index: back_last_index,
                        }));
                    }
                }
            } else if code.as_bytes().get(back_last_index - 1) == Some(&b'.') {
                return Some(Err(ParseError {
                    msg: format!(
                        "Expected decimal value after '.' at index {}",
                        back_last_index - 1
                    ),
                }));
            }
        }

        if next_char_is_break(code, front_last_index) {
            if let Ok(int) = code
                .get(index..front_last_index)
                .unwrap_or("")
                .parse::<IntType>()
            {
                return Some(Ok(ParsedAndIndex {
                    parsed: ParseTree::Atom(Value::Int(int)),
                    index: front_last_index,
                }));
            }
        }
    }

    None
}

fn parse_string(code: &str, index: usize) -> ParseResult {
    let (last_index, _) = consume_while(code, index, |(index, ch)| {
        (index == 0 && ch == '"') || (index > 0 && ch != '"')
    })?;

    if last_index > index {
        if code.as_bytes().get(last_index + 1) == Some(&b'"') {
            Some(Ok(ParsedAndIndex {
                parsed: ParseTree::Atom(Value::String(
                    code.get(index + 1..last_index + 1).unwrap_or("").to_owned(),
                )),
                index: last_index + 2,
            }))
        } else {
            Some(Err(ParseError {
                msg: format!("Unclosed string at index {}", last_index),
            }))
        }
    } else {
        None
    }
}

fn parse_symbol(code: &str, index: usize) -> ParseResult {
    let (last_index, _) = consume_while(code, index, |(index, ch)| {
        (index == 0 && is_symbol_start(ch)) || (index > 0 && is_symbolic(ch))
    })?;
    let last_index = last_index + 1;

    if last_index > index {
        Some(Ok(ParsedAndIndex {
            parsed: ParseTree::Atom(Value::Symbol(Symbol(
                code.get(index..last_index).unwrap_or("").to_owned(),
            ))),
            index: last_index,
        }))
    } else {
        None
    }
}

fn consume(code: &str, index: usize, s: &str) -> ConsumeResult {
    let slice = code.get(index..).unwrap_or("");

    if slice.len() >= s.len()
        && slice
            .chars()
            .zip(s.chars())
            .all(|(a, b)| a.to_ascii_lowercase() == b.to_ascii_lowercase())
    {
        return Some(index + s.len());
    } else {
        return None;
    }
}

fn consume_whitespace_and_comments(code: &str, index: usize) -> usize {
    let mut semicolons = 0;

    consume_while(code, index, move |(_, ch)| {
        if ch == ';' {
            semicolons += 1;
        } else if semicolons < 2 || ch == '\n' {
            semicolons = 0;
        }

        return ch.is_whitespace() || ch == ';' || semicolons >= 2;
    })
    .map(|(index, _)| index + 1)
    .unwrap_or(index)
}

fn consume_while<F: FnMut((usize, char)) -> bool>(
    code: &str,
    index: usize,
    mut pred: F,
) -> Option<(usize, char)> {
    code.get(index..)
        .unwrap_or("")
        .char_indices()
        .take_while(|(i, c)| pred((*i, *c)))
        .last()
        .map(|(last_index, ch)| (last_index + index, ch))
}

fn is_symbol_start(c: char) -> bool {
    !c.is_numeric() && is_symbolic(c)
}

fn is_symbolic(c: char) -> bool {
    !c.is_whitespace() && !SPECIAL_TOKENS.iter().any(|t| *t == c)
}

fn next_char_is_break(code: &str, index: usize) -> bool {
    code.get(index..)
        .map(|s| s.chars().next())
        .flatten()
        .map(|ch| ch.is_whitespace() || SPECIAL_TOKENS.iter().any(|t| *t == ch))
        .unwrap_or(true)
}

const SPECIAL_TOKENS: [char; 4] = ['(', ')', '\'', ';'];
