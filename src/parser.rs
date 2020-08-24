
use crate::model::{Value};
use crate::utils::vec_to_cons;

// A slightly more convenient data structure for building the parse tree, before
// eventually converting it into proper s-expressions.
#[derive(Debug,Clone)]
enum ParseTree {
  Atom{atom: Value, quoted: bool},
  List{vec: Vec<ParseTree>, quoted: bool},
}

impl ParseTree {

  pub fn into_expression(self) -> Value {
    match self {
      ParseTree::Atom{atom, quoted} => 
        if quoted {
          vec_to_cons(&vec![ Value::Symbol(String::from("quote")), atom ])
        } else {
          atom
        },
      ParseTree::List{vec, quoted} =>
        if quoted {
          vec_to_cons(&vec![ 
            Value::Symbol(String::from("quote")), 
            vec_to_cons(
              &vec.into_iter()
                    .map(|parse_tree| parse_tree.into_expression())
                    .collect())
          ])
        } else {
          vec_to_cons(
            &vec.into_iter()
                  .map(|parse_tree| parse_tree.into_expression())
                  .collect())
        }
    }
  }

}

const SPECIAL_TOKENS: [&str;4] = [ "(", ")", ";;", "'" ];

// Tokenize Lisp code
fn tokenize(code: &str) -> impl Iterator<Item=String> {
  let total_chars = code.chars().count();

  let mut tokens: Vec<String> = vec![];
  let mut index = 0;
  let mut current_atom = String::new();
  'charloop: while index < total_chars {
    
    // match known tokens
    for token in SPECIAL_TOKENS.iter() {
      let matched = code.chars().skip(index).zip(token.chars()).all(|(code_char, token_char)| code_char == token_char);

      if matched {
        if current_atom.len() > 0 {
          tokens.push(current_atom);
          current_atom = String::new();
        }

        index += token.chars().count();

        if *token == ";;" {
          while code.chars().nth(index).map_or(false, |c| c != '\n') {
            index += 1;
          }
        } else {
          tokens.push(String::from(*token));
        }

        continue 'charloop;
      }
    }

    // if no exact token found
    let mut remaining = code.chars().skip(index).peekable();
    if remaining.peek().map_or(false, |c| *c == '"') { // string
      let mut ch = remaining.next();
      let mut new_str = ch.unwrap().to_string();

      index += 1;
      ch = remaining.next();
      while ch.map_or(false, |c| c != '"') {
        new_str.push(ch.unwrap());
        index += 1;
        ch = remaining.next();
      }

      new_str.push_str("\"");
      index += 1;

      if current_atom.len() > 0 {
        tokens.push(current_atom);
        current_atom = String::new();
      }

      tokens.push(new_str);

    } else if remaining.peek().map_or(false, |c| c.is_whitespace()) { // whitespace
      if current_atom.len() > 0 {
        tokens.push(current_atom);
        current_atom = String::new();
      }

      while remaining.next().map_or(false, |c| c.is_whitespace()) {
        index += 1;
      }
    } else { // symbol
      current_atom.push(remaining.next().unwrap());
      index += 1;
    }
  }

  return tokens.into_iter();
}

// Parse tokens (created by `tokenize()`) into a series of s-expressions. There
// are more than one when the base string has more than one independent 
// parenthesized lists at its root.
fn read(tokens: impl Iterator<Item=String>) -> Result<Vec<Value>,ParseError> {
  let mut stack: Vec<ParseTree> = vec![ ParseTree::List{vec: vec![], quoted: false} ];
  let mut parenths = 0;
  let mut quote_next = false;

  for token in tokens {
    match token.as_str() {
      "(" => {
        parenths += 1;

        stack.push(ParseTree::List{vec: vec![], quoted: quote_next});
        quote_next = false;
      },
      ")" => {
        parenths -= 1;
  
        if stack.len() == 0 {
          return Err(ParseError {
            msg: format!("Unexpected ')'")
          });
        } else {
          let mut finished = stack.pop().unwrap();
          let destination = stack.last_mut().unwrap();
  
          // () is Nil
          if let ParseTree::List{vec, quoted} = &finished {
            if vec.len() == 0 {
              finished = ParseTree::Atom{ atom: Value::Nil, quoted: *quoted };
            }
          }
  
          match destination {
            ParseTree::List{vec, quoted: _} => vec.push(finished),
            _ => ()
          };
        }
      },
      "'" => {
        quote_next = true;
      },
      _ => {  // atom
        let expr = ParseTree::Atom{ atom: read_atom(&token), quoted: quote_next };
        quote_next = false;

        if let ParseTree::List{vec, quoted: _} = stack.last_mut().unwrap() {
          vec.push(expr);
        }
      }
    };
  }

  if parenths > 0 {
    return Err(ParseError {
      msg: format!("{} unclosed parenths", parenths)
    });
  } else if parenths < 0 {
    return Err(ParseError {
      msg: format!("{} too many closing parenths", parenths)
    });
  }

  let parse_trees = match stack.into_iter().last().unwrap() {
    ParseTree::List{vec, quoted: _} => vec,
    _ => vec![ ParseTree::Atom{ atom: Value::Nil, quoted: false } ]
  };

  return Ok(parse_trees.into_iter().map(|t| t.into_expression()).collect());
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
    return Value::Nil;
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
  && token.chars().nth_back(0).map_or(false, |c| c == '"') {
    return Value::String(String::from(&token[1..token.chars().count() - 1]))
  }

  return Value::Symbol(String::from(token));
}

/// Parse a string of Lisp code into a series of s-expressions. There
/// are more than one expressions when the base string has more than one 
/// independent parenthesized lists at its root.
pub fn parse(code: &str) -> Result<Vec<Value>,ParseError> {
  read(tokenize(code))
}

#[derive(Debug)]
pub struct ParseError {
  pub msg: String,
  // pub line: i32,
}
