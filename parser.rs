
use crate::model::{Value};
use crate::utils::vec_to_cons;

#[derive(Debug,Clone)]
enum ParseTree {
  Atom(Value),
  List(Vec<ParseTree>),
}

impl ParseTree {

  pub fn into_expression(&self) -> Value {
    match self {
      ParseTree::Atom(atom) => atom.clone(),
      ParseTree::List(vec) => vec_to_cons(&vec.iter().map(|parse_tree| parse_tree.into_expression()).collect())
    }
  }

}

// parsing
fn tokenize(code: &str) -> Vec<String> {
  let replaced = code.replace("(", " ( ");
  let replaced = replaced.replace(")", " ) ");

  return replaced.split_whitespace().map(|s| String::from(s)).collect();
}

fn read(tokens: &Vec<String>) -> Value {
  let mut stack: Vec<ParseTree> = vec![ ParseTree::List(vec![]) ];

  for token in tokens {
    if *token == "(" {
      stack.push(ParseTree::List(vec![]));
    } else if *token == ")" {
      if stack.len() == 0 {
        panic!("Unexpected ')'");
      } else {
        let finished = stack.pop().unwrap();
        let destination = stack.last_mut().unwrap();

        match destination {
          ParseTree::List(v) => v.push(finished),
          _ => ()
        };
      }
    } else {  // atom
      match stack.last_mut().unwrap() {
        ParseTree::List(vec) => vec.push(ParseTree::Atom(read_atom(token))),
        _ => ()
      };
    }
  }

  let parse_tree = match stack.into_iter().last().unwrap() {
    ParseTree::List(vec) => vec[0].clone(),
    _ => ParseTree::Atom(Value::Nil)
  };

  return parse_tree.into_expression();
}


fn read_atom(token: &str) -> Value {
  let token_uppercase = token.to_uppercase();

  if token_uppercase == "T" {
    return Value::True;
  }

  if token_uppercase == "NIL" {
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

  if token.chars().nth(0).map_or(false, |c| c == '"') 
  && token.chars().nth_back(0).map_or(false, |c| c == '"') {
    return Value::String(String::from(&token[1..token.chars().count() - 1]))
  }

  return Value::Symbol(String::from(token));
}

pub fn parse(code: &str) -> Value {
  read(&tokenize(code))
}


