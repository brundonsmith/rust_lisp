
use crate::model::{Value};
use crate::utils::vec_to_cons;

#[derive(Debug,Clone)]
enum ParseTree {
  Atom(Value),
  List(Vec<ParseTree>),
}

impl ParseTree {

  pub fn into_expression(self) -> Value {
    match self {
      ParseTree::Atom(atom) => atom,
      ParseTree::List(vec) => 
        vec_to_cons(
          &vec.into_iter()
                .map(|parse_tree| parse_tree.into_expression())
                .collect())
    }
  }

}

// parsing
fn tokenize(code: &str) -> Vec<String> {
  let special_tokens = [ "(", ")", ";;" ];
  let total_chars = code.chars().count();

  let mut tokens: Vec<String> = vec![];
  let mut index = 0;
  let mut current_atom = String::new();
  'charloop: while index < total_chars {
    
    for token in special_tokens.iter() {
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


    // if no exact tokens found
    if code.chars().nth(index).map_or(false, |c| c == '"') { // string
      index += 1;

      let mut new_str = String::from("\"");
      let mut ch = code.chars().nth(index);
      while ch.map_or(false, |c| c != '"') {
        new_str.push(ch.unwrap());
        index += 1;
        ch = code.chars().nth(index);
      }

      new_str.push_str("\"");
      index += 1;

      if current_atom.len() > 0 {
        tokens.push(current_atom);
        current_atom = String::new();
      }

      tokens.push(new_str);

    } else if code.chars().nth(index).map_or(false, |c| c.is_whitespace()) { // whitespace
      if current_atom.len() > 0 {
        tokens.push(current_atom);
        current_atom = String::new();
      }

      while code.chars().nth(index).map_or(false, |c| c.is_whitespace()) {
        index += 1;
      }
    } else { // symbol
      current_atom.push(code.chars().nth(index).unwrap());
      index += 1;
    }
  }

  return tokens;
}

fn read(tokens: &Vec<String>) -> Result<Value,ParseError> {
  let mut stack: Vec<ParseTree> = vec![ ParseTree::List(vec![]) ];
  let mut parenths = 0;

  for token in tokens {
    if *token == "(" {
      parenths += 1;

      stack.push(ParseTree::List(vec![]));
    } else if *token == ")" {
      parenths -= 1;

      if stack.len() == 0 {
        return Err(ParseError {
          msg: format!("Unexpected ')'")
        });
      } else {
        let mut finished = stack.pop().unwrap();
        let destination = stack.last_mut().unwrap();

        // () is Nil
        if let ParseTree::List(vec) = &finished {
          if vec.len() == 0 {
            finished = ParseTree::Atom(Value::Nil);
          }
        }

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

  if parenths > 0 {
    return Err(ParseError {
      msg: format!("{} unclosed parenths", parenths)
    });
  } else if parenths < 0 {
    return Err(ParseError {
      msg: format!("{} too many closing parenths", parenths)
    });
  }

  let parse_tree = match stack.into_iter().last().unwrap() {
    ParseTree::List(vec) => vec[0].clone(),
    _ => ParseTree::Atom(Value::Nil)
  };

  return Ok(parse_tree.into_expression());
}


fn read_atom(token: &str) -> Value {
  let token_lowercase = token.to_lowercase();

  if token_lowercase == "#t" {
    return Value::True;
  }

  if token_lowercase == "#f" {
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

  if token.chars().nth(0).map_or(false, |c| c == '"') 
  && token.chars().nth_back(0).map_or(false, |c| c == '"') {
    return Value::String(String::from(&token[1..token.chars().count() - 1]))
  }

  return Value::Symbol(String::from(token));
}

pub fn parse(code: &str) -> Result<Value,ParseError> {
  read(&tokenize(code))
}

#[derive(Debug)]
pub struct ParseError {
  pub msg: String,
  // pub line: i32,
}
