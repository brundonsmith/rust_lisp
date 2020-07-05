
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

pub fn parse(code: &str) -> Result<Value,ParseError> {
  read(&tokenize(code))
}

#[derive(Debug)]
pub struct ParseError {
  pub msg: String,
  // pub line: i32,
}


#[cfg(test)]
mod tests {
  use crate::model::{ConsCell, Value};
  use crate::{parser::parse};
  use std::rc::Rc;

  #[test]
  fn parse_basic_expression() {
    let source = "(list 
      (* 1 2)  ;; a comment
      (/ 6 3 \"foo\"))";
    let ast = parse(source).unwrap();

    assert_eq!(ast, Value::List(Rc::new(ConsCell {
      car: Value::Symbol(String::from("list")),
      cdr: Some(Rc::new(ConsCell {
        car: Value::List(Rc::new(ConsCell {
          car: Value::Symbol(String::from("*")),
          cdr: Some(Rc::new(ConsCell {
            car: Value::Int(1),
            cdr: Some(Rc::new(ConsCell {
              car: Value::Int(2),
              cdr: None
            }))
          }))
        })),
        cdr: Some(Rc::new(ConsCell {
          car: Value::List(Rc::new(ConsCell {
            car: Value::Symbol(String::from("/")),
            cdr: Some(Rc::new(ConsCell {
              car: Value::Int(6),
              cdr: Some(Rc::new(ConsCell {
                car: Value::Int(3),
                cdr: Some(Rc::new(ConsCell {
                  car: Value::String(String::from("foo")),
                  cdr: None
                }))
              }))
            }))
          })),
          cdr: None
        }))
      }))
    })));
  }

  // #[test]
  // fn test_fib() {
  //   let source = "
  //     (begin
  //       (define fib 
  //         (lambda (n)
  //           (cond           ;; some comment
  //             ((== n 0) 0)
  //             ((== n 1) 1)
  //             (T (+ (fib (- n 1)) (fib (- n 2)) ))))) ;;another comment

  //       (list (fib 0) (fib 1) (fib 2) (fib 3) (fib 4) (fib 5) (fib 6) (fib 7) (fib 8)))";
  //   let ast = parse(source);

  //   let env = Rc::new(RefCell::new(default_env()));
  //   let result = eval(env, &ast);

  //   assert_eq!(result, vec_to_cons(&vec![ Value::Int(0), Value::Int(1), Value::Int(1), Value::Int(2), Value::Int(3), Value::Int(5), Value::Int(8), Value::Int(13), Value::Int(21) ]));
  // }

  // #[test]
  // fn test_merge_sort() {
  //   let source = "
  //     (begin
  //       (define 
  //         list-head 
  //         (lambda (lst n) 
  //           (if (== n 0) 
  //             (list) 
  //             (cons (car lst) (list-head (cdr lst) (- n 1))))))

  //       (define
  //         list-tail 
  //         (lambda (lst n) 
  //           (if (== n 0) 
  //             lst 
  //             (list-tail (cdr lst) (- n 1)))))

  //       (define 
  //         merge 
  //         (lambda (lst-a lst-b)
  //           (cond ((not lst-a) lst-b)
  //                 ((not lst-b) lst-a)
  //                 ((< (car lst-a) (car lst-b)) (cons (car lst-a) (merge (cdr lst-a) lst-b)))
  //                 (T (cons (car lst-b) (merge lst-a (cdr lst-b)))))))

  //       (define 
  //         mergesort 
  //         (lambda (lst)
  //           (if (== (length lst) 1)
  //             lst
  //             (merge (mergesort (list-head lst (truncate (length lst) 2)))
  //                   (mergesort (list-tail lst (truncate (length lst) 2)))))))

  //       (mergesort (list 7 2 5 0 1 5)))";
  //   let ast = parse(source);

  //   let env = Rc::new(RefCell::new(default_env()));
  //   let result = eval(env, &ast);

  //   assert_eq!(result, vec_to_cons(&vec![ Value::Int(0), Value::Int(1), Value::Int(2), Value::Int(5), Value::Int(5), Value::Int(7) ]));
  // }

}
