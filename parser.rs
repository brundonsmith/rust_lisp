
use crate::model::{Atom, Expression, ConsCell, Number, NIL};
use std::rc::Rc;

#[derive(Debug,Clone)]
enum ParseTree {
  Atom(Atom),
  List(Vec<ParseTree>),
}

impl ParseTree {

  pub fn into_expression(&self) -> Rc<Expression> {
    match self {
      ParseTree::Atom(atom) => Rc::new(Expression::Atom(atom.clone())),
      ParseTree::List(vec) => {
        let mut cons: Option<ConsCell> = None;

        for subtree in vec.iter().rev() {
          // println!("{:?}", cons);
          
          cons = Some(ConsCell {
            car: subtree.into_expression(),
            cdr: cons.map(|cons_cell| Rc::new(cons_cell)),
          });
        }

        match cons {
          Some(cons) => Rc::new(Expression::List(Rc::new(cons))),
          None => Rc::new(NIL)
        }
      }
    }
  }

}

// parsing
fn tokenize(code: &str) -> Vec<String> {
  let replaced = code.replace("(", " ( ");
  let replaced = replaced.replace(")", " ) ");

  return replaced.split_whitespace().map(|s| String::from(s)).collect();
}

fn read(tokens: &Vec<String>) -> Rc<Expression> {
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
    _ => ParseTree::Atom(Atom::Nil)
  };

  // println!("{:?}", &parse_tree);

  return parse_tree.into_expression();
}


fn read_atom(token: &str) -> Atom {
  token.parse::<i32>().map(|i| Atom::Number(Number::Int(i))).unwrap_or(
  token.parse::<f32>().map(|f| Atom::Number(Number::Float(f))).unwrap_or(
                               Atom::Symbol(String::from(token))))
}

pub fn parse(code: &str) -> Rc<Expression> {
  read(&tokenize(code))
}


