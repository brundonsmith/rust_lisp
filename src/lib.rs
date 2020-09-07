
mod interpreter;
mod parser;
mod default_environment;

pub use parser::parse;
pub use interpreter::eval;
pub use default_environment::default_env;

pub mod model;
pub mod utils;
#[macro_use]
pub mod macros;

use model::Env;
use std::{rc::Rc, cell::RefCell, io};
use std::io::Write;

/// Starts a REPL prompt at stdin/stdout. **This will block the current thread.**
pub fn start_repl(env: Option<Env>) {
  let env_rc = Rc::new(RefCell::new(env.unwrap_or(default_env())));

  loop {
    print!("> ");
    io::stdout().flush().unwrap();

    let mut buf = String::new();
    io::stdin().read_line(&mut buf).unwrap();
      
    for expr in parse(&buf) {
      match expr {
        Ok(e) => {
          match eval(env_rc.clone(), &e) {
            Ok(val) => println!("{}", val),
            Err(e) => println!("{}", e),
          }
        },
        Err(e) => println!("{}", e),
      }
    }
  }
}
