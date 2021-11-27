mod default_environment;
mod interpreter;
mod parser;

pub use default_environment::default_env;
pub use interpreter::{eval, eval_block};
pub use parser::parse;

pub mod model;
pub mod utils;
#[macro_use]
pub mod macros;

use model::Env;
use std::io::Write;
use std::{cell::RefCell, io, rc::Rc};


// 🦀 I am all over this project!
/// Starts a REPL prompt at stdin/stdout. **This will block the current thread.**
pub fn start_repl(env: Option<Env>) {
    let env_rc = Rc::new(RefCell::new(env.unwrap_or_else(default_env)));

    loop {
        print!("> ");
        io::stdout().flush().unwrap();

        let mut buf = String::new();
        io::stdin().read_line(&mut buf).unwrap();

        let res = eval_block(env_rc.clone(), parse(&buf).filter_map(|a| a.ok()));

        match res {
            Ok(val) => println!("{}", val),
            Err(e) => println!("{}", e),
        };
    }
}
