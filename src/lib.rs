#![forbid(unsafe_code)]

pub mod interpreter;
pub mod model;
pub mod parser;
pub mod utils;

mod default_environment;
pub use default_environment::default_env;

#[macro_use]
mod macros;

use model::Env;
use std::io::{self, prelude::*};

use crate::model::reference;

// 🦀 I am all over this project!
/// Starts a REPL prompt at stdin/stdout. **This will block the current thread.**
pub fn start_repl(env: Option<Env>) {
    let env_rc = reference::new(env.unwrap_or_else(default_env));

    print!("> ");
    io::stdout().flush().unwrap();
    for line in io::stdin().lock().lines() {
        match interpreter::eval_block(
            env_rc.clone(),
            parser::parse(&line.unwrap()).filter_map(|a| a.ok()),
        ) {
            Ok(val) => println!("{}", val),
            Err(e) => println!("{}", e),
        };

        print!("> ");
        io::stdout().flush().unwrap();
    }

    // Properly go to the next line after quitting
    println!();
}
