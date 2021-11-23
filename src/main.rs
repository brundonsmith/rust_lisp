#![allow(dead_code, unused_macros)]

mod default_environment;
mod interpreter;
mod model;
mod parser;
mod utils;
#[macro_use]
mod macros;

use std::{cell::RefCell, rc::Rc};

use rust_lisp::{default_env, eval_block, parse, start_repl};

fn main() {
    match std::env::args().nth(1) {
        Some(code) => {
            let env_rc = Rc::new(RefCell::new(default_env()));

            println!(
                "{}",
                eval_block(env_rc.clone(), parse(&code).filter_map(|a| a.ok().clone())).unwrap()
            );
        }
        None => start_repl(None),
    }
}
