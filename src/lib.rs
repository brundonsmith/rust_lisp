
mod interpreter;
mod parser;
mod default_environment;

pub use parser::parse;
pub use interpreter::eval;
pub use default_environment::default_env;

pub mod model;
pub mod utils;