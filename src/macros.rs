/// A macro for more easily creating s-expressions from within Rust code
/// ```ignore
/// use rust_lisp::prelude::*;
///
/// fn parse_basic_expression() {
///     let ast1 = parse(
///         "
///        (+ 3 1)",
///     )
///     .next()
///     .unwrap()
///     .unwrap();
///
///     let n = 2;
///     let ast2 = lisp! {
///         (+ { Value::Int(n + 1) } 1)
///     };
///
///     assert_eq!(
///         ast1,
///         ast2
///     );
/// }
/// ```
#[allow(unused_macros)]
#[macro_export]
macro_rules! lisp {


    // Embed a Rust expression with { }
    ( { $e:expr } ) => {
        $e
    };


    // Lists
    ( ( $($val:tt)* ) ) => {
        Value::List([ $(lisp!{ $val }),* ].iter().collect::<List>())
    };


    // Symbols
    ($sym:ident) => {
        Value::Symbol(Symbol(String::from(stringify!( $sym ))))
    };
    // these aren't valid Rust identifiers
    ( + ) =>  { Value::Symbol(Symbol(String::from("+"))) };
    ( - ) =>  { Value::Symbol(Symbol(String::from("-"))) };
    ( * ) =>  { Value::Symbol(Symbol(String::from("*"))) };
    ( / ) =>  { Value::Symbol(Symbol(String::from("/"))) };
    ( == ) => { Value::Symbol(Symbol(String::from("=="))) };
    ( != ) => { Value::Symbol(Symbol(String::from("!="))) };
    ( < ) =>  { Value::Symbol(Symbol(String::from("<"))) };
    ( <= ) => { Value::Symbol(Symbol(String::from("<="))) };
    ( > ) =>  { Value::Symbol(Symbol(String::from(">"))) };
    ( >= ) => { Value::Symbol(Symbol(String::from(">="))) };


    // Literals
    ($e:literal) => {
        // HACK: Macros don't have a good way to
        // distinguish different kinds of literals,
        // so we just kick those out to be parsed
        // at runtime.
        parse(stringify!($e)).next().unwrap().unwrap()
    };


    // 🦀 Very special!
    // Special atoms
    (Nil) => { Value::NIL };
    (T) =>   { Value::T   };
    (F) =>   { Value::F   };
}

/**
 * Prelude containing key exports, including everything needed by the lisp! { } macro
 */
pub mod prelude {
    pub use crate::{
        default_environment::default_env,
        interpreter::eval,
        lisp,
        model::{Env, List, RuntimeError, Symbol, Value},
        parser::parse,
    };
}
