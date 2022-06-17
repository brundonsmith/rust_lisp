/// A macro for more easily creating s-expressions from within Rust code
/// ```ignore
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
        $crate::model::Value::List([ $(lisp!{ $val }),* ].iter().collect::<$crate::model::List>())
    };


    // Symbols
    ($sym:ident) => {
        $crate::model::Value::Symbol($crate::model::Symbol(String::from(stringify!( $sym ))))
    };
    // these aren't valid Rust identifiers
    ( + ) =>  { $crate::model::Value::Symbol($crate::model::Symbol(String::from("+"))) };
    ( - ) =>  { $crate::model::Value::Symbol($crate::model::Symbol(String::from("-"))) };
    ( * ) =>  { $crate::model::Value::Symbol($crate::model::Symbol(String::from("*"))) };
    ( / ) =>  { $crate::model::Value::Symbol($crate::model::Symbol(String::from("/"))) };
    ( == ) => { $crate::model::Value::Symbol($crate::model::Symbol(String::from("=="))) };
    ( != ) => { $crate::model::Value::Symbol($crate::model::Symbol(String::from("!="))) };
    ( < ) =>  { $crate::model::Value::Symbol($crate::model::Symbol(String::from("<"))) };
    ( <= ) => { $crate::model::Value::Symbol($crate::model::Symbol(String::from("<="))) };
    ( > ) =>  { $crate::model::Value::Symbol($crate::model::Symbol(String::from(">"))) };
    ( >= ) => { $crate::model::Value::Symbol($crate::model::Symbol(String::from(">="))) };


    // Literals
    ($e:literal) => {
        // HACK: Macros don't have a good way to
        // distinguish different kinds of literals,
        // so we just kick those out to be parsed
        // at runtime.
        $crate::parser::parse(stringify!($e)).next().unwrap().unwrap()
    };


    // 🦀 Very special!
    // Special atoms
    (Nil) => { $crate::model::Value::NIL };
    (T) =>   { $crate::model::Value::T   };
    (F) =>   { $crate::model::Value::F   };
}
