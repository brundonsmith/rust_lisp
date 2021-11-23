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
        Value::Symbol(String::from(stringify!( $sym )))
    };
    // these aren't valid Rust identifiers
    ( + ) =>  { Value::Symbol(String::from("+")) };
    ( - ) =>  { Value::Symbol(String::from("-")) };
    ( * ) =>  { Value::Symbol(String::from("*")) };
    ( / ) =>  { Value::Symbol(String::from("/")) };
    ( == ) => { Value::Symbol(String::from("==")) };
    ( != ) => { Value::Symbol(String::from("!=")) };
    ( < ) =>  { Value::Symbol(String::from("<")) };
    ( <= ) => { Value::Symbol(String::from("<=")) };
    ( > ) =>  { Value::Symbol(String::from(">")) };
    ( >= ) => { Value::Symbol(String::from(">=")) };


    // Literals
    ($e:literal) => {
        // HACK: Macros don't have a good way to
        // distinguish different kinds of literals,
        // so we just kick those out to be parsed
        // at runtime.
        parse(stringify!($e)).next().unwrap().unwrap()
    };


    // Special atoms
    (Nil) => { Value::NIL };
    (T) =>   { Value::T   };
    (F) =>   { Value::F   };
}
