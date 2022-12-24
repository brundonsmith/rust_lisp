use cfg_if::cfg_if;
cfg_if! {
    if #[cfg(feature = "bigint")] {
        use num_bigint::BigInt;
    }
}

cfg_if! {
    if      #[cfg(feature = "bigint")] { pub type IntType = BigInt; }
    else if #[cfg(feature = "i128")]   { pub type IntType = i128;   }
    else if #[cfg(feature = "i64")]    { pub type IntType = i64;    }
    else if #[cfg(feature = "i16")]    { pub type IntType = i16;    }
    else if #[cfg(feature = "i8")]     { pub type IntType = i8;     }
    else                               {
        /// The underlying type to use for storing lisp integers. Controlled via feature-flags.
        pub type IntType = i32;
    }
}

cfg_if! {
    if #[cfg(feature = "f64")] { pub type FloatType = f64; }
    else                       {
        /// The underlying type to use for storing lisp floats. Controlled via feature-flags.
        pub type FloatType = f32;
    }
}

mod env;
mod lambda;
mod list;
mod runtime_error;
mod symbol;
mod value;

pub use env::Env;
pub use lambda::Lambda;
pub use list::List;
pub use runtime_error::RuntimeError;
pub use symbol::Symbol;
pub use value::{HashMapRc, NativeFunc, Value};
