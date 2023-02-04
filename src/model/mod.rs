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

cfg_if! {
    if #[cfg(feature = "arc")] {
        /// Types and utilities for working with either Rc<RefCell<>> or
        /// Arc<Mutex<>> depending on (and independently of) the `arc` crate
        /// feature
        pub mod reference {
            use std::{sync::{Arc, Mutex, MutexGuard}};

            /// Either Rc<T> or Arc<T>, depending on the `arc` feature
            pub type ImmReference<T> = Arc<T>;

            /// Either Rc<RefCell<T>> or Arc<Mutex<T>>, depending on the `arc` feature
            pub type Reference<T> = ImmReference<Mutex<T>>;

            #[inline]
            pub fn new_imm<T>(val: T) -> ImmReference<T> {
                Arc::new(val)
            }

            #[inline]
            pub fn new<T>(val: T) -> Reference<T> {
                new_imm(Mutex::new(val))
            }

            #[inline]
            pub fn replace<T>(r: &Reference<T>, new: T) -> T {
                std::mem::replace(&mut r.as_ref().lock().unwrap(), new)
            }

            #[inline]
            pub fn borrow<'a, T>(r: &'a Reference<T>) -> MutexGuard<'a, T> {
                r.as_ref().lock().unwrap()
            }

            #[inline]
            pub fn borrow_mut<'a, T: ?Sized>(r: &'a Reference<T>) -> MutexGuard<'a, T> {
                r.as_ref().lock().unwrap()
            }

            #[inline]
            pub fn ptr_eq<T>(a: &ImmReference<T>, b: &ImmReference<T>) -> bool {
                Arc::ptr_eq(a, b)
            }

            #[inline]
            pub fn as_ptr<T>(r: &ImmReference<T>) -> *const T {
                Arc::as_ptr(r)
            }

            #[inline]
            pub fn eq<T: PartialEq>(a: &Reference<T>, b: &Reference<T>) -> bool {
                ptr_eq(a, b) || borrow(a).eq(&borrow(b))
            }
        }
    } else {
        /// Types and utilities for working with either Rc<RefCell<>> or
        /// Arc<Mutex<>> depending on (and independently of) the `arc` crate
        /// feature
        pub mod reference {
            use std::{cell::{RefCell, Ref, RefMut}, rc::Rc};

            /// Either Rc<T> or Arc<T>, depending on the `arc` feature
            pub type ImmReference<T> = Rc<T>;

            /// Either Rc<RefCell<T>> or Arc<Mutex<T>>, depending on the `arc` feature
            pub type Reference<T> = ImmReference<RefCell<T>>;

            #[inline]
            pub fn new_imm<T>(val: T) -> ImmReference<T> {
                Rc::new(val)
            }

            #[inline]
            pub fn new<T>(val: T) -> Reference<T> {
                new_imm(RefCell::new(val))
            }

            #[inline]
            pub fn replace<T>(r: &Reference<T>, new: T) -> T {
                std::mem::replace(&mut r.borrow_mut(), new)
            }

            #[inline]
            pub fn borrow<'a, T>(r: &'a Reference<T>) -> Ref<'a, T> {
                r.borrow()
            }

            #[inline]
            pub fn borrow_mut<'a, T: ?Sized>(r: &'a Reference<T>) -> RefMut<'a, T> {
                r.borrow_mut()
            }

            #[inline]
            pub fn ptr_eq<T>(a: &ImmReference<T>, b: &ImmReference<T>) -> bool {
                Rc::ptr_eq(a, b)
            }

            #[inline]
            pub fn as_ptr<T>(r: &ImmReference<T>) -> *const T {
                Rc::as_ptr(r)
            }

            #[inline]
            pub fn eq<T: PartialEq>(a: &Reference<T>, b: &Reference<T>) -> bool {
                ptr_eq(a, b) || borrow(a).eq(&borrow(b))
            }
        }
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
pub use value::{HashMapReference, NativeFunc, Value};
