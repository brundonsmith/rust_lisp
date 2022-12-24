use std::{any::Any, rc::Rc};

use crate::model::{FloatType, HashMapRc, IntType, List, RuntimeError, Symbol, Value};

/// Given a `Value` assumed to be a `Value::List()`, grab the item at `index`
/// and err if there isn't one.
pub fn require_arg<'a>(
    func_or_form_name: &str,
    args: &'a [Value],
    index: usize,
) -> Result<&'a Value, RuntimeError> {
    args.get(index).ok_or_else(|| RuntimeError {
        msg: format!(
            "\"{}\" requires an argument {}",
            func_or_form_name,
            index + 1
        ),
    })
}

/// Given a `Value` assumed to be a `Value::List()`, and some type T, grab the
/// item at `index` in the list and try converting it to type T. RuntimeError if
/// the argument doesn't exist, or if it is the wrong type.
pub fn require_typed_arg<'a, T>(
    func_or_form_name: &str,
    args: &'a [Value],
    index: usize,
) -> Result<T, RuntimeError>
where
    T: TryFrom<&'a Value> + TypeName,
{
    require_arg(func_or_form_name, args, index)?
        .try_into()
        .map_err(|_| RuntimeError {
            msg: format!(
                "\"{}\" requires argument {} to be a {}; got {}",
                func_or_form_name,
                index + 1,
                T::get_name(),
                args.get(index).unwrap_or(&Value::NIL)
            ),
        })
}

pub trait TypeName {
    fn get_name() -> &'static str;
}

impl TypeName for IntType {
    fn get_name() -> &'static str {
        "int"
    }
}

impl TypeName for FloatType {
    fn get_name() -> &'static str {
        "float"
    }
}

impl TypeName for &String {
    fn get_name() -> &'static str {
        "string"
    }
}

impl TypeName for &Symbol {
    fn get_name() -> &'static str {
        "symbol"
    }
}

impl TypeName for &List {
    fn get_name() -> &'static str {
        "list"
    }
}

impl TypeName for &HashMapRc {
    fn get_name() -> &'static str {
        "hash map"
    }
}

impl TypeName for &Rc<dyn Any> {
    fn get_name() -> &'static str {
        "foreign value"
    }
}
