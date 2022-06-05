use crate::{
    interpreter::eval,
    lisp,
    model::{Env, FloatType, List, RuntimeError, Symbol, Value},
    utils::{
        require_hash_parameter, require_int_parameter, require_list_parameter, require_parameter,
    },
};
use cfg_if::cfg_if;
use std::{cell::RefCell, collections::HashMap, convert::TryInto, rc::Rc};
cfg_if! {
    if #[cfg(feature = "bigint")] {
        use num_traits::ToPrimitive;
    } else {
        use crate::model::IntType;
    }
}

/// Initialize an instance of `Env` with several core Lisp functions implemented
/// in Rust. **Without this, you will only have access to the functions you
/// implement yourself.**
pub fn default_env() -> Env {
    let mut entries = HashMap::new();

    entries.insert(
        String::from("print"),
        Value::NativeFunc(|_env, args| {
            let expr = require_parameter("print", args, 0)?;

            println!("{}", &expr);
            Ok(expr.clone())
        }),
    );

    entries.insert(
        String::from("is-null"),
        Value::NativeFunc(|_env, args| {
            let val = require_parameter("is-null", args, 0)?;

            Ok(Value::from_truth(*val == Value::NIL))
        }),
    );

    entries.insert(
        String::from("is-number"),
        Value::NativeFunc(|_env, args| {
            let val = require_parameter("is-number", args, 0)?;

            Ok(match val {
                Value::Int(_) => Value::True,
                Value::Float(_) => Value::True,
                _ => Value::NIL,
            })
        }),
    );

    entries.insert(
        String::from("is-symbol"),
        Value::NativeFunc(|_env, args| {
            let val = require_parameter("is-symbol", args, 0)?;

            Ok(match val {
                Value::Symbol(_) => Value::True,
                _ => Value::NIL,
            })
        }),
    );

    entries.insert(
        String::from("is-boolean"),
        Value::NativeFunc(|_env, args| {
            let val = require_parameter("is-boolean", args, 0)?;

            Ok(match val {
                Value::True => Value::True,
                Value::False => Value::True,
                _ => Value::NIL,
            })
        }),
    );

    entries.insert(
        String::from("is-procedure"),
        Value::NativeFunc(|_env, args| {
            let val = require_parameter("is-procedure", args, 0)?;

            Ok(match val {
                Value::Lambda(_) => Value::True,
                Value::NativeFunc(_) => Value::True,
                _ => Value::NIL,
            })
        }),
    );

    entries.insert(
        String::from("is-pair"),
        Value::NativeFunc(|_env, args| {
            let val = require_parameter("is-pair", args, 0)?;

            Ok(match val {
                Value::List(_) => Value::True,
                _ => Value::NIL,
            })
        }),
    );

    entries.insert(
        String::from("car"),
        Value::NativeFunc(|_env, args| {
            let list = require_list_parameter("car", args, 0)?;

            list.car()
        }),
    );

    entries.insert(
        String::from("cdr"),
        Value::NativeFunc(|_env, args| {
            let list = require_list_parameter("cdr", args, 0)?;

            Ok(Value::List(list.cdr()))
        }),
    );

    entries.insert(
        String::from("cons"),
        Value::NativeFunc(|_env, args| {
            let car = require_parameter("cons", args, 0)?;
            let cdr = require_list_parameter("cons", args, 1)?;

            Ok(Value::List(cdr.cons(car.clone())))
        }),
    );

    entries.insert(
        String::from("list"),
        Value::NativeFunc(|_env, args| Ok(Value::List(args.iter().collect::<List>()))),
    );

    entries.insert(
        String::from("nth"),
        Value::NativeFunc(|_env, args| {
            let index = require_int_parameter("nth", args, 0)?;
            let list = require_list_parameter("nth", args, 1)?;

            let index = TryInto::<usize>::try_into(index).map_err(|_| RuntimeError {
                msg: "Failed converting to `usize`".to_owned(),
            })?;

            Ok(list.into_iter().nth(index).unwrap_or(Value::NIL))
        }),
    );

    entries.insert(
        String::from("sort"),
        Value::NativeFunc(|_env, args| {
            let list = require_list_parameter("sort", args, 0)?;

            let mut v: Vec<Value> = list.into_iter().collect();

            v.sort();

            Ok(Value::List(v.into_iter().collect()))
        }),
    );

    entries.insert(
        String::from("reverse"),
        Value::NativeFunc(|_env, args| {
            let list = require_list_parameter("reverse", args, 0)?;

            let mut v: Vec<Value> = list.into_iter().collect();

            v.reverse();

            Ok(Value::List(v.into_iter().collect()))
        }),
    );

    entries.insert(
        String::from("map"),
        Value::NativeFunc(|env, args| {
            let func = require_parameter("map", args, 0)?;
            let list = require_list_parameter("map", args, 1)?;

            list.into_iter()
                .map(|val| {
                    let expr = lisp! { ({func.clone()} (quote {val})) };

                    eval(env.clone(), &expr)
                })
                .collect::<Result<List, RuntimeError>>()
                .map(Value::List)
        }),
    );

    // 🦀 Oh the poor `filter`, you must feel really sad being unused.
    entries.insert(
        String::from("filter"),
        Value::NativeFunc(|env, args| {
            let func = require_parameter("filter", args, 0)?;
            let list = require_list_parameter("filter", args, 1)?;

            list.into_iter()
                .filter_map(|val: Value| -> Option<Result<Value, RuntimeError>> {
                    let expr = lisp! { ({func.clone()} (quote {val.clone()})) };

                    match eval(env.clone(), &expr) {
                        Ok(matches) => match matches.is_truthy() {
                            true => Some(Ok(val)),
                            false => None,
                        },
                        Err(e) => Some(Err(e)),
                    }
                })
                .collect::<Result<List, RuntimeError>>()
                .map(Value::List)
        }),
    );

    entries.insert(
        String::from("length"),
        Value::NativeFunc(|_env, args| {
            let list = require_list_parameter("length", args, 0)?;

            cfg_if! {
                if #[cfg(feature = "bigint")] {
                    Ok(Value::Int(list.into_iter().len().into()))
                } else {
                    Ok(Value::Int(list.into_iter().len() as IntType))
                }
            }
        }),
    );

    entries.insert(
        String::from("range"),
        Value::NativeFunc(|_env, args| {
            let start = require_int_parameter("range", args, 0)?;
            let end = require_int_parameter("range", args, 1)?;

            cfg_if! {
                if #[cfg(feature = "bigint")] {
                    let start = start.to_i128().ok_or_else(|| RuntimeError { msg: "Failed converting to `i128`".to_owned() })?;
                    let end = end.to_i128().ok_or_else(|| RuntimeError { msg: "Failed converting to `i128`".to_owned() })?;
                }
            }

            Ok(Value::List(
                (start..end).map(Value::from_int).collect::<List>(),
            ))
        }),
    );

    entries.insert(
        String::from("hash"),
        Value::NativeFunc(|_env, args| {
            let chunks = args.chunks(2);

            let mut hash = HashMap::new();

            for pair in chunks {
                let key = pair.get(0).unwrap();
                let value = pair.get(1);

                if let Some(value) = value {
                    hash.insert(key.clone(), value.clone());
                } else {
                    return Err(RuntimeError {
                        msg: format!("Must pass an even number of arguments to 'hash', because they're used as key/value pairs; found extra argument {}", key)
                    });
                }
            }

            Ok(Value::HashMap(Rc::new(RefCell::new(hash))))
        }),
    );

    entries.insert(
        String::from("hash-get"),
        Value::NativeFunc(|_env, args| {
            let hash = require_hash_parameter("hash-get", args, 0)?;
            let key = require_parameter("hash-get", args, 1)?;

            Ok(hash
                .borrow()
                .get(key)
                .map(|v| v.clone())
                .unwrap_or(Value::NIL))
        }),
    );

    entries.insert(
        String::from("hash-set"),
        Value::NativeFunc(|_env, args| {
            let hash = require_hash_parameter("hash-set", args, 0)?;
            let key = require_parameter("hash-set", args, 1)?;
            let value = require_parameter("hash-set", args, 2)?;

            hash.borrow_mut().insert(key.clone(), value.clone());

            Ok(Value::HashMap(hash.clone()))
        }),
    );

    entries.insert(
        String::from("+"),
        Value::NativeFunc(|_env, args| {
            let mut total = Value::NIL;

            for arg in args {
                if total == Value::NIL {
                    total = arg.clone();
                } else {
                    total = match arg {
                        Value::Int(x) => {
                            match total {
                                Value::Int(t) => Value::Int(t + x.clone()),
                                Value::Float(t) => {
                                    cfg_if! {
                                        if #[cfg(feature = "bigint")] {
                                            Value::Float(t + x.to_i128().unwrap() as FloatType)
                                        } else {
                                            Value::Float(t + *x as FloatType)
                                        }
                                    }
                                },
                                Value::String(t) => Value::String(t + x.to_string().as_str()),
                                _ => unreachable!("Will only ever assign int/float/string to `total`")
                            }
                        },
                        Value::Float(x) => {
                            match total {
                                Value::Int(t) => {
                                    cfg_if! {
                                        if #[cfg(feature = "bigint")] {
                                            Value::Float(t.to_i128().unwrap() as FloatType + *x)
                                        } else {
                                            Value::Float(t as FloatType + *x)
                                        }
                                    }
                                },
                                Value::Float(t) => Value::Float(t + *x),
                                Value::String(t) => Value::String(t + x.to_string().as_str()),
                                _ => unreachable!("Will only ever assign int/float/string to `total`")
                            }
                        },
                        Value::String(x) => {
                            match total {
                                Value::Int(t) => Value::String(format!("{}{}", t, x)),
                                Value::Float(t) => Value::String(format!("{}{}", t, x)),
                                Value::String(t) => Value::String(t + x),
                                _ => unreachable!("Will only ever assign int/float/string to `total`")
                            }
                        },
                        _ => {
                            return Err(RuntimeError {
                                msg: format!(
                                    "Function \"+\" requires arguments to be numbers or strings; found {}",
                                    arg
                                ),
                            });
                        }
                    };
                }
            }

            Ok(total)
        }),
    );

    entries.insert(
        String::from("-"),
        Value::NativeFunc(|_env, args| {
            let a = require_parameter("-", args, 0)?;
            let b = require_parameter("-", args, 1)?;

            if let (Some(a), Some(b)) = (a.as_int(), b.as_int()) {
                return Ok(Value::Int(a - b));
            }

            if let (Some(a), Some(b)) = (a.as_float(), b.as_float()) {
                return Ok(Value::Float(a - b));
            }

            Err(RuntimeError {
                msg: String::from("Function \"-\" requires arguments to be numbers"),
            })
        }),
    );

    entries.insert(
        String::from("*"),
        Value::NativeFunc(|_env, args| {
            let a = require_parameter("*", args, 0)?;
            let b = require_parameter("*", args, 1)?;

            if let (Some(a), Some(b)) = (a.as_int(), b.as_int()) {
                return Ok(Value::Int(a * b));
            }

            if let (Some(a), Some(b)) = (a.as_float(), b.as_float()) {
                return Ok(Value::Float(a * b));
            }

            Err(RuntimeError {
                msg: String::from("Function \"*\" requires arguments to be numbers"),
            })
        }),
    );

    entries.insert(
        String::from("/"),
        Value::NativeFunc(|_env, args| {
            let a = require_parameter("/", args, 0)?;
            let b = require_parameter("/", args, 1)?;

            if let (Some(a), Some(b)) = (a.as_int(), b.as_int()) {
                return Ok(Value::Int(a / b));
            }

            if let (Some(a), Some(b)) = (a.as_float(), b.as_float()) {
                return Ok(Value::Float(a / b));
            }

            Err(RuntimeError {
                msg: String::from("Function \"/\" requires arguments to be numbers"),
            })
        }),
    );

    entries.insert(
        String::from("truncate"),
        Value::NativeFunc(|_env, args| {
            let a = require_parameter("truncate", args, 0)?;
            let b = require_parameter("truncate", args, 1)?;

            if let (Some(a), Some(b)) = (a.as_int(), b.as_int()) {
                return Ok(Value::Int(a / b));
            }

            Err(RuntimeError {
                msg: String::from("Function \"truncate\" requires arguments to be integers"),
            })
        }),
    );

    entries.insert(
        String::from("not"),
        Value::NativeFunc(|_env, args| {
            let a = require_parameter("not", args, 0)?;

            Ok(Value::from_truth(!a.is_truthy()))
        }),
    );

    entries.insert(
        String::from("=="),
        Value::NativeFunc(|_env, args| {
            let a = require_parameter("==", args, 0)?;
            let b = require_parameter("==", args, 1)?;

            Ok(Value::from_truth(a == b))
        }),
    );

    entries.insert(
        String::from("!="),
        Value::NativeFunc(|_env, args| {
            let a = require_parameter("!=", args, 0)?;
            let b = require_parameter("!=", args, 1)?;

            Ok(Value::from_truth(a != b))
        }),
    );

    entries.insert(
        String::from("<"),
        Value::NativeFunc(|_env, args| {
            let a = require_parameter("<", args, 0)?;
            let b = require_parameter("<", args, 1)?;

            Ok(Value::from_truth(a < b))
        }),
    );

    entries.insert(
        String::from("<="),
        Value::NativeFunc(|_env, args| {
            let a = require_parameter("<=", args, 0)?;
            let b = require_parameter("<=", args, 1)?;

            Ok(Value::from_truth(a <= b))
        }),
    );

    entries.insert(
        String::from(">"),
        Value::NativeFunc(|_env, args| {
            let a = require_parameter(">", args, 0)?;
            let b = require_parameter(">", args, 1)?;

            Ok(Value::from_truth(a > b))
        }),
    );

    entries.insert(
        String::from(">="),
        Value::NativeFunc(|_env, args| {
            let a = require_parameter(">=", args, 0)?;
            let b = require_parameter(">=", args, 1)?;

            Ok(Value::from_truth(a >= b))
        }),
    );

    entries.insert(
        String::from("eval"),
        Value::NativeFunc(|env, args| {
            let expr = require_parameter("eval", args, 0)?;

            eval(env, expr)
        }),
    );

    entries.insert(
        String::from("apply"),
        Value::NativeFunc(|env, args| {
            let func = require_parameter("apply", args, 0)?;
            let params = require_list_parameter("apply", args, 1)?;

            eval(env, &Value::List(params.cons(func.clone())))
        }),
    );

    Env {
        parent: None,
        entries,
    }
}
