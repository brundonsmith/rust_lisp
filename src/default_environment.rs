use crate::{
    interpreter::eval,
    lisp,
    model::{Env, FloatType, HashMapRc, IntType, List, RuntimeError, Symbol, Value},
    utils::{require_arg, require_typed_arg},
};
use cfg_if::cfg_if;
use std::{cell::RefCell, collections::HashMap, convert::TryInto, rc::Rc};
cfg_if! {
    if #[cfg(feature = "bigint")] {
        use num_traits::ToPrimitive;
    }
}

/// Initialize an instance of `Env` with several core Lisp functions implemented
/// in Rust. **Without this, you will only have access to the functions you
/// implement yourself.**
pub fn default_env() -> Env {
    let mut env = Env::new();

    env.define(
        Symbol::from("print"),
        Value::NativeFunc(|_env, args| {
            let expr = require_arg("print", args, 0)?;

            println!("{}", &expr);
            Ok(expr.clone())
        }),
    );

    env.define(
        Symbol::from("is_null"),
        Value::NativeFunc(|_env, args| {
            let val = require_arg("is_null", args, 0)?;

            Ok(Value::from(*val == Value::NIL))
        }),
    );

    env.define(
        Symbol::from("is_number"),
        Value::NativeFunc(|_env, args| {
            let val = require_arg("is_number", args, 0)?;

            Ok(match val {
                Value::Int(_) => Value::True,
                Value::Float(_) => Value::True,
                _ => Value::NIL,
            })
        }),
    );

    env.define(
        Symbol::from("is_symbol"),
        Value::NativeFunc(|_env, args| {
            let val = require_arg("is_symbol", args, 0)?;

            Ok(match val {
                Value::Symbol(_) => Value::True,
                _ => Value::NIL,
            })
        }),
    );

    env.define(
        Symbol::from("is_boolean"),
        Value::NativeFunc(|_env, args| {
            let val = require_arg("is_boolean", args, 0)?;

            Ok(match val {
                Value::True => Value::True,
                Value::False => Value::True,
                _ => Value::NIL,
            })
        }),
    );

    env.define(
        Symbol::from("is_procedure"),
        Value::NativeFunc(|_env, args| {
            let val = require_arg("is_procedure", args, 0)?;

            Ok(match val {
                Value::Lambda(_) => Value::True,
                Value::NativeFunc(_) => Value::True,
                _ => Value::NIL,
            })
        }),
    );

    env.define(
        Symbol::from("is_pair"),
        Value::NativeFunc(|_env, args| {
            let val = require_arg("is_pair", args, 0)?;

            Ok(match val {
                Value::List(_) => Value::True,
                _ => Value::NIL,
            })
        }),
    );

    env.define(
        Symbol::from("car"),
        Value::NativeFunc(|_env, args| {
            let list = require_typed_arg::<&List>("car", args, 0)?;

            list.car()
        }),
    );

    env.define(
        Symbol::from("cdr"),
        Value::NativeFunc(|_env, args| {
            let list = require_typed_arg::<&List>("cdr", args, 0)?;

            Ok(Value::List(list.cdr()))
        }),
    );

    env.define(
        Symbol::from("cons"),
        Value::NativeFunc(|_env, args| {
            let car = require_arg("cons", args, 0)?;
            let cdr = require_typed_arg::<&List>("cons", args, 1)?;

            Ok(Value::List(cdr.cons(car.clone())))
        }),
    );

    env.define(
        Symbol::from("list"),
        Value::NativeFunc(|_env, args| Ok(Value::List(args.iter().collect::<List>()))),
    );

    env.define(
        Symbol::from("nth"),
        Value::NativeFunc(|_env, args| {
            let index = require_typed_arg::<IntType>("nth", args, 0)?;
            let list = require_typed_arg::<&List>("nth", args, 1)?;

            let index = TryInto::<usize>::try_into(index).map_err(|_| RuntimeError {
                msg: "Failed converting to `usize`".to_owned(),
            })?;

            Ok(list.into_iter().nth(index).unwrap_or(Value::NIL))
        }),
    );

    env.define(
        Symbol::from("sort"),
        Value::NativeFunc(|_env, args| {
            let list = require_typed_arg::<&List>("sort", args, 0)?;

            let mut v: Vec<Value> = list.into_iter().collect();

            v.sort();

            Ok(Value::List(v.into_iter().collect()))
        }),
    );

    env.define(
        Symbol::from("reverse"),
        Value::NativeFunc(|_env, args| {
            let list = require_typed_arg::<&List>("reverse", args, 0)?;

            let mut v: Vec<Value> = list.into_iter().collect();

            v.reverse();

            Ok(Value::List(v.into_iter().collect()))
        }),
    );

    env.define(
        Symbol::from("map"),
        Value::NativeFunc(|env, args| {
            let func = require_arg("map", args, 0)?;
            let list = require_typed_arg::<&List>("map", args, 1)?;

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
    env.define(
        Symbol::from("filter"),
        Value::NativeFunc(|env, args| {
            let func = require_arg("filter", args, 0)?;
            let list = require_typed_arg::<&List>("filter", args, 1)?;

            list.into_iter()
                .filter_map(|val: Value| -> Option<Result<Value, RuntimeError>> {
                    let expr = lisp! { ({func.clone()} (quote {val.clone()})) };

                    match eval(env.clone(), &expr) {
                        Ok(matches) => {
                            if matches.into() {
                                Some(Ok(val))
                            } else {
                                None
                            }
                        }
                        Err(e) => Some(Err(e)),
                    }
                })
                .collect::<Result<List, RuntimeError>>()
                .map(Value::List)
        }),
    );

    env.define(
        Symbol::from("length"),
        Value::NativeFunc(|_env, args| {
            let list = require_typed_arg::<&List>("length", args, 0)?;

            cfg_if! {
                if #[cfg(feature = "bigint")] {
                    Ok(Value::Int(list.into_iter().len().into()))
                } else {
                    Ok(Value::Int(list.into_iter().len() as IntType))
                }
            }
        }),
    );

    env.define(
        Symbol::from("range"),
        Value::NativeFunc(|_env, args| {
            let start = require_typed_arg::<IntType>("range", args, 0)?;
            let end = require_typed_arg::<IntType>("range", args, 1)?;

            cfg_if! {
                if #[cfg(feature = "bigint")] {
                    let range = bigint_range(start, end);
                } else {
                    let range = start..end;
                }
            }

            Ok(Value::List(range.map(Value::from).collect()))
        }),
    );

    env.define(
        Symbol::from("hash"),
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

    env.define(
        Symbol::from("hash_get"),
        Value::NativeFunc(|_env, args| {
            let hash = require_typed_arg::<&HashMapRc>("hash_get", args, 0)?;
            let key = require_arg("hash_get", args, 1)?;

            Ok(hash
                .borrow()
                .get(key)
                .map(|v| v.clone())
                .unwrap_or(Value::NIL))
        }),
    );

    env.define(
        Symbol::from("hash_set"),
        Value::NativeFunc(|_env, args| {
            let hash = require_typed_arg::<&HashMapRc>("hash_set", args, 0)?;
            let key = require_arg("hash_set", args, 1)?;
            let value = require_arg("hash_set", args, 2)?;

            hash.borrow_mut().insert(key.clone(), value.clone());

            Ok(Value::HashMap(hash.clone()))
        }),
    );

    env.define(
        Symbol::from("+"),
        Value::NativeFunc(|_env, args| {
            let mut total = Value::NIL;

            for arg in args {
                total = match arg {
                    Value::Int(x) => {
                        match total {
                            Value::List(List::NIL) => arg.clone(),
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
                            Value::List(List::NIL) => arg.clone(),
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
                            Value::List(List::NIL) => arg.clone(),
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

            Ok(total)
        }),
    );

    env.define(
        Symbol::from("-"),
        Value::NativeFunc(|_env, args| {
            let a = require_arg("-", args, 0)?;
            let b = require_arg("-", args, 1)?;

            if let (Ok(a), Ok(b)) = (
                TryInto::<IntType>::try_into(a),
                TryInto::<IntType>::try_into(b),
            ) {
                return Ok(Value::Int(a - b));
            }

            if let (Ok(a), Ok(b)) = (
                TryInto::<FloatType>::try_into(a),
                TryInto::<FloatType>::try_into(b),
            ) {
                return Ok(Value::Float(a - b));
            }

            Err(RuntimeError {
                msg: String::from("Function \"-\" requires arguments to be numbers"),
            })
        }),
    );

    env.define(
        Symbol::from("*"),
        Value::NativeFunc(|_env, args| {
            if args.len() < 2 {
                return Err(RuntimeError {
                    msg: String::from("Function \"*\" requires at least two numeric arguments"),
                });
            }

            let mut product = Value::Int(1);

            for arg in args {
                product = match arg {
                    Value::Int(x) => match product {
                        Value::Int(t) => Value::Int(t * x.clone()),
                        Value::Float(t) => {
                            cfg_if! {
                                if #[cfg(feature = "bigint")] {
                                    Value::Float(t * x.to_i128().unwrap() as FloatType)
                                } else {
                                    Value::Float(t * *x as FloatType)
                                }
                            }
                        }
                        _ => unreachable!("Will only ever assign int/float to `product`"),
                    },
                    Value::Float(x) => match product {
                        Value::Int(t) => {
                            cfg_if! {
                                if #[cfg(feature = "bigint")] {
                                    Value::Float(t.to_i128().unwrap() as FloatType * *x)
                                } else {
                                    Value::Float(t as FloatType * *x)
                                }
                            }
                        }
                        Value::Float(t) => Value::Float(t * *x),
                        _ => unreachable!("Will only ever assign int/float to `product`"),
                    },
                    _ => {
                        return Err(RuntimeError {
                            msg: format!(
                                "Function \"+\" requires arguments to be numbers; found {}",
                                arg
                            ),
                        });
                    }
                };
            }

            Ok(product)
        }),
    );

    env.define(
        Symbol::from("/"),
        Value::NativeFunc(|_env, args| {
            let a = require_arg("/", args, 0)?;
            let b = require_arg("/", args, 1)?;

            if let (Ok(a), Ok(b)) = (
                TryInto::<IntType>::try_into(a),
                TryInto::<IntType>::try_into(b),
            ) {
                return Ok(Value::Int(a / b));
            }

            if let (Ok(a), Ok(b)) = (
                TryInto::<FloatType>::try_into(a),
                TryInto::<FloatType>::try_into(b),
            ) {
                return Ok(Value::Float(a / b));
            }

            Err(RuntimeError {
                msg: String::from("Function \"/\" requires arguments to be numbers"),
            })
        }),
    );

    env.define(
        Symbol::from("truncate"),
        Value::NativeFunc(|_env, args| {
            let a = require_arg("truncate", args, 0)?;
            let b = require_arg("truncate", args, 1)?;

            if let (Ok(a), Ok(b)) = (
                TryInto::<IntType>::try_into(a),
                TryInto::<IntType>::try_into(b),
            ) {
                return Ok(Value::Int(a / b));
            }

            Err(RuntimeError {
                msg: String::from("Function \"truncate\" requires arguments to be integers"),
            })
        }),
    );

    env.define(
        Symbol::from("not"),
        Value::NativeFunc(|_env, args| {
            let a = require_arg("not", args, 0)?;
            let a: bool = a.into();

            Ok(Value::from(!a))
        }),
    );

    env.define(
        Symbol::from("=="),
        Value::NativeFunc(|_env, args| {
            let a = require_arg("==", args, 0)?;
            let b = require_arg("==", args, 1)?;

            Ok(Value::from(a == b))
        }),
    );

    env.define(
        Symbol::from("!="),
        Value::NativeFunc(|_env, args| {
            let a = require_arg("!=", args, 0)?;
            let b = require_arg("!=", args, 1)?;

            Ok(Value::from(a != b))
        }),
    );

    env.define(
        Symbol::from("<"),
        Value::NativeFunc(|_env, args| {
            let a = require_arg("<", args, 0)?;
            let b = require_arg("<", args, 1)?;

            Ok(Value::from(a < b))
        }),
    );

    env.define(
        Symbol::from("<="),
        Value::NativeFunc(|_env, args| {
            let a = require_arg("<=", args, 0)?;
            let b = require_arg("<=", args, 1)?;

            Ok(Value::from(a <= b))
        }),
    );

    env.define(
        Symbol::from(">"),
        Value::NativeFunc(|_env, args| {
            let a = require_arg(">", args, 0)?;
            let b = require_arg(">", args, 1)?;

            Ok(Value::from(a > b))
        }),
    );

    env.define(
        Symbol::from(">="),
        Value::NativeFunc(|_env, args| {
            let a = require_arg(">=", args, 0)?;
            let b = require_arg(">=", args, 1)?;

            Ok(Value::from(a >= b))
        }),
    );

    env.define(
        Symbol::from("eval"),
        Value::NativeFunc(|env, args| {
            let expr = require_arg("eval", args, 0)?;

            eval(env, expr)
        }),
    );

    env.define(
        Symbol::from("apply"),
        Value::NativeFunc(|env, args| {
            let func = require_arg("apply", args, 0)?;
            let params = require_typed_arg::<&List>("apply", args, 1)?;

            eval(env, &Value::List(params.cons(func.clone())))
        }),
    );

    env
}

cfg_if! {
    if #[cfg(feature = "bigint")] {
        use num_bigint::BigInt;

        fn bigint_range(start: BigInt, end: BigInt) -> impl Iterator<Item=BigInt> {
            let mut current = start;

            std::iter::from_fn(move || {
                if current == end {
                    None
                } else {
                    let res = Some(current.clone());

                    current += 1;

                    res
                }
            })
        }
    }
}
