use crate::model::{Env, Lambda, List, RuntimeError, Symbol, Value};
use std::{cell::RefCell, rc::Rc};

/// Evaluate a single Lisp expression in the context of a given environment.
pub fn eval(env: Rc<RefCell<Env>>, expression: &Value) -> Result<Value, RuntimeError> {
    eval_inner(env, expression, false, false)
}

/// Evaluate a series of s-expressions. Each expression is evaluated in
/// order and the final one's return value is returned.
pub fn eval_block(
    env: Rc<RefCell<Env>>,
    clauses: impl Iterator<Item = Value>,
) -> Result<Value, RuntimeError> {
    eval_block_inner(env, clauses, false, false)
}

fn eval_block_inner(
    env: Rc<RefCell<Env>>,
    clauses: impl Iterator<Item = Value>,
    found_tail: bool,
    in_func: bool,
) -> Result<Value, RuntimeError> {
    let mut current_expr: Option<Value> = None;

    for clause in clauses {
        if let Some(expr) = current_expr {
            match eval_inner(env.clone(), &expr, true, in_func) {
                Ok(_) => (),
                Err(e) => {
                    return Err(e);
                }
            }
        }

        current_expr = Some(clause);
    }

    if let Some(expr) = &current_expr {
        eval_inner(env, expr, found_tail, in_func)
    } else {
        Err(RuntimeError {
            msg: "Unrecognized expression".to_owned(),
        })
    }
}

/// `found_tail` and `in_func` are used when locating the tail position for
/// tail-call optimization. Candidates are not eligible if a) we aren't already
/// inside a function call, or b) we've already found the tail inside the current
/// function call. `found_tail` is currently overloaded inside special forms to
/// factor out function calls in, say, the conditional slot, which are not
/// eligible to be the tail-call based on their position. A future refactor hopes
/// to make things a little more semantic.
fn eval_inner(
    env: Rc<RefCell<Env>>,
    expression: &Value,
    found_tail: bool,
    in_func: bool,
) -> Result<Value, RuntimeError> {
    match expression {
        // look up symbol
        Value::Symbol(symbol) => env.borrow().get(symbol).ok_or_else(|| RuntimeError {
            msg: format!("\"{}\" is not defined", symbol),
        }),

        // s-expression
        Value::List(list) if *list != List::NIL => {
            match &list.car()? {
                // special forms
                Value::Symbol(Symbol(keyword)) if keyword == "define" || keyword == "set" => {
                    let cdr = list.cdr();
                    let symbol = &cdr.car()?;
                    let symbol: &Symbol = symbol.try_into().map_err(|_| RuntimeError {
                        msg: format!(
                            "Symbol required for definition; received \"{}\", which is a {}",
                            symbol,
                            symbol.type_name()
                        ),
                    })?;
                    let value_expr = &cdr.cdr().car()?;
                    let value = eval_inner(env.clone(), value_expr, true, in_func)?;

                    if keyword == "define" {
                        env.borrow_mut().define(symbol.clone(), value.clone());
                    } else {
                        env.borrow_mut().set(symbol.clone(), value.clone())?;
                    }

                    Ok(value)
                }

                Value::Symbol(Symbol(keyword)) if keyword == "defun" => {
                    let cdr = list.cdr();
                    let symbol = &cdr.car()?;
                    let symbol: &Symbol = symbol.try_into().map_err(|_| RuntimeError {
                        msg: format!(
                            "Function name must by a symbol; received \"{}\", which is a {}",
                            symbol,
                            symbol.type_name()
                        ),
                    })?;
                    let argnames = value_to_argnames(cdr.cdr().car()?)?;
                    let body = Rc::new(Value::List(cdr.cdr().cdr()));

                    let lambda = Value::Lambda(Lambda {
                        closure: env.clone(),
                        argnames,
                        body,
                    });

                    env.borrow_mut().define(symbol.clone(), lambda);

                    Ok(Value::NIL)
                }

                Value::Symbol(Symbol(keyword)) if keyword == "lambda" => {
                    let cdr = list.cdr();
                    let argnames = value_to_argnames(cdr.car()?)?;
                    let body = Rc::new(Value::List(cdr.cdr()));

                    Ok(Value::Lambda(Lambda {
                        closure: env,
                        argnames,
                        body,
                    }))
                }

                Value::Symbol(Symbol(keyword)) if keyword == "quote" => Ok(list.cdr().car()?),

                Value::Symbol(Symbol(keyword)) if keyword == "let" => {
                    let let_env = Rc::new(RefCell::new(Env::extend(env)));
                    let declarations = &list.cdr().car()?;
                    let declarations: &List =
                        declarations.try_into().map_err(|_| RuntimeError {
                            msg: "Expected list of declarations for let form".to_owned(),
                        })?;

                    for decl in declarations.into_iter() {
                        let decl = &decl;

                        let decl_cons: &List = decl.try_into().map_err(|_| RuntimeError {
                            msg: format!("Expected declaration clause, found {}", decl),
                        })?;
                        let symbol = &decl_cons.car()?;
                        let symbol: &Symbol = symbol.try_into().map_err(|_| RuntimeError {
                            msg: format!("Expected symbol for let declaration, found {}", symbol),
                        })?;
                        let expr = &decl_cons.cdr().car()?;

                        let result = eval_inner(let_env.clone(), expr, true, in_func)?;
                        let_env.borrow_mut().define(symbol.clone(), result);
                    }

                    let body = &Value::List(list.cdr().cdr());
                    let body: &List = body.try_into().map_err(|_| RuntimeError {
                        msg: format!(
                            "Expected expression(s) after let-declarations, found {}",
                            body
                        ),
                    })?;

                    eval_block_inner(let_env, body.into_iter(), found_tail, in_func)
                }

                Value::Symbol(Symbol(keyword)) if keyword == "begin" => {
                    eval_block_inner(env, list.cdr().into_iter(), found_tail, in_func)
                }

                Value::Symbol(Symbol(keyword)) if keyword == "cond" => {
                    let clauses = list.cdr();

                    for clause in clauses.into_iter() {
                        let clause = &clause;

                        let clause: &List = clause.try_into().map_err(|_| RuntimeError {
                            msg: format!("Expected conditional clause, found {}", clause),
                        })?;

                        let condition = &clause.car()?;
                        let then = &clause.cdr().car()?;

                        if eval_inner(env.clone(), condition, true, in_func)?.into() {
                            return eval_inner(env, then, found_tail, in_func);
                        }
                    }

                    Ok(Value::NIL)
                }

                Value::Symbol(Symbol(keyword)) if keyword == "if" => {
                    let cdr = list.cdr();
                    let condition = &cdr.car()?;
                    let then_expr = &cdr.cdr().car()?;
                    let else_expr = cdr.cdr().cdr().car().ok();

                    if eval_inner(env.clone(), condition, true, in_func)?.into() {
                        Ok(eval_inner(env, then_expr, found_tail, in_func)?)
                    } else {
                        else_expr
                            .map(|expr| eval_inner(env, &expr, found_tail, in_func))
                            .unwrap_or(Ok(Value::NIL))
                    }
                }

                Value::Symbol(Symbol(keyword)) if keyword == "and" || keyword == "or" => {
                    let cdr = list.cdr();
                    let a = &cdr.car()?;
                    let b = &cdr.cdr().car()?;

                    let truth = match keyword.as_str() {
                        "and" => {
                            eval_inner(env.clone(), a, true, in_func)?.into()
                                && eval_inner(env, b, true, in_func)?.into()
                        }
                        "or" => {
                            eval_inner(env.clone(), a, true, in_func)?.into()
                                || eval_inner(env, b, true, in_func)?.into()
                        }
                        _ => unreachable!("Only 'and' and 'or' are allowed by the match arm"),
                    };

                    Ok(Value::from(truth))
                }

                // function call
                _ => {
                    let func = eval_inner(env.clone(), &list.car()?, true, in_func)?;
                    let args = list
                        .into_iter()
                        .skip(1)
                        .map(|car| eval_inner(env.clone(), &car, true, in_func))
                        .collect::<Result<Vec<Value>, RuntimeError>>()?;

                    if !found_tail && in_func {
                        Ok(Value::TailCall {
                            func: Rc::new(func),
                            args,
                        })
                    } else {
                        let mut res = call_function(env.clone(), &func, args);

                        while let Ok(Value::TailCall { func, args }) = res {
                            res = call_function(env.clone(), &func, args);
                        }

                        res
                    }
                }
            }
        }

        // plain value
        _ => Ok(expression.clone()),
    }
}
// 🦀 Boo! Did I scare ya? Haha!

fn value_to_argnames(argnames: Value) -> Result<Vec<Symbol>, RuntimeError> {
    if let Value::List(argnames) = argnames {
        argnames
            .into_iter()
            .enumerate()
            .map(|(index, arg)| match arg {
                Value::Symbol(s) => Ok(s),
                _ => Err(RuntimeError {
                    msg: format!(
                        "Expected list of arg names, but arg {} is a {}",
                        index,
                        arg.type_name()
                    ),
                }),
            })
            .collect()
    } else {
        Err(RuntimeError {
            msg: format!("Expected list of arg names, received \"{}\"", argnames),
        })
    }
}

/// Calling a function is separated from the main `eval_inner()` function
/// so that tail calls can be evaluated without just returning themselves
/// as-is as a tail-call.
fn call_function(
    env: Rc<RefCell<Env>>,
    func: &Value,
    args: Vec<Value>,
) -> Result<Value, RuntimeError> {
    match func {
        // call native function
        Value::NativeFunc(func) => func(env, &args),

        // call lambda function
        Value::Lambda(lamb) => {
            // bind args
            let mut arg_env = Env::extend(lamb.closure.clone());
            for (index, arg_name) in lamb.argnames.iter().enumerate() {
                if arg_name.0 == "..." {
                    // rest parameters
                    arg_env.define(
                        Symbol::from("..."),
                        Value::List(args.into_iter().skip(index).collect()),
                    );
                    break;
                } else {
                    arg_env.define(arg_name.clone(), args[index].clone());
                }
            }

            // evaluate each line of body
            let clauses: &List = lamb.body.as_ref().try_into()?;
            eval_block_inner(
                Rc::new(RefCell::new(arg_env)),
                clauses.into_iter(),
                false,
                true,
            )
        }

        _ => Err(RuntimeError {
            msg: format!("{} is not callable", func),
        }),
    }
}
