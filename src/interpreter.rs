use crate::{
    model::{Env, ForeignValueRc, Lambda, List, RuntimeError, Symbol, Value},
    utils::{require_arg, require_typed_arg},
};
use std::{cell::RefCell, rc::Rc};

/// Evaluate a single Lisp expression in the context of a given environment.
pub fn eval(env: Rc<RefCell<Env>>, expression: &Value) -> Result<Value, RuntimeError> {
    eval_inner(env, expression, Context::new())
}

/// Evaluate a series of s-expressions. Each expression is evaluated in
/// order and the final one's return value is returned.
pub fn eval_block(
    env: Rc<RefCell<Env>>,
    clauses: impl Iterator<Item = Value>,
) -> Result<Value, RuntimeError> {
    eval_block_inner(env, clauses, Context::new())
}

fn eval_block_inner(
    env: Rc<RefCell<Env>>,
    clauses: impl Iterator<Item = Value>,
    context: Context,
) -> Result<Value, RuntimeError> {
    let mut current_expr: Option<Value> = None;

    for clause in clauses {
        if let Some(expr) = current_expr {
            match eval_inner(env.clone(), &expr, context.found_tail(true)) {
                Ok(_) => (),
                Err(e) => {
                    return Err(e);
                }
            }
        }

        current_expr = Some(clause);
    }

    if let Some(expr) = &current_expr {
        eval_inner(env, expr, context)
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
    context: Context,
) -> Result<Value, RuntimeError> {
    if context.quoting {
        match expression {
            Value::List(list) if *list != List::NIL => match &list.car()? {
                Value::Symbol(Symbol(keyword)) if keyword == "comma" => {
                    // do nothing, handle it down below
                }
                _ => {
                    return list
                        .into_iter()
                        .map(|el| eval_inner(env.clone(), &el, context))
                        .collect::<Result<List, RuntimeError>>()
                        .map(Value::List);
                }
            },
            _ => return Ok(expression.clone()),
        }
    }

    match expression {
        // look up symbol
        Value::Symbol(symbol) => env.borrow().get(symbol).ok_or_else(|| RuntimeError {
            msg: format!("\"{}\" is not defined", symbol),
        }),

        // s-expression
        Value::List(list) if *list != List::NIL => {
            match &list.car()? {
                // special forms
                Value::Symbol(Symbol(keyword)) if keyword == "comma" => {
                    eval_inner(env, &list.cdr().car()?, context.quoting(false))
                }

                Value::Symbol(Symbol(keyword)) if keyword == "quote" => {
                    eval_inner(env, &list.cdr().car()?, context.quoting(true))
                }

                Value::Symbol(Symbol(keyword)) if keyword == "define" || keyword == "set" => {
                    let args = &list.cdr().into_iter().collect::<Vec<Value>>();

                    let symbol = require_typed_arg::<&Symbol>(keyword, args, 0)?;
                    let value_expr = require_arg(keyword, args, 1)?;

                    let value = eval_inner(env.clone(), value_expr, context.found_tail(true))?;

                    if keyword == "define" {
                        env.borrow_mut().define(symbol.clone(), value.clone());
                    } else {
                        env.borrow_mut().set(symbol.clone(), value.clone())?;
                    }

                    Ok(value)
                }

                Value::Symbol(Symbol(keyword)) if keyword == "defmacro" => {
                    let args = &list.cdr().into_iter().collect::<Vec<Value>>();

                    let symbol = require_typed_arg::<&Symbol>(keyword, args, 0)?;
                    let argnames_list = require_typed_arg::<&List>(keyword, args, 1)?;
                    let argnames = value_to_argnames(argnames_list.clone())?;
                    let body = Rc::new(Value::List(list.cdr().cdr().cdr()));

                    let lambda = Value::Macro(Lambda {
                        closure: env.clone(),
                        argnames,
                        body,
                    });

                    env.borrow_mut().define(symbol.clone(), lambda);

                    Ok(Value::NIL)
                }

                Value::Symbol(Symbol(keyword)) if keyword == "defun" => {
                    let args = &list.cdr().into_iter().collect::<Vec<Value>>();

                    let symbol = require_typed_arg::<&Symbol>(keyword, args, 0)?;
                    let argnames_list = require_typed_arg::<&List>(keyword, args, 1)?;
                    let argnames = value_to_argnames(argnames_list.clone())?;
                    let body = Rc::new(Value::List(list.cdr().cdr().cdr()));

                    let lambda = Value::Lambda(Lambda {
                        closure: env.clone(),
                        argnames,
                        body,
                    });

                    env.borrow_mut().define(symbol.clone(), lambda);

                    Ok(Value::NIL)
                }

                Value::Symbol(Symbol(keyword)) if keyword == "lambda" => {
                    let args = &list.cdr().into_iter().collect::<Vec<Value>>();

                    let argnames_list = require_typed_arg::<&List>(keyword, args, 0)?;
                    let argnames = value_to_argnames(argnames_list.clone())?;
                    let body = Rc::new(Value::List(list.cdr().cdr()));

                    Ok(Value::Lambda(Lambda {
                        closure: env,
                        argnames,
                        body,
                    }))
                }

                Value::Symbol(Symbol(keyword)) if keyword == "let" => {
                    let let_env = Rc::new(RefCell::new(Env::extend(env)));

                    let args = &list.cdr().into_iter().collect::<Vec<Value>>();

                    let declarations = require_typed_arg::<&List>(keyword, args, 0)?;

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

                        let result = eval_inner(let_env.clone(), expr, context.found_tail(true))?;
                        let_env.borrow_mut().define(symbol.clone(), result);
                    }

                    let body = &Value::List(list.cdr().cdr());
                    let body: &List = body.try_into().map_err(|_| RuntimeError {
                        msg: format!(
                            "Expected expression(s) after let-declarations, found {}",
                            body
                        ),
                    })?;

                    eval_block_inner(let_env, body.into_iter(), context)
                }

                Value::Symbol(Symbol(keyword)) if keyword == "begin" => {
                    eval_block_inner(env, list.cdr().into_iter(), context)
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

                        if eval_inner(env.clone(), condition, context.found_tail(true))?.into() {
                            return eval_inner(env, then, context);
                        }
                    }

                    Ok(Value::NIL)
                }

                Value::Symbol(Symbol(keyword)) if keyword == "if" => {
                    let args = &list.cdr().into_iter().collect::<Vec<Value>>();

                    let condition = require_arg(keyword, args, 0)?;
                    let then_expr = require_arg(keyword, args, 1)?;
                    let else_expr = require_arg(keyword, args, 2).ok();

                    if eval_inner(env.clone(), condition, context.found_tail(true))?.into() {
                        eval_inner(env, then_expr, context)
                    } else {
                        else_expr
                            .map(|expr| eval_inner(env, &expr, context))
                            .unwrap_or(Ok(Value::NIL))
                    }
                }

                Value::Symbol(Symbol(keyword)) if keyword == "and" || keyword == "or" => {
                    let args = &list.cdr().into_iter().collect::<Vec<Value>>();
                    let is_or = keyword.as_str() == "or";

                    let mut last_result: Option<Value> = None;
                    for arg in args {
                        let result = eval_inner(env.clone(), arg, context.found_tail(true))?;
                        let truthy: bool = (&result).into();

                        if is_or == truthy {
                            return Ok(result);
                        }

                        last_result = Some(result);
                    }

                    Ok(if let Some(last_result) = last_result {
                        last_result
                    } else {
                        // there were zero arguments
                        (!is_or).into()
                    })
                }

                Value::Symbol(Symbol(keyword)) if keyword == "cmd" => {
                    let args = &list.cdr().into_iter().collect::<Vec<Value>>();

                    let target = require_arg(keyword, args, 0)?;
                    let target = &eval_inner(env.clone(), target, context)?;
                    let target: &ForeignValueRc = target.try_into()?;
                    let mut target_borrowed = target.borrow_mut();
                    let command = require_typed_arg::<&Symbol>(keyword, args, 1)?;
                    let command_args = &args[2..]
                        .iter()
                        .map(|arg| eval_inner(env.clone(), arg, context))
                        .collect::<Result<Vec<Value>, RuntimeError>>()?;

                    target_borrowed.command(env, &command.0, command_args)
                }

                // function call or macro expand
                _ => {
                    let mut func_or_macro =
                        eval_inner(env.clone(), &list.car()?, context.found_tail(true))?;

                    if matches!(func_or_macro, Value::Macro(_)) {
                        let args = list.into_iter().skip(1).collect::<Vec<Value>>();

                        let expanded =
                            call_function_or_macro(env.clone(), &mut func_or_macro, args)?;

                        eval_inner(env.clone(), &expanded, Context::new())
                    } else {
                        let args = list
                            .into_iter()
                            .skip(1)
                            .map(|car| eval_inner(env.clone(), &car, context.found_tail(true)))
                            .collect::<Result<Vec<Value>, RuntimeError>>()?;

                        if !context.found_tail && context.in_func {
                            Ok(Value::TailCall {
                                func: Rc::new(func_or_macro),
                                args,
                            })
                        } else {
                            let mut res =
                                call_function_or_macro(env.clone(), &mut func_or_macro, args);

                            while let Ok(Value::TailCall { func, args }) = res {
                                res = call_function_or_macro(env.clone(), func.as_ref(), args);
                            }

                            res
                        }
                    }
                }
            }
        }

        // plain value
        _ => Ok(expression.clone()),
    }
}
// 🦀 Boo! Did I scare ya? Haha!

fn value_to_argnames(argnames: List) -> Result<Vec<Symbol>, RuntimeError> {
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
}

/// Calling a function is separated from the main `eval_inner()` function
/// so that tail calls can be evaluated without just returning themselves
/// as-is as a tail-call.
fn call_function_or_macro(
    env: Rc<RefCell<Env>>,
    func: &Value,
    args: Vec<Value>,
) -> Result<Value, RuntimeError> {
    if let Value::NativeFunc(func) = func {
        func(env, args)
    } else if let Value::NativeClosure(closure) = func {
        closure.borrow_mut()(env, args)
    } else {
        let lambda = match func {
            Value::Lambda(lamb) => Some(lamb),
            Value::Macro(lamb) => Some(lamb),
            _ => None,
        };

        if let Some(lambda) = lambda {
            // bind args
            let mut arg_env = Env::extend(lambda.closure.clone());
            for (index, arg_name) in lambda.argnames.iter().enumerate() {
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
            let clauses: &List = lambda.body.as_ref().try_into()?;
            eval_block_inner(
                Rc::new(RefCell::new(arg_env)),
                clauses.into_iter(),
                Context {
                    found_tail: false,
                    in_func: true,
                    quoting: false,
                },
            )
        } else {
            Err(RuntimeError {
                msg: format!("{} is not callable", func),
            })
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct Context {
    pub found_tail: bool,
    pub in_func: bool,
    pub quoting: bool,
}

impl Context {
    pub fn new() -> Self {
        Self {
            found_tail: false,
            in_func: false,
            quoting: false,
        }
    }

    pub fn found_tail(self, found_tail: bool) -> Self {
        Self {
            found_tail,
            in_func: self.in_func,
            quoting: self.quoting,
        }
    }

    // pub fn in_func(self, in_func: bool) -> Self {
    //     Self {
    //         found_tail: self.found_tail,
    //         in_func,
    //         quoting: self.quoting,
    //     }
    // }

    pub fn quoting(self, quoting: bool) -> Self {
        Self {
            found_tail: self.found_tail,
            in_func: self.in_func,
            quoting,
        }
    }
}
