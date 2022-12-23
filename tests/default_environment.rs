use rust_lisp::{
    default_env,
    interpreter::eval,
    lisp,
    model::{IntType, RuntimeError, Symbol, Value},
};
use std::{cell::RefCell, rc::Rc};

#[test]
fn range() {
    assert_eq!(
        eval_ast(lisp! {
            (range 0 10)
        }),
        eval_ast(lisp! {
            (list 0 1 2 3 4 5 6 7 8 9)
        })
    );
}

#[test]
fn nth() {
    assert_eq!(
        eval_ast(lisp! {
            (nth 3 (range 5 15))
        }),
        Value::from(Into::<IntType>::into(8))
    );
}

#[test]
fn map() {
    assert_eq!(
        eval_ast(lisp! {
            (map
                (lambda (n) (* n 2))
                (range 0 10))
        }),
        eval_ast(lisp! {
            (list 0 2 4 6 8 10 12 14 16 18)
        })
    );
}

#[test]
fn reverse() {
    assert_eq!(
        eval_ast(lisp! {
            (reverse (range 0 10))
        }),
        eval_ast(lisp! {
            (list 9 8 7 6 5 4 3 2 1 0)
        })
    );
}

#[test]
fn sort() {
    assert_eq!(
        eval_ast(lisp! {
            (sort (list 4 3 2 0 1))
        }),
        eval_ast(lisp! {
            (list 0 1 2 3 4)
        })
    );
}

#[test]
fn filter() {
    assert_eq!(
        eval_ast(lisp! {
            (filter
                (lambda (x) (== x "foo"))
                (list "foo" "bar" "a" "b" "foo"))
        }),
        eval_ast(lisp! {
            (list "foo" "foo")
        })
    );
}

#[test]
fn map_list_of_lists() {
    let result = eval_ast(lisp! {
        (map (lambda (x) x) (list (list 0 1) (list 2 3)))
    });

    assert_eq!(result, lisp! {((0 1) (2 3))});
}

#[test]
fn filter_list_of_lists() {
    let result = eval_ast(lisp! {
        (filter (lambda (x) (> (length x) 2))
            (list
                (list 0)
                (list 0 1)
                (list 0 1 2)
                (list 0 1 2 3)))
    });

    assert_eq!(result, lisp! {((0 1 2) (0 1 2 3))});
}

#[test]
fn hash_map() {
    let result = eval_ast(lisp! {
        (begin
            (define my_hash (hash "one" 1 "two" 2 "three" 3))
            (hash_set my_hash "four" 4)
            (+
                (hash_get my_hash "one")
                " "
                (hash_get my_hash "four")))
    });

    assert_eq!(result, lisp! { "1 4" });
}

#[test]
fn number_cast_comparisons() {
    assert_eq!(
        eval_ast(lisp! {
            (< 0 0.1)
        }),
        lisp! { T }
    );

    assert_eq!(
        eval_ast(lisp! {
            (< { Value::Int((-2).into()) } 1)
        }),
        lisp! { T }
    );

    assert_eq!(
        eval_ast(lisp! {
            (>= { Value::Float(-2.1) } 1)
        }),
        lisp! { F }
    );
}

#[test]
fn opaque() {
    let env = Rc::new(RefCell::new(default_env()));

    struct Foo(i64);

    env.borrow_mut().define(
        Symbol::from("makefoo"),
        Value::NativeFunc(|_, args| {
            let val = match args[0] {
                Value::Int(i) => Ok(i),
                _ => Err(RuntimeError {
                    msg: "incorrect type".to_string(),
                }),
            }?;

            Ok(Value::Opaque(Rc::new(RefCell::new(Foo(val.into())))))
        }),
    );

    let result = eval(
        env,
        &lisp! {
            (makefoo 42)
        },
    )
    .unwrap();

    if let Value::Opaque(op) = result {
        if let Some(Foo(v)) = op.borrow().downcast_ref() {
            assert_eq!(*v, 42);
        } else {
            panic!("value is not a Foo");
        }
    } else {
        panic!("value not an opaque value");
    }
}

#[cfg(test)]
fn eval_ast(ast: Value) -> Value {
    let env = Rc::new(RefCell::new(default_env()));
    return eval(env, &ast).unwrap();
}
