![Rust](https://github.com/brundonsmith/rust-lisp/workflows/Rust/badge.svg)

# What is this?

This is a Lisp interpreter, written in Rust, intended to be embeddable as a 
library in a larger application for scripting purposes. Goals:
- Small footprint (both code size and memory usage)
- No dependencies
- Easy, ergonomic interop with native Rust functions
- Small but practical set of Lisp functionality

# Basic Usage

```rust
use std::{cell::RefCell, rc::Rc};

use rust_lisp::{parse,eval,default_env};

fn main() {

  // create a base environment
  let env = Rc::new(RefCell::new(default_env()));

  // parse into a syntax tree
  let parse_result = parse("(+ \"Hello \" \"world!\")").unwrap();

  // evaluate
  let evaluation_result = eval(env.clone(), &parse_result[0]).unwrap();

  // use result
  println!("{}", &evaluation_result);
}
```

As you can see, the base environment is managed by the user of the library, as 
is the parsing stage. This is to give the user maximum control, including 
error-handling by way of `Result`s.

# The data model

The heart of the model is `Value`, an enum encompassing every type of valid Lisp
value. Most of these are trivial, but `Value::List` is not. It holds a 
recursive `ConsCell` data structure behind an `Rc` which functions like a 
linked-list. This can be difficult to work with in Rust code, so `into_iter()`
has been implemented for `&ConsCell`. On a similar 
note, `vec_to_cons()` is provided for going the other direction- from a 
`Vec<Value>` to a cons list.

`Value` does not implement `Copy` because of cases like `Value::List`, so if you
read the source you'll see lots of `value.clone()`. This almost always amounts 
to copying a primitive, except in the `Value::List` case where it means cloning
an `Rc` pointer. In all cases, it's considered cheap enough to do liberally.


# The environment and exposing Rust functions

The base environment is managed by the user of the library mainly so that it 
can be customized. `default_env()` prepopulates the environment with a number 
of common functions, but these can be omitted (or pared down) if you wish. 
Adding an entry to the environment is also how you would expose your Rust 
functions to your scripts, which can take the form of either regular functions 
or closures:

```rust
fn my_func(env: Rc<RefCell<Env>>, args: &Vec<Value>) -> Result<Value,RuntimeError> {
  Ok(Value::Nil)
}
```
```rust
env.borrow_mut().entries.insert(
  String::from("customfn"), 
  Value::NativeFunc(my_func));
```

```rust
entries.insert(
  String::from("sayhello"),
  Value::NativeFunc(
    |env, args| {
      println!("Hello world!");
      return Ok(Value::Nil);
    }));
```

In either case, a native function must have the following function signature:
```rust
type NativeFunc = fn(env: Rc<RefCell<Env>>, args: &Vec<Value>) -> Result<Value, RuntimeError>;
```

The first argument is the environment at the time and place of calling 
(closures are implemented as environment extensions). The second argument is 
the Vec of evaluated argument values. For convenience, utility functions 
(`require_parameter()`, `require_int_parameter()`, etc) have been provided for 
doing basic argument retrieval with error messaging. See 
`default_environment.rs` for examples.

# Included functionality

Special forms:
`define`, `set`, `defun`, `lambda`, `quote`, `let`, `begin`, `cond`, `if`, 
`and`, `or`

Functions (in `default_env()`):
`print`, `null?`, `number?`, `symbol?`, `boolean?`, `procedure?`, `pair?`, 
`car`, `cdr`, `cons`, `list`, `nth`, `sort`, `reverse`, `map`, `filter`, 
`length`, `range`, `+`, `-`, `*`, `/`, `truncate`, `not`, `==`, `!=`, `<`, `<=`,
`>`, `>=`, `apply`, `eval`

Other:
- Single-tick quoting

# Example REPL

```rust
use std::{cell::RefCell, rc::Rc, io};
use std::io::Write;

use rust_lisp::{default_env, parse, eval};

fn main() {

  // create a base environment
  let env = Rc::new(RefCell::new(default_env()));

  let mut buf = String::new();
  loop {
    print!("> ");
    io::stdout().flush().unwrap();
    io::stdin().read_line(&mut buf).unwrap();

    for expr in parse(&buf).unwrap() {
      println!("{}", eval(env.clone(), &expr).unwrap());
    }
  }
}
```