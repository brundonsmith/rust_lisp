![Rust](https://github.com/brundonsmith/rust-lisp/workflows/Rust/badge.svg)

# What is this?

This is a Lisp interpreter, written in Rust, intended to be embeddable as a
library in a larger application for scripting purposes. Goals:

- Small footprint (both code size and memory usage)
- No runtime dependencies [1]
- Easy, ergonomic interop with native Rust functions
- Small but practical set of Lisp functionality

[1] `cfg-if` is build-time, `num-traits` add (I believe) no runtime presence,
and `num-bigint` is entirely opt-in (at build time)

# Basic Usage

```rust
[dependencies]
rust_lisp = "0.11.0"
```

```rust
use std::{cell::RefCell, rc::Rc};

use rust_lisp::prelude::*;

fn main() {

  // create a base environment
  let env = Rc::new(RefCell::new(default_env()));

  // parse into an iterator of syntax trees (one for each root)
  let mut ast_iter = parse("(+ \"Hello \" \"world!\")");
  let first_expression = ast_iter.next().unwrap().unwrap();

  // evaluate
  let evaluation_result = eval(env.clone(), &first_expression).unwrap();

  // use result
  println!("{}", &evaluation_result);
}
```

As you can see, the base environment is managed by the user of the library, as
is the parsing stage. This is to give the user maximum control, including
error-handling by way of `Result`s.

# The data model

The heart of the model is `Value`, an enum encompassing every type of valid Lisp
value. Most of these are trivial, but `Value::List` is not. It holds a recursive
`List` data structure which functions internally like a linked-list.
`into_iter()` and `from_iter()` have been implemented for `List`, and there is
also a `lisp!` macro (see below) which makes working with Lists, in particular,
much more conventient.

`Value` does not implement `Copy` because of cases like `Value::List`, so if you
read the source you'll see lots of `value.clone()`. This almost always amounts
to copying a primitive, except in the `Value::List` case where it means cloning
an internal `Rc` pointer. In all cases, it's considered cheap enough to do
liberally.

# The environment and exposing Rust functions

The base environment is managed by the user of the library mainly so that it can
be customized. `default_env()` prepopulates the environment with a number of
common functions, but these can be omitted (or pared down) if you wish. Adding
an entry to the environment is also how you would expose your Rust functions to
your scripts, which can take the form of either regular functions or closures:

```rust
fn my_func(env: Rc<RefCell<Env>>, args: &Vec<Value>) -> Result<Value,RuntimeError> {
  println!("Hello world!");
  return Ok(Value::NIL);
}

...

  env.borrow_mut().entries.insert(
    String::from("sayhello"),
    Value::NativeFunc(my_func));
```

```rust
entries.insert(
  String::from("sayhello"),
  Value::NativeFunc(
    |env, args| {
      println!("Hello world!");
      return Ok(Value::NIL);
    }));
```

In either case, a native function must have the following function signature:

```rust
type NativeFunc = fn(env: Rc<RefCell<Env>>, args: &Vec<Value>) -> Result<Value, RuntimeError>;
```

The first argument is the environment at the time and place of calling (closures
are implemented as environment extensions). The second argument is the Vec of
evaluated argument values. For convenience, utility functions
(`require_parameter()`, `require_int_parameter()`, etc) have been provided for
doing basic argument retrieval with error messaging. See
`default_environment.rs` for examples.

# The `lisp!` macro

A Rust macro, named `lisp!`, is provided which allows the user to embed
sanitized Lisp syntax inside their Rust code, which will be converted to an AST
at compile-time:

```rust
fn parse_basic_expression() {
  let ast = parse("
    (list 
      (* 1 2)  ;; a comment
      (/ 6 3 \"foo\"))").next().unwrap().unwrap();

  assert_eq!(ast, lisp! {
    (list 
      (* 1 2)
      (/ 6 3 "foo"))
  });
}
```

Note that this just gives you a syntax tree (in the form of a `Value`). If you
want to actually evaluate the expression, you would need to then pass it to
`eval()`.

The macro also allows Rust expressions (of type `Value`) to be embedded within
the lisp code using `{  }`:

```rust
fn parse_basic_expression() {
  let ast = parse("
    (+ 3 1)").next().unwrap().unwrap();

  let n = 2;

  assert_eq!(ast, lisp! {
    (+ { Value::Int(n + 1) } 1)
  });
}
```

NOTE: When parsing lisp code from a string, dashes (`-`) are allowed to be used
in identifiers. _However_, due to the limitations of declarative Rust macros,
these cannot be handled correctly by `lisp! {}`. So it's recommended that you
use underscores in your identifiers instead, which the macro will be able to
handle correctly. The built-in functions follow this convention.

# Included functionality

Special forms: `define`, `set`, `defun`, `lambda`, `quote`, `let`, `begin`,
`cond`, `if`, `and`, `or`

Functions (in `default_env()`): `print`, `is_null`, `is_number`, `is_symbol`,
`is_boolean`, `is_procedure`, `is_pair`, `car`, `cdr`, `cons`, `list`, `nth`,
`sort`, `reverse`, `map`, `filter`, `length`, `range`, `hash`, `hash_get`,
`hash_set`, `+`, `-`, `*`, `/`, `truncate`, `not`, `==`, `!=`, `<`, `<=`, `>`,
`>=`, `apply`, `eval`

Other features:

- Single-tick quoting
- Tail-call optimization
