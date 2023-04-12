# SIMP Is a Minimal Production

**A very simple compiler for a simple programming language**

This project is only a practice project of working with the cranelift backend,
it is not intended to be used for any production purposes.

## Compiling a File

SIMP uses the standard cargo build process:

```bash
cargo build --release
```

You can compile a SIMP source file into an object file by:

```bash
# compile into source.o:
./target/release/simp source.simp

# or specify an output path:
./target/release/simp source.simp -o output.o
```

You may then link the output file using any linker you like:

```bash
# using clang or gcc to link with clib:
clang source.o
./a.out
```

## Basic Syntax:

Defining a function:

```rust
// All functions have only one expression,
// the result of this expression is the return value of the function
fn inc(x) = x + 1;

// But a block is also an expression which returns its tail,
// so this is how you define a multi-expression function
fn main() = {
    let zero = inc(-1);
    zero   // Rust-style tail return
};
```

Notably, all values are `i64` in SIMP for simplicity sake.

But there is a partial type system such that there can be tuple assignments and expressions that don't return anything (such as `let`) or never returns (`break`, `continue`, `return`).

Control flow works like every programming language ever, except there is not `for` or `while` loop,
only a `loop` loop which require you to manually break out of. Like Rust, `break` can also carry a value.

```rust
// A weird way of returning zero, just to show that `break` can carry a value.
fn zero() = loop { break 0; };

fn main() = {
    // All variables are mutable
    // If-else returns value
    // The compiler would check for if the return value from both branches are the same type,
    // here, `return 255;` has value of type `never`, which can be coerced into any types
    let i = if 1 { zero() } else { return 255; };

    loop {
        i = i + 1;
        if i == 10 {
            break;
        }
    };

    let j = i * 2;

    // Tupled assignments
    (i, j) = (j, i);

    j
};
```

