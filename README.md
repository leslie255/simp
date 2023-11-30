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
$ ./target/release/simp source.simp -o source.o
```

You may then link the object file using clang:

```bash
$ clang source.o -o program
```

Note that if you're on Mac and using the latest versions of Xcode the default linker `ld` used by `clang` may crash. In this case you need to use LLVM's `lld` linker by:

```bash
$ clang -fuse-ld=/path/to/ld64.lld -o program
```

To install `lld`, you need to install `LLVM`:

```bash
# You need to have homebrew, obviously.
$ brew install llvm
# Get the path to the LLVM directory.
# (Usually /opt/homebrew/opt/llvm@17/).
$ brew info llvm
# The path to ld64.lld would then be:
# /opt/homebrew/opt/llvm@17/bin/ld64.lld
# You may then link the file using:
$ clang -fuse-ld=/opt/homebrew/opt/llvm@17/bin/ld64.lld source.o -o program
```

## Basic Syntax:

Defining a function:

```rust
// All functions have only one expression,
// the result of this expression is the return value of the function
// Also +-*/% works just like any programming language ever
fn inc(x) = x + 1;

// But a block is also an expression which returns its tail,
// so this is how you define a multi-expression function
fn main() = {
    let zero = inc(-1);
    zero   // Rust-style tail return
};
```

Notably, all values are `i64` in SIMP for simplicity sake. And all functions returns one `i64` value.

But there is a partial type system that allows for tupled values, single values, empty values, and unreachabe values.

Such that there can be tuple assignments, expressions that don't return anything (such as `let`), and expressions that never returns (`break`, `continue`, `return`).

Control flow works like every programming language ever, except there is no `for` or `while` loop,
only a `loop` loop which require you to manually break out of. Like Rust, `break` can also carry a value.

```rust
// A weird way of returning zero just to show that `break` can carry a value.
fn zero() = loop { break 0; };

fn main() = {
    // All variables are mutable.
    // If-else returns value.
    // The compiler would check for if the return type of the two branches matches.
    // here, `return 255;` has value of type `never`, which can be coerced into any type.
    let i = if 1 { zero() } else { return 255; };

    // Note that all expressions has to end with semicolons, including control flow statements
    loop {
        i = i + 1;
        if i == 10 {
            break;
        };
    };

    let j = i * 2;

    // Tupled assignments
    (i, j) = (j, i);

    j
};
```
