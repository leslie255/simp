# SIMP Is a Minimal Production

**A very simple compiler for a simple programming language**

This project is only a practice project of working with the cranelift backend,
it is not intended to be used for any production purposes.

## Compiling a File

### Using `run.sh`

You can use the `run.sh` script to compile and run one or multiple simp files.

Note that if you're using a Mac with later versions of Xcode you may experience linker error. To solve this problem you need to use LLVM's linker `lld`, and therfore requiring the install of LLVM, see [Installing LLVM](#installing-llvm).

```bash
./run.sh main.simp lib.simp
# If you encountered the linker crash on Mac:
./run.sh --use-lld main.simp lib.simp
# All availble flags:
./run.sh --help
```

### Manual compiling

SIMP uses the standard cargo build process:

```bash
$ cargo build --release
```

Compile a SIMP source file into an object file by:

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

Obviously you would need to install LLVM for this, see [Installing LLVM](#installing-llvm)

#### Installing LLVM
To install `lld`, you need to install LLVM:

```bash
# You need to have homebrew installed, obviously.
$ brew install llvm
# Get the path to the LLVM directory.
# (Usually /opt/homebrew/opt/llvm@17/).
$ brew info llvm
```

Assuming your path to LLVM directory is `/opt/homebrew/opt/llvm@17/`,
the LLVM `bin` directory would then be: `/opt/homebrew/opt/llvm@17/bin/`.

You may then add the this directory to PATH.

```bash
$ export PATH="$PATH:/opt/homebrew/opt/llvm@17/bin/"
# Note that it is possible to the -fuse-lld flag in clang by specifying the path to ld64.lld, but setting PATH is required for the run.sh to work.
```

Note that any recent versions of LLVM works, it doesn't have to be LLVM 17.

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
