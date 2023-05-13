# Rabbit 🐇

_A Wabbit compiler written in Rust._

## What's this?

This is my project code for the May 2023 session of [David Beazley](https://www.dabeaz.com)'s [Write a compiler course](https://www.dabeaz.com/compiler.html)—which I highly recommend. This repository contains a copy of the original code, which was originally developed in the course's private repository.

Rabbit is a compiler for the [Wabbit](docs/Wabbit-Specification.md) language designed by David for the course. The following stages are implemented:

- tokenizer
- parser
- formatter
- interpreter
- LLVM code generator


## Limitations and the `calc` project

Rabbit is mostly functional, but suffers from the following major limitations:

- The architecture is far from great.
- The type checker is missing due to lack of time.

Because of this, I rewrote everything after the course—albeit for a much smaller language—to study and address these limitations. This is what the [`calc`](https://github.com/abey79/calc) project is about. Interestingly, it turned out that the type checker, in addition to its inherent value for the end user, often simplifies the implementation of subsequent stages. 

Rabbit also has the following additional limitations:

- Bloc scoping is not properly handled (e.g. a variable defined in a `if` block is visibly outside of that block).
- Float literal without integer part (`.123`) are not supported.
- Proper escaping of whitespace `char` literal by the formatter.
- Short-circuit handling for comparison operators.
- Built-in types' conversion functions (`float(3)`, etc.)
- The LLVM codegen implementation is partial (functions not implemented, no error handling)

## Ferrous Metal

This project also includes [Ferrous Metal](crates/ferrous_metal), a Rust port of a tiny VM—called "Metal"—included in the course material to introduce stack-based (virtual) CPUs. A macro companion crate is included to provide an assembler-like DSL with support for labels, absolute address and relative offsets.

## Installation

Either clone the repo and use `cargo run`, or `cargo install --git https://github.com/abey79/rabbit`.

Run tests using `cargo test`.

## Usage

Rabbit includes a CLI interface to test various stages of the compiler:

```
$ cargo run -q -- --help
Rabbit — the Rust Wabbit compiler

Usage: rabbit <COMMAND>

Commands:
  tokenize  Tokenize the input and display the tokens
  parse     Parse the input and display the AST
  interp    Run the interpreter on the code
  format    Run the formatter on the code
  llvm      Run the code generator on the code
  help      Print this message or the help of the given subcommand(s)

Options:
  -h, --help     Print help
  -V, --version  Print version
```

For example:

```
$ cargo run --release -q -- run tests/Programs/15_mandel.wb 
................................................................................
................................................................................
................................................................................
................................................................................
................................................................................
................................................................................
................................................................................
................................................................................
................................................................................
.................................................***............................
................................................*****...........................
.................................................***............................
.......................................**...*************.......................
........................................***********************.................
.......................................***********************..................
.....................................**************************.................
....................................****************************................
.......................********....******************************...............
.....................************.******************************................
.....................******************************************.................
......*...*..**.*********************************************...................
.....................******************************************.................
.....................************.******************************................
.......................********....******************************...............
....................................****************************................
.....................................**************************.................
.......................................***********************..................
........................................***********************.................
.......................................**...*************.......................
.................................................***............................
................................................*****...........................
.................................................***............................
................................................................................
................................................................................
................................................................................
................................................................................
................................................................................
................................................................................
................................................................................
................................................................................
................................................................................

```

## Compiling to native executable

Compiling Wabbit code to a native executable requires `clang`:

```
$ cargo run -q -- llvm tests/Programs/15_mandel.wb | clang crates/rabbit/misc/runtime.c -x ir - -o mandel
warning: overriding the module target triple with arm64-apple-macosx13.0.0 [-Woverride-module]
1 warning generated.
 
$ ./mandel 
................................................................................
................................................................................
................................................................................
................................................................................
................................................................................
................................................................................
................................................................................
................................................................................
................................................................................
.................................................***............................
................................................*****...........................
.................................................***............................
.......................................**...*************.......................
........................................***********************.................
.......................................***********************..................
.....................................**************************.................
....................................****************************................
.......................********....******************************...............
.....................************.******************************................
.....................******************************************.................
......*...*..**.*********************************************...................
.....................******************************************.................
.....................************.******************************................
.......................********....******************************...............
....................................****************************................
.....................................**************************.................
.......................................***********************..................
........................................***********************.................
.......................................**...*************.......................
.................................................***............................
................................................*****...........................
.................................................***............................
................................................................................
................................................................................
................................................................................
................................................................................
................................................................................
................................................................................
................................................................................
................................................................................
................................................................................

```

Note that this requires a [small C runtime](crates/rabbit/misc/runtime.c) which implements the main function as well as print function for the various types.