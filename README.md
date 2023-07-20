# monkey-interpreter
An interpreter for the Monkey programming language written in Rust. This
project contains a lexer, parser, REPL and interpreter for the language.

## The Monkey Programming Language
The [Monkey programming language](https://monkeylang.org/) is a language
designed by the author of the book [Writing An Interpreter In
Go](https://interpreterbook.com/). The language has:

- C-like syntax
- variable bindings
- integers and booleans
- arithmetic expressions
- built-in functions
- first-class and higher-order functions
- closures
- a string data structure
- an array data structure
- a hash data structure

## Quick Start
```bash
# repl
$ cargo run --release --bin repl

# interpreter
$ cargo run --release --bin interpreter <file>
```
