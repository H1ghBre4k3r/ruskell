<div align="center">

# Ruskell

**A Haskell-inspired programming language implemented in Rust**

[![Rust](https://img.shields.io/badge/Rust-2024_Edition-orange?logo=rust)](https://www.rust-lang.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![Status](https://img.shields.io/badge/Status-Experimental-yellow)]()

*Where functional elegance meets systems programming*

</div>

---

## Overview

Ruskell is a small, experimental functional programming language that takes inspiration from Haskell's elegant syntax while being built entirely in Rust. It features a custom lexer and parser, along with a tree-walking interpreter for program execution.

> **Note:** This project is in early development and is intended for learning and experimentation.

## Features

| Feature | Description |
|---------|-------------|
| **Haskell-like Syntax** | Clean, functional-style function definitions |
| **Do-block Expressions** | Sequential execution with `do...end` blocks |
| **Local Bindings** | Variable assignment with `:=` operator |
| **Lambda Expressions** | Multi-parameter lambdas with automatic currying |
| **Lexical Closures** | Proper environment capture for nested functions |
| **Pattern Matching** | Multi-clause functions and case expressions with destructuring |
| **List Cons Patterns** | Pattern matching on lists with `[head \| tail]` syntax |
| **Arithmetic Operators** | `+`, `-`, `*`, `/` with proper precedence |
| **Type Inference** | Full Hindley-Milner type system with let-polymorphism |
| **Type Safety** | Compile-time type checking catches errors early |
| **Lambda Lifting** | Automatic closure conversion for optimization |
| **Basic Types** | Integers, strings, lists, and unit values |
| **Tree-walking Interpreter** | Direct AST evaluation for simplicity |

## Language Syntax

### Function Definition

Functions are defined using the `=` operator followed by a `do...end` block:

```haskell
functionName = do
    -- expressions here
end
```

### Variable Binding

Local variables can be bound using the `:=` operator:

```haskell
myFunc = do
    x := 42
    x
end
```

### List Pattern Matching

Ruskell supports functional list manipulation with cons patterns:

```haskell
-- Multi-clause pattern matching on lists
length [] = 0
length [h | t] = 1 + length(t)

sum [] = 0
sum [x | xs] = x + sum(xs)

-- Higher-order list functions
map f [] = []
map f [x | xs] = listCons(f(x), map(f, xs))

filter p [] = []
filter p [x | xs] = do
    if p(x) then
        listCons(x, filter(p, xs))
    else
        filter(p, xs)
    end
end

-- Using list operations
main = do
    nums := [1, 2, 3, 4, 5]
    total := sum(nums)
    doubled := map(\x => x * 2, nums)
    print(toString(total))  -- 15
    0
end
```

### Complete Example

```haskell
main = do
    -- Define a closure that captures x
    makeAdder := \x => do
        \y => x + y
    end
    
    -- Create specialized functions
    add10 := makeAdder(10)
    add20 := makeAdder(20)
    
    -- Use them
    result1 := add10(5)   -- 15
    result2 := add20(5)   -- 25
    
    -- Arithmetic with precedence
    calculation := (result1 + result2) * 2 - 10
    
    calculation
end
```

**Output:**
```
Type checking...
✓ Type checking passed!
  main : Unit -> Int

Running program...
```
Exit code: 70

## Getting Started

### Prerequisites

- [Rust](https://rustup.rs/) (2024 edition)

### Installation

```bash
# Clone the repository
git clone https://github.com/yourusername/ruskell.git
cd ruskell

# Build the project
cargo build --release

# Run the interpreter (rsk binary)
cargo run
# Or after building
./target/release/rsk
```

## Architecture

```
┌─────────────┐     ┌─────────────┐     ┌─────────────┐     ┌─────────────┐
│   Source    │ ──▶ │    Lexer    │ ──▶ │   Parser    │ ──▶ │  Desugarer  │
│    Code     │     │   (lachs)   │     │  (custom)   │     │             │
└─────────────┘     └─────────────┘     └─────────────┘     └─────────────┘
                                                                    │
                                                                    ▼
┌─────────────┐     ┌─────────────┐                         ┌─────────────┐
│ Interpreter │ ◀── │    Type     │ ◀───────────────────────│   Core AST  │
│   (eval)    │     │   Checker   │                         │             │
└─────────────┘     └─────────────┘                         └─────────────┘
```

### Components

- **Lexer** (`src/lexer/`) – Tokenizes source code using the `lachs` library
- **Parser** (`src/parser/`) – Recursive descent parser building the surface AST
- **Desugarer** (`src/desugar/`) – Transforms surface AST to core AST (multi-param → single-param lambdas)
- **Lambda Lifter** (`src/desugar/lift.rs`) – Converts closures to top-level functions
- **Type Checker** (`src/types/`) – Hindley-Milner type inference with let-polymorphism
- **Interpreter** (`src/interpreter/`) – Tree-walking evaluator with scoped symbol resolution
- **Formatter** (`src/fmt/`) – Pretty-printing for both surface and core ASTs

## Dependencies

| Crate | Version | Purpose |
|-------|---------|---------|
| [anyhow](https://crates.io/crates/anyhow) | 1.0.100 | Flexible error handling |
| [clap](https://crates.io/crates/clap) | 4.5.53 | Command-line argument parsing |
| [lachs](https://crates.io/crates/lachs) | 0.1.4 | Lexer generation with derive macros |

## Roadmap

**Completed:**
- Function parameters and multi-argument calls
- Hindley-Milner type inference (type annotations in progress)
- Pattern matching with destructuring
- Lambda lifting for closure optimization
- Improved parser error reporting

**In Progress:**
- Type annotations syntax
- Additional primitive types (floats, booleans)

**Planned:**
- Standard library with common functions
- Interactive REPL
- Bytecode compiler and virtual machine

## Contributing

Contributions are welcome! Feel free to open issues or submit pull requests.

## License

This project is licensed under the MIT License – see the [LICENSE](LICENSE) file for details.

---

<div align="center">

Made with Rust

</div>
