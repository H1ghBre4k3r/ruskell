<div align="center">

# 🦀 Ruskell

**A Haskell-inspired programming language implemented in Rust**

[![Rust](https://img.shields.io/badge/Rust-2024_Edition-orange?logo=rust)](https://www.rust-lang.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![Status](https://img.shields.io/badge/Status-Experimental-yellow)]()

*Where functional elegance meets systems programming*

</div>

---

## ✨ Overview

Ruskell is a small, experimental functional programming language that takes inspiration from Haskell's elegant syntax while being built entirely in Rust. It features a custom lexer and parser, along with a tree-walking interpreter for program execution.

> 🚧 **Note:** This project is in early development and is intended for learning and experimentation.

## 🎯 Features

| Feature | Description |
|---------|-------------|
| 📝 **Haskell-like Syntax** | Clean, functional-style function definitions |
| 🔄 **Do-block Expressions** | Sequential execution with `do...end` blocks |
| 📦 **Local Bindings** | Variable assignment with `:=` operator |
| 🧱 **Basic Types** | Integers, strings, and unit values |
| 🔍 **Lexical Scoping** | Proper scoped evaluation for variables and functions |
| ⚡ **Tree-walking Interpreter** | Direct AST evaluation for simplicity |

## 📖 Language Syntax

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

### Complete Example

```haskell
main = do
    foo
end

foo = do 
    bar := baz
    bar
end

baz = do 
    42
end
```

## 🚀 Getting Started

### Prerequisites

- [Rust](https://rustup.rs/) (2024 edition)

### Installation

```bash
# Clone the repository
git clone https://github.com/yourusername/ruskell.git
cd ruskell

# Build the project
cargo build --release

# Run
cargo run
```

## 🏗️ Architecture

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

- **Lexer** – Tokenizes source code using the `lachs` library with derive macros
- **Parser** – Recursive descent parser building the surface AST
- **Desugarer** – Transforms surface AST to core AST (multi-param → single-param lambdas)
- **Type Checker** – Hindley-Milner type inference with let-polymorphism
- **Interpreter** – Tree-walking evaluator with scoped symbol resolution

## 📦 Dependencies

| Crate | Version | Purpose |
|-------|---------|---------|
| [anyhow](https://crates.io/crates/anyhow) | 1.0.100 | Flexible error handling |
| [lachs](https://crates.io/crates/lachs) | 0.1.4 | Lexer generation |
| [lachs_derive](https://crates.io/crates/lachs_derive) | 0.1.4 | Lexer derive macros |

## 🗺️ Roadmap

- [x] Function arguments/parameters
- [x] Type system (inference complete, annotations pending)
- [ ] Pattern matching
- [ ] More primitive types (floats, booleans)
- [ ] Standard library
- [x] Better error messages
- [ ] REPL

## 🤝 Contributing

Contributions are welcome! Feel free to open issues or submit pull requests.

## 📄 License

This project is licensed under the MIT License – see the [LICENSE](LICENSE) file for details.

---

<div align="center">

Made with ❤️ and 🦀

</div>
