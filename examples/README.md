# Ruskell Examples

This directory contains example programs demonstrating various features of the Ruskell programming language.

## Running Examples

To run any example:

```bash
cargo run -- run examples/<example_name>.rsk
```

Or with release optimizations:

```bash
cargo run --release -- run examples/<example_name>.rsk
```

## Available Examples

### pattern_matching_showcase.rsk

**Comprehensive demonstration of pattern matching features.**

This example showcases:

1. **Multi-clause Functions with Literal Patterns**
2. **Case Expressions**  
3. **Wildcard Patterns**
4. **Recursive Pattern Matching**
5. **Comparison and Conditional Logic**
6. **Higher-order Functions**
7. **String Concatenation**
8. **Boolean Logic**

### list_operations.rsk

**List manipulation using cons patterns `[head | tail]`.**

Demonstrates:
- `length` - count list elements recursively
- `sum` - add all elements in a list
- `head` / `tail` - extract first element or remaining elements
- `isEmpty` - check if list is empty
- `map` - apply function to all elements
- `filter` - keep only elements matching a predicate
- `take` / `drop` - take/drop first N elements

All implemented using multi-clause pattern matching on list cons patterns.

### print_demo.rsk

**Demonstrates the `print` builtin function.**

Shows printing of:
- Strings
- Integers
- Booleans
- Unit values `()`
- Computed values

### string_interpolation.rsk

**String manipulation and concatenation examples.**

### multi_param_showcase.rsk

**Multi-parameter function examples.**

### multi_param_pattern.rsk

**Pattern matching with multiple parameters.**

### many_patterns.rsk

**Examples of functions with many pattern clauses.**

### interesting_pattern_matching.rsk

**Additional pattern matching examples.**

### desugar.rsk

**Examples related to desugaring transformations.**

## Language Features Demonstrated

### Pattern Matching

Ruskell supports pattern matching in two forms:

1. **Multi-clause functions:**
   ```ruskell
   factorial 0 = 1
   factorial n = n * factorial(n - 1)
   ```

2. **Case expressions:**
   ```ruskell
   describeValue x = case x of
       0 => "zero"
       1 => "one"
       _ => "other"
   end
   ```

### Supported Pattern Types

- **Literal patterns:** `0`, `1`, `"hello"`, `true`, `false`, `[]` (empty list)
- **Identifier patterns:** `x`, `name` (binds the value to a variable)
- **Wildcard patterns:** `_` (matches anything, doesn't bind)
- **Unit pattern:** `()` (matches the unit value)
- **List cons patterns:** `[head | tail]` (matches non-empty lists)
  - In multi-clause functions: only simple patterns (identifiers/wildcards) allowed in head/tail
  - In case expressions: full nested pattern matching supported

### Function Definition Styles

1. **Single expression:**
   ```ruskell
   double x = x * 2
   ```

2. **Do-block for multiple statements:**
   ```ruskell
   main = do
       x := 10
       y := 20
       x + y
   end
   ```

### Current Limitations

- **Comments are not supported** - no `#`, `//`, or `--` comment syntax
- **Negative number literals** require subtraction: use `0 - 5` instead of `-5`
- **Comparisons in single-line expressions** don't parse correctly outside of do-blocks:
  ```ruskell
  max a b = if a > b then a else b  -- ❌ Doesn't parse
  
  max a b = do                      -- ✓ Works
      if a > b then a else b end
  end
  ```
- **List cons patterns in multi-clause functions only support simple patterns:**
  ```ruskell
  -- ✓ Works: simple identifiers and wildcards
  length [] = 0
  length [h | t] = 1 + length(t)
  
  -- ❌ Doesn't work: literals in cons pattern
  matchOne [1 | xs] = 100
  matchOne [x | xs] = x
  
  -- ✓ Workaround: use case expressions for complex patterns
  matchOne xs = case xs of
      [1 | rest] => 100
      [x | rest] => x
  end
  ```

## Tips

- Always use `do...end` blocks when you need comparisons, conditionals, or multiple statements
- Pattern matching is evaluated top-to-bottom; put more specific patterns before general ones
- The last expression in a `do` block is the return value
- Use `:=` for local bindings within `do` blocks
- Use `=` for function definitions at the top level
