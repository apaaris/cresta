# Cresta Compiler

A compiler for the Cresta scientific computing language, written in OCaml.

## Language Design

Cresta is a C++-like statically typed language optimized for scientific computing with:
- Strict type safety (no implicit casting)
- Native complex number support
- Matrix-first operations with clear syntax
- Row-major memory layout
- Catastrophic crash error handling

See `docs/design.md` for the complete language specification.

## Building

```bash
# Install dependencies
opam install dune llvm

# Setup environment
eval $(opam env)

# Build the compiler
dune build

# Run the compiler
dune exec cre -- <options> <input_file>
```

## Usage

```cresta
// Matrix class with proper encapsulation
class Matrix {
private:
    <int32[100]> data;
    <int32> rows;
    <int32> cols;

public:
    void initialize(<int32> r, <int32> c) {
        rows = r;
        cols = c;
        <int32> i = 0;
        while (i < int32(100)) {
            data[i] = 0;
            i = i + int32(1);
        }
    }
    
    <int32> sum_elements() {
        <int32> total = 0;
        <int32> i = 0;
        <int32> size = rows * cols;
        
        while (i < size) {
            total = total + data[i];
            i = i + int32(1);
        }
        return total;
    }
}

// Function with array parameters and control flow
<int32> fibonacci(<int32> n) {
    if (n <= int32(1)) {
        return n;
    }
    
    <int32> a = 0;
    <int32> b = 1;
    <int32> i = 2;
    
    while (i <= n) {
        <int32> temp = a + b;
        a = b;
        b = temp;
        i = i + int32(1);
    }
    return b;
}

<int32> main() {
    <Matrix> matrix;
    matrix.initialize(int32(3), int32(3));
    
    <int32[5]> numbers = [10, 25, 5, 30, 15];
    <int32> result = matrix.sum_elements() + fibonacci(int32(10));
    
    return result;
}
```

# Usage

```bash
# Run the LLVM code generator test
dune exec test/test_codegen.exe

# Build the main compiler (when integrated)
dune exec bin/main.exe -- <input_file>
```

# Development Status

**Fully Implemented:**
- [x] **Lexical Analysis** - Complete tokenizer with error handling
- [x] **Parser** - Full recursive descent parser with comprehensive AST
- [x] **Semantic Analysis** - Type checking, symbol tables, and error reporting
- [x] **LLVM Code Generation** - Complete backend with optimization support
  - [x] Variables and memory management
  - [x] Expressions and arithmetic operations
  - [x] Control flow (if/else, while loops)
  - [x] Function declarations and calls
  - [x] Array types and access
  - [x] Object-oriented programming (classes, methods)
  - [x] Return statements and proper function handling

# Project Structure

```
cresta/
├── bin/              # Main executable
│   └── main.ml       # Command-line interface
├── lib/              # Compiler library modules  
│   ├── lexer.ml      # Tokenization and lexical analysis
│   ├── parser.ml     # Recursive descent parser
│   ├── semantic.ml   # Type checking and semantic analysis
│   └── codegen.ml    # LLVM IR code generation
├── test/             # Comprehensive test suite
│   ├── test_lexer.ml    # Lexer unit tests
│   ├── test_parser.ml   # Parser unit tests  
│   ├── test_semantic.ml # Semantic analysis tests
│   └── test_codegen.ml  # Code generation tests
├── docs/             # Documentation
│   ├── design.md     # Language specification
│   └── roadmap.md    # Development roadmap
└── examples/         # Example Cresta programs
```

# Generated LLVM IR

The compiler generates clean, optimizable LLVM IR. Example output:

```llvm
define i32 @add(i32 %a, i32 %b) {
entry:
  %a2 = alloca i32, align 4
  store i32 %a, i32* %a2, align 4
  %b3 = alloca i32, align 4
  store i32 %b, i32* %b3, align 4
  %a4 = load i32, i32* %a2, align 4
  %b5 = load i32, i32* %b3, align 4
  %addtmp = add i32 %a4, %b5
  ret i32 %addtmp
}
```

This IR can be compiled to native code for any target architecture supported by LLVM.

# Using Generated LLVM Code

To compile the generated LLVM IR to an executable:

```bash
# Generate assembly from LLVM IR
llc program.ll -o program.s

# Compile assembly to executable
clang program.s -o program
```
