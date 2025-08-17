# Cresta Compiler

A compiler for the Cresta scientific computing language, written in OCaml.

## Language Design

Cresta is a C++-like statically typed language optimized for scientific computing with:
- Strict type safety (no implicit casting)
- Native complex number support
- Matrix-first operations with clear syntax
- Row-major memory layout
- Catastrophic crash error handling

See `design.md` for the complete language specification.

## Building

```bash
# Setup environment
eval $(opam env)

# Build the compiler
dune build

# Run the compiler
dune exec cre -- <options> <input_file>
```

## Usage

```bash
# Show help
cre --help

# Compile a file
cre program.cr

# Compile with verbose output
cre -v program.cr

# Specify output file
cre -o output.c program.cr

# Debug mode
cre -d program.cr
```

## Development Status

Currently implemented:
- [x] Project scaffolding
- [x] Command line argument parsing
- [x] Lexical analysis (tokenizer)
- [x] Parser (AST generation)
- [x] Semantic analysis & type checking
- [ ] Code generation

## Project Structure

```
cresta/
├── bin/          # Main executable
├── lib/          # Compiler library modules
├── test/         # Test files
├── examples/     # Example Cresta programs
├── design.md     # Language specification
└── roadmap.md    # Development roadmap
```
