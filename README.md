### Uniphiler: the compiler you love!

This is a compiler written in Prolog using DCG for the language Latte.


### Usage

#### Checking for errors

Running
```
> ./latc path/to/source.lat
```
will return "OK" for accepted programs and "ERROR" with additional errors for all others.

#### Compiling

For programs accepted by `latc`, running
```
> ./latc_llvm path/to/source.lat
```
will produce `path/to/source.ll` and `path/to/source.bc`.

#### Show Latte AST

```
> ./latc_ast path/to/source.lat
```

#### Show Latte tokens

```
> ./latc_tokens path/to/source.lat
```

The executables are just wrappers for [swi-prolog](https://www.swi-prolog.org/) running the scripts prolog scripts:
  * `latc` runs `src/check_latte.prolog`
  * `latc_llvm` runs `src/compile.prolog`
  * `latc_ast` runs `src/show_parse.prolog`
  * `latc_tokens` runs `src/show_tokens.prolog`


### Structure
The main stages of compilation:

1. Tokenization (`src/tokenize.prolog`)
2. Building Latte AST (`src/parse.prolog`)
3. Type derivation and error checks (`src/typing_and_checks.prolog`)
4. Generating LLVM AST (`src/llvm.prolog`)
5. Register numbering and printing LLVM (`src/llvm_print.prolog`)

Additionally, some helper modules:

  * uniform error handling for all stages of compilation (`src/errors.prolog`)
  * a _Context_ structure useful for tracking types and register allocations (`src/context.prolog`)
  * simplification of simple boolean expressions (`src/simplify.prolog`)

Some builtin functions are compiled from C, those can be found in `runtime/lib.c`, and compiled by running `make`.
