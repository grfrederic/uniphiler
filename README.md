### Uniphiler: the compiler you love!

This is a compiler written in Prolog using DCG for the language Latte.


### Usage

#### Compiling

Running
```
> ./latc path/to/source.lat
```
for correct programs will produce `path/to/source.prelink.ll`, `path/to/source.ll` and `path/to/source.bc`.

#### Show LLVM

Running
```
> ./latc_llvm path/to/source.lat
```
for correct programs will print the _prelink_ LLVM code.

#### Just checking for errors

Running
```
> ./latc_check path/to/source.lat
```
will return "OK" for accepted programs and "ERROR" with additional errors for all others.

#### Show Latte AST

```
> ./latc_ast path/to/source.lat
```

#### Show Latte tokens

```
> ./latc_tokens path/to/source.lat
```

The executables are just wrappers for [swi-prolog](https://www.swi-prolog.org/) running the scripts prolog scripts:
  * `latc` and `latc_llvm` run `src/compile.prolog`
  * `latc_check` runs `src/check_latte.prolog`
  * `latc_ast` runs `src/show_parse.prolog`
  * `latc_tokens` runs `src/show_tokens.prolog`


### Structure
The main stages of compilation:

1. Tokenization (`src/tokenize.prolog`)
2. Building Latte AST (`src/parse.prolog`)
3. Type derivation and error checks (`src/typing_and_checks.prolog`)
4. Generating LLVM AST (`src/llvm.prolog`)
5. Optimization of LLVM AST (`src/llvm_opts.prolog`)
6. Register numbering and printing LLVM (`src/llvm_print.prolog`)

Additionally, some helper modules:

  * uniform error handling for all stages of compilation (`src/errors.prolog`)
  * a _Context_ structure useful for tracking types and register allocations (`src/context.prolog`)
  * simplification of simple boolean expressions (`src/simplify.prolog`)

Some builtin functions are compiled from C, those can be found in `runtime/lib.c`, and compiled by running `make`.


### Optimization

#### Constant / copy propagation

In order to comply with SSA, the compiler keeps track of all constants and copy
instructions, automatically avoiding unnecessary assignments and copies.

Examples: `examples/opts/const*` and `examples/opts/copy*`


#### Use of phi for loops and conditionals

Instead of allocating variables, the compiler uses `phi`s to implement
`if` and `while` statements. It will detect which variables are changing
to avoid creating unnecessary `phi`s.
It will also try to clean up ones discovered to be redundant during optimization.

Examples: `examples/opts/if*` and `examples/opts/while*`


#### Global common subexpression optimization

The compiler will avoid recalculating instructions that don't have side effects.

Examples: `examples/opts/gcse*`
