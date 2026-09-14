# Contributing

Use Cabal to build and test the library:

```sh
cabal build all
cabal test all --test-show-details=direct
```

Supported compilers start at GHC 8.0. The tested versions are listed in
`language-c-quote.cabal`. Use Cabal's `-w` option to select another compiler.
Cabal manages dependencies and generates the lexer and parser with Alex and Happy.

Test both antiquote parsers when changing quotation support or its tests:

```sh
cabal test all -ffull-haskell-antiquotes --test-show-details=direct
cabal test all -f-full-haskell-antiquotes --test-show-details=direct
```

The default enables full Haskell expressions and patterns through
`haskell-src-meta`. Disabling the flag uses the simpler `haskell-exp-parser`.
Keep shared tests within the simpler parser's syntax, with type annotations in
ordinary Haskell bindings. Put tests requiring full Haskell syntax in
`tests/unit/MainCPP.hs`, guarded by `FULL_HASKELL_ANTIQUOTES`.

The unit suite uses Tasty with `tasty-hunit`. List cases or run a selected group
by passing options to the test executable:

```sh
cabal test unit --test-option=--list-tests --test-show-details=direct
cabal test unit --test-option='--pattern=/Source locations/' --test-show-details=direct
```

## Continuous integration

The generated GitHub Actions workflow checks the default configuration and the
`simple-haskell-antiquotes` constraint set across the supported compiler matrix.
The constraint set in `cabal.haskell-ci` disables full Haskell antiquotes and
enables both building and running the tests.

The `development-tools` constraint set builds the optional instance generator
and example parser from the source archive across the same compiler matrix.

CI lets Cabal select `containers`, `binary`, and `text` instead of requiring
the copies bundled with GHC. This lets newer compilers build with published
pretty-printer versions whose dependency bounds require older `containers`.

After changing `tested-with` or `cabal.haskell-ci`, regenerate the workflow with
the haskell-ci version recorded in `.github/workflows/haskell-ci.yml`:

```sh
haskell-ci regenerate
```

Review the generated diff alongside the configuration change.

## Formatting

Format hand-written Haskell modules with Stylish Haskell:

```sh
stylish-haskell -i path/to/Module.hs
```

The repository configuration matches the sibling libraries' import and pragma
style, expands tabs to eight spaces, and removes trailing whitespace. VS Code
uses the same formatter on save and inserts four-space indentation.

Review formatting changes inside string literals and quasiquotes, where
whitespace may be significant. Do not run the formatter on the Alex and Happy
inputs (`.x` and `.y`), build outputs, or the generated
`src/Language/C/Syntax-instances.hs` include. Keep generated instances in sync
through the generator below.

Place CPP conditionals around complete definitions when possible so the
formatter can parse the source without evaluating the conditionals.

## Development tools

The manual `development-tools` flag enables two optional executables. It is
disabled by default.

```sh
cabal build -fdevelopment-tools exe:gen-instances exe:parse-c
```

The helper scripts below require Bash and a Cabal version with `cabal list-bin`.
`CABAL` and `GHC` select executables and default to `cabal` and `ghc`. Each
override is an executable name or path, not a command with arguments.

### Generate location instances

After changing AST constructors or the instance generator, run:

```sh
bash scripts/regenerate-instances.sh
GHC=ghc-8.6.5 bash scripts/regenerate-instances.sh
```

The script builds the generator from the local `src/Language/C/Syntax.hs` with
`ONLY_TYPEDEFS`, so it does not need the generated instances to exist or compile.
It generates into a temporary file and replaces
`src/Language/C/Syntax-instances.hs` only after successful generation. Unchanged
output leaves the tracked file untouched. Review the generated diff alongside
the AST and generator changes.

Additional script arguments are passed to both Cabal commands, for example
`--offline` or `--project-file=/path/to/cabal.project`.

### Parse and pretty-print C

```sh
cabal run -fdevelopment-tools exe:parse-c -- --gcc input.i --print
```

The example parses each input and checks a parse/print/parse round trip. It can
also print tokens (`--tokens`) or emit source directives (`--print --pragma`).
Parsing and round-trip failures return an unsuccessful exit status.
For output redirection, build first and run the executable directly:

```sh
cabal build -fdevelopment-tools exe:parse-c
parser=$(cabal list-bin -fdevelopment-tools exe:parse-c)
"$parser" --gcc input.i --print > output.c
```

This keeps Cabal's build messages out of the generated C file.

An optional integration check compiles both supplied preprocessed C files and
their pretty-printed output:

```sh
cc -E source.c -o source.i
bash examples/parse/test.sh source.i
```

Pass one or more preprocessed input files. `CC` selects the C compiler and
defaults to `cc`. It must accept GCC-style compilation options. C object files
and rendered C are kept in a private temporary directory and removed on exit.
This check is separate from the ordinary unit suite and needs no downloaded
Apache source tree.

## Package checks

```sh
cabal check
cabal haddock all
cabal sdist
```

Check the extracted source archive too. Build and run both optional tools there,
and confirm instance regeneration still works without a pre-existing instance
include. Do not publish a release until the supported CI matrix passes.
