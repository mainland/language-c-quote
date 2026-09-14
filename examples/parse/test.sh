#!/usr/bin/env bash
set -euo pipefail

fail() {
    printf '%s\n' "$*" >&2
    exit 1
}

find_program() {
    local program
    program=$(command -v "$1") || fail "Executable not found: $1"
    [[ -f $program && -x $program ]] || fail "Not an executable file: $program"
    printf '%s/%s\n' "$(cd -- "$(dirname -- "$program")" && pwd -P)" "${program##*/}"
}

if [[ $# == 0 ]]; then
    fail 'Usage: examples/parse/test.sh FILE.i ... (overrides: CABAL, GHC, CC)'
fi

check_cabal=$(find_program "${CABAL:-cabal}")
check_ghc=$(find_program "${GHC:-ghc}")
check_cc=$(find_program "${CC:-cc}")
check_root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/../.." && pwd -P)

# Resolve inputs and executable overrides before changing directory.
check_sources=()
for source_file in "$@"; do
    [[ -f $source_file ]] || fail "Input file not found: $source_file"
    check_sources+=("$(cd -- "$(dirname -- "$source_file")" && pwd -P)/${source_file##*/}")
done

check_tmp=$(mktemp -d "${TMPDIR:-/tmp}/language-c-quote-parse.XXXXXX")
trap 'rm -rf -- "$check_tmp"' EXIT
trap 'exit 130' INT
trap 'exit 143' TERM

cd -- "$check_root"
check_args=(-fdevelopment-tools "--with-compiler=$check_ghc" exe:parse-c)
"$check_cabal" build "${check_args[@]}"
check_parser=$("$check_cabal" list-bin -v0 "${check_args[@]}")

for source_file in "${check_sources[@]}"; do
    printf 'Check parsing and C compilation: %s\n' "$source_file"
    "$check_parser" --gcc --blocks --print "$source_file" > "$check_tmp/pretty.c"
    "$check_cc" -x cpp-output -c "$source_file" -o "$check_tmp/original.o"
    "$check_cc" -x c -c "$check_tmp/pretty.c" -o "$check_tmp/pretty.o"
done

printf 'Passed %s preprocessed C files.\n' "${#check_sources[@]}"
