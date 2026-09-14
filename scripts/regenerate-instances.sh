#!/usr/bin/env bash
set -euo pipefail

find_program() {
    local program
    program=$(command -v "$1") || {
        printf 'Executable not found: %s\n' "$1" >&2
        return 1
    }
    [[ -f $program && -x $program ]] || {
        printf 'Not an executable file: %s\n' "$program" >&2
        return 1
    }
    printf '%s/%s\n' "$(cd -- "$(dirname -- "$program")" && pwd -P)" "${program##*/}"
}

cabal_program=$(find_program "${CABAL:-cabal}")
ghc_program=$(find_program "${GHC:-ghc}")
repo_root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd -P)
cd -- "$repo_root"

"$cabal_program" build -w "$ghc_program" -fdevelopment-tools exe:gen-instances "$@"
generator=$("$cabal_program" list-bin -v0 -w "$ghc_program" -fdevelopment-tools exe:gen-instances "$@")

target=src/Language/C/Syntax-instances.hs
generated=$(mktemp "${target}.XXXXXX")
trap 'rm -f -- "$generated"' EXIT
trap 'exit 130' INT
trap 'exit 143' TERM

"$generator" > "$generated"
if ! cmp -s -- "$generated" "$target"; then
    chmod 644 "$generated"
    mv -- "$generated" "$target"
fi
