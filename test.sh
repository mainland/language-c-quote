#!/bin/sh
set -e
ghc --make -idist/build -itests -odir tmp -hidir tmp tests/Main.hs
./tests/Main
