#!/bin/sh
git submodule update --init
cp cabal.project.dist cabal.project
cabal configure
cabal install
cerridwen sim --target examples/target.o --query examples/query.o --summary examples/Labeled-Elfs-summary
