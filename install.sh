#!/bin/bash

FLAGS=""
if [[ "${1:-}" == "develop" ]]; then
    FLAGS="-f develop"
fi

cabal sandbox init
cabal install --only-dependencies --enable-tests
cabal configure $FLAGS
cabal build
cabal test
