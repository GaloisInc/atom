#!/bin/bash

FLAGS=""
if [[ "${1:-}" == "develop" ]]; then
    FLAGS="-f develop"
fi

if [ ! -d .cabal-sandbox ]; then
    echo "Building new sandbox..."
    cabal sandbox init
    cabal install --only-dependencies --enable-tests
fi

cabal configure $FLAGS
cabal build
cabal test --show-detail=always
