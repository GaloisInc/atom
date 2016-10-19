#!/bin/bash

cabal sandbox init
cabal install --only-dependencies --enable-tests
cabal build
cabal test
