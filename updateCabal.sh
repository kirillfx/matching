#!/bin/sh
cabal2nix --sha256=0 . > default.nix
nix-shell --command "cabal clean && cabal configure; return"
