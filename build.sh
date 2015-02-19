#!/usr/bin/env bash

HERE="$(pwd)"

cmake -GUnix\ Makefiles . &&
make &&
#FIXME: don't use absolute paths!
cabal configure --extra-include-dirs="$HERE"/include/ --extra-lib-dirs="$HERE" &&
cabal build
