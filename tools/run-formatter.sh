#!/bin/sh
#
# Run ormolu on the input directories
#
# Example command:
#
#   ./run-formatter.sh src src-testing

find "$@" -name '*.hs' -print -exec ormolu --mode inplace --check-idempotence {} \;
