#!/bin/sh
#
# Run ormolu on the input directories
#
# Example command:
#
#   ./run-formatter.sh src unittests

find "$@" -name '*.hs' -print -exec ormolu --mode inplace --check-idempotence {} \;
