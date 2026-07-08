#!/bin/sh
#
# Check that we don't have any warnings in the generated C code. Obviously quite
# fragile, but we do our best.

set -e

BACKEND=c

futhark $BACKEND --server program.fut

cc -c program.c -Wall -Wextra -pedantic -Werror
