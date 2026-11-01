#!/bin/sh

set -e

cc -o test -Wall -Wextra -pedantic -Werror -Wno-unused-function -I ../../rts/c test.c -fsanitize=undefined -fsanitize=address -g
./test
