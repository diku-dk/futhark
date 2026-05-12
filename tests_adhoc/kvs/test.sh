#!/bin/sh
#
# Test part of the RTS in an ad-hoc way.

set -ex

gcc testkvs.c -I../../ -o testkvs -g -fsanitize=address -fsanitize=undefined -Wall -Wextra -Werror -pedantic -Wno-unused-function

./testkvs
