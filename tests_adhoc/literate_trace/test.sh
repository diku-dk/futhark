#!/bin/sh

set -e

rm -rf trace-img
futhark literate trace.fut -v | tee | fgrep 'trace.fut:1:22-32: 1.000000 2.000000 3.000000'
