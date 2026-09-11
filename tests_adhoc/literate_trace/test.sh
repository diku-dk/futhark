#!/bin/sh

set -e

rm -rf trace-img
futhark literate trace.fut -v | tee | fgrep 'trace.fut:1:22-32: [1.0, 2.0, 3.0]'
