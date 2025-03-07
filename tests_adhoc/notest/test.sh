#!/bin/sh

set -e

futhark test notest.fut
futhark test -i notest.fut
