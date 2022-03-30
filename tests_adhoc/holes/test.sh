#!/bin/sh

set -e
set -x

futhark check holes.fut

! futhark c holes.fut
