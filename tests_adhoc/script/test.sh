#!/bin/sh

set -e

futhark script ./double.fut '$store "testout" (main [1,2,3])'

[ "$(futhark dataset <testout)" = "[2.0f32, 4.0f32, 6.0f32]" ]
