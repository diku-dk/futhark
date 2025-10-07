#!/bin/sh

set -ex

futhark script ./double.fut '$store "testout" (main [1,2,3])'

[ "$(futhark dataset <testout)" = "[2.0f32, 4.0f32, 6.0f32]" ]

echo Load/store of opaques

futhark script ./opaque.fut -e '$store "datafile" (mk_opaque true)'
futhark script ./opaque.fut -e 'unmk_opaque ($restore "opaque" "datafile")'
