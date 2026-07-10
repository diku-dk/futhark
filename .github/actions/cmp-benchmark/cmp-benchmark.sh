#!/bin/bash
set -e

BACKEND=$1
SYSTEM=$2

echo "::group::Installing nightly master compiler"
curl -L https://futhark-lang.org/releases/futhark-nightly-linux-x86_64.tar.xz \
  -o futhark-nightly-master.tar.xz
mkdir -p /tmp/futhark-master
tar xf futhark-nightly-master.tar.xz -C /tmp/futhark-master --strip-components=1
make -C /tmp/futhark-master/ install PREFIX=$HOME/.local-master
echo "::endgroup::"

# Pin the benchmark suite to the commit used by the master compiler so both
# compilers are compared against an identical suite.
MASTER_SHA=$(cat /tmp/futhark-master/commit-id)
BENCH_SHA=$(git ls-tree "$MASTER_SHA" futhark-benchmarks | awk '{print $3}')
git -C futhark-benchmarks checkout "$BENCH_SHA"

echo "::group::Downloading external benchmark data"
module load perl
(cd futhark-benchmarks && ./get-data.sh external-data.txt)
echo "::endgroup::"

hostname
module unload cuda
module load cuda/11.8

echo "::group::Benchmarking with master compiler"
$HOME/.local-master/bin/futhark bench futhark-benchmarks \
  --backend "$BACKEND" \
  --exclude "no_$SYSTEM" \
  --json "old-$BACKEND.json" \
  --ignore-files /lib/
echo "::endgroup::"

echo "::group::Benchmarking with PR compiler"
futhark bench futhark-benchmarks \
  --backend "$BACKEND" \
  --exclude "no_$SYSTEM" \
  --json "new-$BACKEND.json" \
  --ignore-files /lib/
echo "::endgroup::"

echo "::group::Sorted by significant regressions"
futhark benchcmp --sort-by=significant "old-$BACKEND.json" "new-$BACKEND.json"
echo "::endgroup::"

echo "::group::Sorted by geomean - significant only"
futhark benchcmp --sort-by=geomean-significant "old-$BACKEND.json" "new-$BACKEND.json"
echo "::endgroup::"

echo "::group::Sorted by geomean - all datasets"
futhark benchcmp --sort-by=geomean-all "old-$BACKEND.json" "new-$BACKEND.json"
echo "::endgroup::"
