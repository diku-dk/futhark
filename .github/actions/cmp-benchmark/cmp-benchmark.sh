#!/bin/bash
set -e

BACKEND=$1
SYSTEM=$2

# Install the master (nightly) compiler.
curl -L https://futhark-lang.org/releases/futhark-nightly-linux-x86_64.tar.xz \
  -o futhark-nightly-master.tar.xz
mkdir -p /tmp/futhark-master
tar xf futhark-nightly-master.tar.xz -C /tmp/futhark-master --strip-components=1
make -C /tmp/futhark-master/ install PREFIX=$HOME/.local-master

# Find the benchmark suite commit pinned by the master compiler.
MASTER_SHA=$(cat /tmp/futhark-master/commit-id)
BENCH_SHA=$(git ls-tree "$MASTER_SHA" futhark-benchmarks | cut -f3)
git -C futhark-benchmarks checkout "$BENCH_SHA"
cp -r futhark-benchmarks futhark-benchmarks-master
git -C futhark-benchmarks checkout -

# Download data for each suite independently, as they may differ.
module load perl
(cd futhark-benchmarks-master && ./get-data.sh external-data.txt)
(cd futhark-benchmarks && ./get-data.sh external-data.txt)

hostname
module unload cuda
module load cuda/11.8

$HOME/.local-master/bin/futhark bench futhark-benchmarks-master \
  --backend "$BACKEND" \
  --exclude "no_$SYSTEM" \
  --json "old-$BACKEND.json" \
  --ignore-files /lib/

futhark bench futhark-benchmarks \
  --backend "$BACKEND" \
  --exclude "no_$SYSTEM" \
  --json "new-$BACKEND.json" \
  --ignore-files /lib/

echo "::group::Sorted by significant regressions"
futhark benchcmp --sort-by=significant "old-$BACKEND.json" "new-$BACKEND.json"
echo "::endgroup::"

echo "::group::Sorted by geomean - significant only"
futhark benchcmp --sort-by=geomean-significant "old-$BACKEND.json" "new-$BACKEND.json"
echo "::endgroup::"

echo "::group::Sorted by geomean - all datasets"
futhark benchcmp --sort-by=geomean-all "old-$BACKEND.json" "new-$BACKEND.json"
echo "::endgroup::"
