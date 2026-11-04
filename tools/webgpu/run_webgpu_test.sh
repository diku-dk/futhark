#!/usr/bin/env bash
#
# A smoke test of the WebGPU backend in a real browser on a real GPU: a few
# small programs are run in headless Chrome by tools/browser_test.py.  Expects
# the environment set up by setup_webgpu_ci.sh.
set -euo pipefail

cd "$(dirname "$0")/../.."

# browser_test.py serves on a fixed port, so only one program can run at a
# time.
futhark test --backend=webgpu --runner=tools/browser_test.py \
  --concurrency=1 --no-tuning \
  tests/soacs/map1.fut \
  tests/soacs/reduce1.fut \
  tests/soacs/scan1.fut \
  tests/primitive/f16_binop.fut \
  tests/hist/f16.fut
