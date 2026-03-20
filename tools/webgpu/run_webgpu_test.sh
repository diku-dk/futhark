#!/usr/bin/env bash
set -euo pipefail

BASE="${HOME}/fut-webgpu-ci"
WEBGPU_DIR="${BASE}/futhark-webgpu"
TESTROOT="${WEBGPU_DIR}/examples/hendrix-ci"

cd "${TESTROOT}"

npm init -y
npm i playwright
npx playwright install chromium

node check-headless/check_webgpu.mjs
make -C test1 run
