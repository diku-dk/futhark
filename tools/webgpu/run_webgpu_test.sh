#!/usr/bin/env bash
set -euo pipefail

BASE="${HOME}/fut-webgpu-ci"
WEBGPU_DIR="${BASE}/futhark-webgpu"
TESTROOT="${WEBGPU_DIR}/examples/hendrix-ci"

cd "${TESTROOT}"

if [ ! -f package.json ]; then
  npm init -y
fi

if [ ! -d node_modules/playwright ]; then
  npm i playwright
fi

if ! find "${PLAYWRIGHT_BROWSERS_PATH}" -maxdepth 1 -type d -name 'chromium-*' | grep -q .; then
  npx playwright install chromium
fi

node check-headless/check_webgpu.mjs
make -C test1 run
