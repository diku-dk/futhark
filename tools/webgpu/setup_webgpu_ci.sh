#!/usr/bin/env bash
set -euo pipefail

BASE="${HOME}/fut-webgpu-ci"

export PATH="${HOME}/.local/bin:${PATH}"
export XDG_CACHE_HOME="${HOME}/.cache"

mkdir -p "${BASE}" "${HOME}/opt" "${HOME}/.local/lib" "${XDG_CACHE_HOME}"

if ! type module >/dev/null 2>&1; then
  [ -f /etc/profile.d/modules.sh ] && source /etc/profile.d/modules.sh
  [ -f /usr/share/Modules/init/bash ] && source /usr/share/Modules/init/bash
fi

module purge || true
module load gcc/11.2.0
module load gmp/6.2.1
module load perl/5.38.0
module load python/3.12.8

python3 --version
which python3

# emsdk
if [ ! -d "${HOME}/opt/emsdk" ]; then
  git clone https://github.com/emscripten-core/emsdk.git "${HOME}/opt/emsdk"
else
  # The release list is part of the checkout, so it must be updated
  # before a newer version can be installed.
  git -C "${HOME}/opt/emsdk" pull --ff-only
fi

pushd "${HOME}/opt/emsdk" >/dev/null
./emsdk install 6.0.9
./emsdk activate 6.0.9
export EMSDK_QUIET=1
source "${HOME}/opt/emsdk/emsdk_env.sh"
popd >/dev/null

# GMP workaround
if [ ! -e "${HOME}/.local/lib/libgmp.so" ] && [ -e /usr/lib64/libgmp.so.10 ]; then
  ln -sf /usr/lib64/libgmp.so.10 "${HOME}/.local/lib/libgmp.so"
fi
export LIBRARY_PATH="${HOME}/.local/lib:${LIBRARY_PATH:-}"
export LD_LIBRARY_PATH="${HOME}/.local/lib:${LD_LIBRARY_PATH:-}"

# Python packages for tools/browser_test.py.  Selenium downloads Chrome and
# its driver itself.
if [ ! -x "${BASE}/venv/bin/python3" ]; then
  python3 -m venv "${BASE}/venv"
fi
source "${BASE}/venv/bin/activate"
pip install --quiet --upgrade aiohttp numpy selenium

echo "OK: workspace ready at ${BASE}"
echo "OK: futhark is $(command -v futhark)"
echo "OK: emcc is $(command -v emcc)"
echo "OK: python3 is $(command -v python3)"
