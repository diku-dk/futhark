#!/usr/bin/env bash
set -euo pipefail

: "${GITHUB_WORKSPACE:?GITHUB_WORKSPACE must be set}"

BASE="${HOME}/fut-webgpu-ci"
FUTHARK_DIR="${GITHUB_WORKSPACE}"
WEBGPU_DIR="${BASE}/futhark-webgpu"

export PATH="${HOME}/.local/bin:${PATH}"
export XDG_CACHE_HOME="${HOME}/.cache"
export PLAYWRIGHT_BROWSERS_PATH="${HOME}/pw-browsers"

mkdir -p "${BASE}" "${HOME}/opt" "${HOME}/.local/lib" "${XDG_CACHE_HOME}"

# Make 'module' available in non-interactive shells.
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

# ghcup + GHC/Cabal
if [ ! -f "${HOME}/.ghcup/env" ]; then
  curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | \
    BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
    BOOTSTRAP_HASKELL_MINIMAL=1 \
    sh
fi

# shellcheck source=/dev/null
source "${HOME}/.ghcup/env"

if ! ghcup whereis ghc 9.10.3 >/dev/null 2>&1; then
  ghcup install ghc 9.10.3
fi
ghcup set ghc 9.10.3

if ! command -v cabal >/dev/null 2>&1; then
  ghcup install cabal
fi
ghcup set cabal

# emsdk
if [ ! -d "${HOME}/opt/emsdk" ]; then
  git clone https://github.com/emscripten-core/emsdk.git "${HOME}/opt/emsdk"
fi

pushd "${HOME}/opt/emsdk" >/dev/null
./emsdk install 5.0.0
./emsdk activate 5.0.0
export EMSDK_QUIET=1
# shellcheck source=/dev/null
source "${HOME}/opt/emsdk/emsdk_env.sh"
popd >/dev/null

# GMP workaround
if [ ! -e "${HOME}/.local/lib/libgmp.so" ] && [ -e /usr/lib64/libgmp.so.10 ]; then
  ln -sf /usr/lib64/libgmp.so.10 "${HOME}/.local/lib/libgmp.so"
fi
export LIBRARY_PATH="${HOME}/.local/lib:${LIBRARY_PATH:-}"
export LD_LIBRARY_PATH="${HOME}/.local/lib:${LD_LIBRARY_PATH:-}"

# local Node.js + npm
NODE_VERSION="22.16.0"
NODE_DIR="${HOME}/opt/node-v${NODE_VERSION}-linux-x64"
NODE_BIN="${NODE_DIR}/bin"

if [ ! -x "${NODE_BIN}/node" ]; then
  TARBALL="node-v${NODE_VERSION}-linux-x64.tar.xz"
  URL="https://nodejs.org/dist/v${NODE_VERSION}/${TARBALL}"
  rm -rf "${NODE_DIR}"
  curl -fsSL -o "${HOME}/opt/${TARBALL}" "${URL}"
  tar -xf "${HOME}/opt/${TARBALL}" -C "${HOME}/opt"
fi

export PATH="${NODE_BIN}:${PATH}"

# Build/install the checked-out Futhark repo under test.
pushd "${FUTHARK_DIR}" >/dev/null
make configure
make build
make install
popd >/dev/null

# Clone the WebGPU examples repo for smoke testing.
if [ ! -d "${WEBGPU_DIR}/.git" ]; then
  git clone https://github.com/diku-dk/futhark-webgpu.git "${WEBGPU_DIR}"
else
  git -C "${WEBGPU_DIR}" fetch --all --prune
  git -C "${WEBGPU_DIR}" pull --ff-only || true
fi

echo "OK: workspace ready at ${BASE}"
echo "OK: futhark is $(command -v futhark)"
echo "OK: emcc is $(command -v emcc)"
echo "OK: node is $(command -v node)"
echo "OK: npm is $(command -v npm)"