#!/bin/sh
#
# Run a program compiled with 'futhark webgpu --server' in Node, for use as a
# runner with 'futhark test' and 'futhark bench':
#
#   futhark test --backend=webgpu --runner=tools/webgpu/node-runner.sh prog.fut
#
# WebGPU is provided by Dawn, through the 'webgpu' npm package, which must be
# found through NODE_PATH (the '#test-webgpu' Nix shell sets this up).  To run
# on the CPU, without a GPU, point VK_DRIVER_FILES at the driver file of a
# software Vulkan implementation, such as Mesa's lavapipe (the '#test-webgpu'
# Nix shell has its path in LAVAPIPE_ICD).

prog=$1
shift
# Node treats a relative path given to --import as a package name.
here=$(cd "$(dirname "$0")" && pwd)
exec node --import "$here/node-gpu.mjs" "$prog.js" "$@"
