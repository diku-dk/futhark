# The 'webgpu' npm package, which provides Node bindings for Dawn, Google's
# implementation of WebGPU.  We use it to run programs compiled with 'futhark
# webgpu' in Node (see tools/webgpu/node-runner.sh).  The package comes with
# prebuilt binaries, of which we keep only the one for this platform.
{
  lib,
  stdenv,
  fetchurl,
  writeText,
  autoPatchelfHook,
  vulkan-loader,
}:

let
  platform =
    {
      x86_64-linux = "linux-x64";
      aarch64-linux = "linux-arm64";
    }
    .${stdenv.hostPlatform.system};
in
stdenv.mkDerivation rec {
  pname = "node-webgpu";
  version = "0.6.2";

  src = fetchurl {
    url = "https://registry.npmjs.org/webgpu/-/webgpu-${version}.tgz";
    sha256 = "1z8g6dwq830v3ary34q3bpf8rczg0gk0qqi58klw5xxr2djvx9kp";
  };

  nativeBuildInputs = [ autoPatchelfHook ];
  buildInputs = [ stdenv.cc.cc.lib ];

  # Dawn loads the Vulkan loader with dlopen().
  appendRunpaths = [ "${lib.getLib vulkan-loader}/lib" ];

  # Make the package visible to Node in a shell that contains it.
  setupHook = writeText "setup-hook.sh" ''
    addToSearchPath NODE_PATH @out@/lib/node_modules
  '';

  installPhase = ''
    find dist -mindepth 1 -maxdepth 1 ! -name ${platform} -exec rm -r {} +
    mkdir -p $out/lib/node_modules/webgpu
    cp -r index.js package.json dist $out/lib/node_modules/webgpu
  '';

  meta = with lib; {
    description = "WebGPU for Node, using Dawn";
    homepage = "https://github.com/dawn-gpu/node-webgpu";
    license = licenses.mit;
    platforms = [
      "x86_64-linux"
      "aarch64-linux"
    ];
  };
}
