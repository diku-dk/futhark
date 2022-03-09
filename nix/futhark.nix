{ stdenv
, lib
, haskellPackages
, commit ? ""
, ...}:

let
  futhark = haskellPackages.futhark;
in
stdenv.mkDerivation {
  name = "futhark";
  version = futhark.version; # TODO: this does not reflect the actual version of futhark
  src = ../tools/release;

  buildInputs = [ futhark ];

  buildPhase = ''
    runHook preBuild

    cp -r skeleton futhark
    cp -r ${futhark}/bin futhark/bin
    mkdir -p futhark/share
    cp -r ${futhark}/share/man futhark/share/
    chmod +w -R futhark
    cp ${futhark}/share/futhark/LICENSE futhark
    [ "${commit}" ] && echo "${commit}" > futhark/commit-id

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    cp -r futhark $out/

    runHook postInstall
  '';
}
