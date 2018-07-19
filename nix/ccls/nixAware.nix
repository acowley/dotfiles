{ stdenv, llvmPackages_6, ccls, makeWrapper }:
{ buildInputs }:
llvmPackages_6.stdenv.mkDerivation {
  name = "project-ccls";
  buildInputs = buildInputs ++ [ ccls makeWrapper ];
  phases = ["installPhase"];

  #  --add-flags "--log-file=/tmp/cc.log"
  installPhase = ''
    extraincs=$(($CXX -xc++ -E -v /dev/null) 2>&1 | awk 'BEGIN { incsearch = 0} /^End of search list/ { incsearch = 0 } { if(incsearch) { print $0 }} /^#include </ { incsearch = 1 }' | sed 's/^[[:space:]]*\(.*\)/"-I\1"/' | tr '\n' ' ' | tr ' ' ',' | sed 's/,$//')
    mkdir -p $out/bin
    makeWrapper "${ccls}/bin/ccls" $out/bin/ccls --add-flags "--init='{\"clang\": {\"extraArgs\": [''${extraincs}]}}'"
  '';
}
