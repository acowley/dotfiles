{ stdenv, clang, fetchFromGitHub, cmake }:
let SHLIB = if stdenv.isDarwin then "dylib" else "so"; in
stdenv.mkDerivation {
  name = "irony-server-2016-08-24";
  # src = fetchFromGitHub {
  #   owner = "Sarcasm";
  #   repo = "irony-mode";
  #   rev = "11bdf09b998c54be59bac8df3368039de2c0aa19";
  #   sha256 = "1q0liagda4ch9ya5ic9flwprxv4wq23ws3kg00d4wg0y3x4czwhm";
  # };
  src = ../../../src/irony-mode;
  buildInputs = [ cmake ];
  preConfigure = "cd server";
  cmakeFlags = [
    "-DLIBCLANG_LIBRARY=${stdenv.cc.cc.lib}/lib/libclang.${SHLIB}"
    "-DLIBCLANG_INCLUDE_DIR=${stdenv.cc.cc.lib}/include"
  ];
}
