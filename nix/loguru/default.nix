{ stdenv, fetchFromGitHub, python, git, llvmPackages }:
stdenv.mkDerivation rec {
  name = "loguru-${version}";
  version = "2017-12-05";

  src = fetchFromGitHub {
    owner = "emilk";
    repo = "loguru";
    rev = "83b6f3c3d16e40453ec0d12d3baef42cd2f37c3b";
    sha256 = "19lny76l0azswg67l0dihjvyx506rw5w3h68rzlsnjaq2kp85g5f";
    fetchSubmodules = true;
  };

  buildInputs = [];

  builder = builtins.toFile "builder.sh" ''
    source $stdenv/setup
    mkdir -p $out/include
    cp $src/loguru.hpp $out/include
  '';

  meta = {
    description = "A lightweight C++ logging library";
    homepage = https://github.com/emilk/loguru;
    license = stdenv.lib.licenses.publicDomain;
    platforms = stdenv.lib.platforms.all;
  };
}
