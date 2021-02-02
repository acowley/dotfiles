{ stdenv, lib, fetchFromGitHub, python27, zlib, autoconf, wget
, subversion, git, which, darwin
, sctk, sph2pipe, openfst, atlas, openblas }:

stdenv.mkDerivation rec {
  name = "kaldi-2017-01-08";
  src = fetchFromGitHub {
    owner = "kaldi-asr";
    repo = "kaldi";
    rev = "5cdbd1879747d40d29f5099fd51270355102b92b";
    sha256 = "0cysvzpv3q43g8dprk2ipvx9ml20mwsj7kbh9c31c4q0rx521bph";
  };
  buildInputs = [ python27 zlib autoconf wget which git subversion
    sctk sph2pipe openfst openblas atlas ]
    ++ lib.optional stdenv.isDarwin darwin.cctools;

  postPatch = ''
    sed -e 's/# CXX = clang++/CXX = clang++/' \
        -e 's/CC = gcc/CC = clang/' \
        -e 's|extras/check_dependencies.sh|true|' \
        -i ./tools/Makefile

    sed -e 's|SHELL := /bin/bash|SHELL := ${stdenv.shell}|' \
        -i ./src/Makefile
  '';
  configurePhase = ''
    cd src && ./configure --shared --fst-root=${openfst} --openblas-root=${openblas} --atlas-root=${atlas}
  '';
  buildPhase = ''
    cd src && make
  '';

  meta = with lib; {
    description = "Kaldi Speech Recognition Toolkit";
    homepage = http://kaldi-asr.org/;
    license = licenses.apsl20;
    maintainers = [ maintainers.acowley ];
    platforms = platforms.unix;
  };
}
