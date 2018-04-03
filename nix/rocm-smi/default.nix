{ stdenv, fetchFromGitHub, python3 }:
stdenv.mkDerivation rec {
  name = "rocm-smi";
  version = "1.7.1";
  tag = "roc-${version}";
  src = fetchFromGitHub {
    owner = "RadeonOpenCompute";
    repo = "ROC-smi";
    rev = tag;
    sha256 = "16jhk4x9giyq3zfsnj2vf02g2djms4w3avv2j3rxw7qlc3gg3p3q";
  };

  patchPhase = "sed 's,#!/usr/bin/python3,#!${python3}/bin/python3,' -i rocm-smi";
  buildPhase = null;
  installPhase = ''
    mkdir -p $out/bin
    cp rocm-smi $out/bin
  '';
}
