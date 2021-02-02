{ stdenv, lib, fetchFromGitHub, writeText, python3 }:
stdenv.mkDerivation {
  name = "zenstates";
  version = "2017-05-07";
  src = fetchFromGitHub {
    owner = "r4m0n";
    repo = "ZenStates-Linux";
    rev = "0bc27f4740e382f2a2896dc1dabfec1d0ac96818";
    sha256 = "1h1h2n50d2cwcyw3zp4lamfvrdjy1gjghffvl3qrp6arfsfa615y";
  };
  builder = writeText "builder.sh" ''
    source $stdenv/setup
    mkdir -p $out/bin
    sed 's,#!/usr/bin/python,#!${python3}/bin/python,' $src/zenstates.py > $out/bin/zenstates
    chmod u+x $out/bin/zenstates
  '';
  meta = {
    description = "Dynamically edit AMD Ryzen processor P-States";
    longDescription = "Uses the msr kernel module to edit P-states and toggle C6 support";
    homepage = https://github.com/r4m0n/ZenStates-Linux;
    license = lib.licenses.mit;
  };
}
