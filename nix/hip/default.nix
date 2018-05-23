{ stdenv, llvmPackages, fetchFromGitHub, cmake, git, writeText, perl,
  hcc, rocr, roct, rocminfo }:
stdenv.mkDerivation rec {
  name = "hip";
  version = "1.8.0";
  tag = "roc-${version}";
  src = fetchFromGitHub {
    owner = "ROCm-Developer-Tools";
    repo = "HIP";
    # rev = tag;

    rev = "323a6226b0a6ed43b43918b351fd64bb03d53c98";
    sha256 = "1wxxqsgrg01arzig69vf9m3g9kvpfky0ispdgccmrg44h623j418";
  };
  propagatedBuildInputs = [ hcc roct ];
  buildInputs = [ rocminfo ];
  nativeBuildInputs = [ cmake git ];
  cmakeFlags = [ "-DHSA_PATH=${rocr}" "-DHCC_HOME=${hcc}" "-DCMAKE_CURRENT_SOURCE_DIR=${src}" ];
  patchPhase = ''
    for f in $(find bin -type f); do
      sed -e 's,#!/usr/bin/perl,#!${perl}/bin/perl,' \
          -e 's,#!/bin/bash,#!${stdenv.shell},' \
          -i "$f"
    done
    sed -e 's,$ROCM_AGENT_ENUM = "''${ROCM_PATH}/bin/rocm_agent_enumerator";,$ROCM_AGENT_ENUM = "${rocminfo}/bin/rocm_agent_enumerator";,' \
        -e 's,^\([[:space:]]*$HSA_PATH=\).*$,\1"${rocr}";,' \
        -e 's,^\([[:space:]]*$HCC_HOME=\).*$,\1"${hcc}";,' \
        -i bin/hipcc
    sed -i 's,\([[:space:]]*$HCC_HOME=\).*$,\1"${hcc}";,' -i bin/hipconfig
  '';

  setupHook = writeText "setupHook.sh" ''
    export HIP_PATH="@out@"
  '';
}
