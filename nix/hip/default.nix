{ stdenv, llvmPackages, fetchFromGitHub, cmake, writeText, perl, hcc, rocr, roct, rocminfo }:
stdenv.mkDerivation rec {
  name = "hip";
  version = "1.7.0";
  tag = "roc-${version}";
  src = fetchFromGitHub {
    owner = "ROCm-Developer-Tools";
    repo = "HIP";
    rev = tag;
    sha256 = "1jds1d45wa0qqns907z50zwzmpygl94yc6qn52b8sdll3klgf7l1";
  };
  propagatedBuildInputs = [ hcc roct ];
  buildInputs = [ rocminfo ];
  nativeBuildInputs = [ cmake ];
  cmakeFlags = [ "-DHSA_PATH=${rocr}" "-DHCC_HOME=${hcc}" "-DCMAKE_CURRENT_SOURCE_DIR=${src}"];
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

  # This patch-level version number gets lost in the nix build, so we
  # provide it ourselves. Setting HIP_VERSION_PATCH to the same number
  # may not be correct.
  postConfigure = ''
    export NIX_CFLAGS_COMPILE+=' -D__hcc_workweek__=17503 -DHIP_VERSION_PATCH=17503 -Wno-macro-redefined'
  '';

  setupHook = writeText "setupHook.sh" ''
    export NIX_CFLAGS_COMPILE+=' -D__hcc_workweek__=17503 -Wno-macro-redefined'
    export HIP_PATH="@out@"
  '';
}
