{ stdenv, llvmPackages, fetchFromGitHub, cmake, pkgconfig, python, git, libunwind, rocr, rocminfo }:
let
  gcc = if stdenv.cc.isGNU then stdenv.cc.cc else stdenv.cc.cc.gcc;
  release_version = "6.0.0";
in
llvmPackages.stdenv.mkDerivation rec {
  name = "hcc-clang";
  version = "1.7.1";
  tag = "roc-${version}";
  src = fetchFromGitHub {
    owner = "RadeonOpenCompute";
    repo = "hcc";
    rev = tag;
    sha256 = "14f3xfil15vs3dgaxzsha349khyyhihc15bhf2n0jcskljygs7ag";
    fetchSubmodules = true;
  };
  nativeBuildInputs = [ cmake pkgconfig python ];
  buildInputs = [ libunwind rocr ];
  cmakeFlags = [
    "-DCMAKE_CXX_FLAGS=-std=c++11"
    "-DCMAKE_BUILD_TYPE=Release"
  ]
  ++ stdenv.lib.optional stdenv.isLinux "-DGCC_INSTALL_PREFIX=${gcc}"
  ++ stdenv.lib.optional (stdenv.cc.libc != null) "-DC_INCLUDE_DIRS=${stdenv.cc.libc}/include";

  # - Stop the build after the clang and rocdl builds
  # - Fix bash paths in various tool scripts
  # - Let the hcc driver find rocm_agent_enumerator to avoid needing
  #   --amdgpu-target arguments on every compiler invocation
  preConfigure = ''
    sed '/^add_subdirectory(hcc_config)/,$'d -i ./CMakeLists.txt
    for f in $(find lib -name '*.in'); do
      sed 's_#!/bin/bash_#!${stdenv.shell}_' -i "$f"
    done
    sed 's,^\([[:space:]]*const char\* tmp = \)std::getenv("ROCM_ROOT");,\1"${rocminfo}";,' -i ./clang/lib/Driver/ToolChains/Hcc.cpp
  '';

  postInstall = ''
    ln -sv ${llvmPackages.llvm}/lib/LLVMgold.so $out/lib
    ln -sv $out/bin/clang $out/bin/cpp
  '';

  passthru = {
    isClang = true;
    inherit (llvmPackages) llvm;
  } // stdenv.lib.optionalAttrs stdenv.isLinux {
    inherit gcc;
  };

  hardeningUnsupportedFlags = ["stackprotector"];
  hardeningDisable = ["all"];
}
