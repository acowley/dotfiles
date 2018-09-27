{ stdenv, fetchFromGitHub, cmake, rocm-cmake, rocm-opencl-runtime, hcc }:
stdenv.mkDerivation {
  name = "clang-ocl";
  version = "2018-06-18";
  src = fetchFromGitHub {
    owner = "RadeonOpenCompute";
    repo = "clang-ocl";
    rev = "799713643b5591a3b877c586ef2c7fbc012af819";
    sha256 = "172wn8drixzxv4rlz5i33l31ixbmkn1nx7asm697pa86nw2lwdm0";
  };
  nativeBuildInputs = [ cmake rocm-cmake rocm-opencl-runtime hcc ];
  cmakeFlags = [
    "-DOPENCL_ROOT=${rocm-opencl-runtime}"
    "-DCLINFO=${rocm-opencl-runtime}/bin/clinfo"
  ];
  patchPhase = ''
    CLANGVER=$(ls ${hcc}/lib/clang)
    sed -e 's,^BITCODE_DIR=.*$,BITCODE_DIR=${hcc}/lib,' \
        -e 's,^CLANG=.*$,CLANG=${hcc}/bin/clang,' \
        -e 's,^LLVM_LINK=.*$,LLVM_LINK=${hcc}/bin/llvm-link,' \
        -e "s,\''${OPENCL_ROOT}/include/opencl-c.h,${hcc}/lib/clang/$CLANGVER/include/opencl-c.h," \
        -e 's,#!/bin/bash,#!${stdenv.shell},' \
        -i clang-ocl.in
  '';
}
