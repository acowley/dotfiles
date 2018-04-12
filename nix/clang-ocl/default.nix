{ stdenv, fetchFromGitHub, cmake, rocm-cmake, rocm-opencl-runtime, hcc }:
stdenv.mkDerivation {
  name = "clang-ocl";
  version = "2018-01-08";
  src = fetchFromGitHub {
    owner = "RadeonOpenCompute";
    repo = "clang-ocl";
    rev = "c1b678e1706cefbdd3b62e311203f3b492a0d17d";
    sha256 = "0hma3gg887wswz11gvqll4k0wcp604y76h39wz0nzy8j9sh5xjgf";
  };
  nativeBuildInputs = [ cmake rocm-cmake rocm-opencl-runtime ];
  cmakeFlags = [
    "-DOPENCL_ROOT=${rocm-opencl-runtime}"
    "-DCLINFO=${rocm-opencl-runtime}/bin/clinfo"
  ];
  patchPhase = ''
    CLANGVER=$(ls ${rocm-opencl-runtime}/lib/clang)
    sed -e 's,^BITCODE_DIR=.*$,BITCODE_DIR=${rocm-opencl-runtime}/lib,' \
        -e 's,^CLANG=.*$,CLANG=${hcc}/bin/clang,' \
        -e 's,^LLVM_LINK=.*$,LLVM_LINK=${hcc}/bin/llvm-link,' \
        -e "s,\''${OPENCL_ROOT}/include/opencl-c.h,${rocm-opencl-runtime}/lib/clang/$CLANGVER/include/opencl-c.h," \
        -e 's,#!/bin/bash,#!${stdenv.shell},' \
        -i clang-ocl.in
  '';
}
