{ stdenv, fetchFromGitHub, cmake, llvmPackages_7 }:
llvmPackages_7.stdenv.mkDerivation rec {
  name = "ccls-${version}";
  version = "20190301";
  src = fetchFromGitHub {
    owner = "MaskRay";
    repo = "ccls";
    rev = "a5c06232159df4994d411448ba07f853d6a3d0f1";
    sha256 = "04bllpv57lni3z6v8jg74sxi3phzbf9y62qwpf66fcwrnmnyxp4v";
    fetchSubmodules = true;
  };
  enableParallelBuilding = true;
  nativeBuildInputs = [ cmake ];
  buildInputs = [ llvmPackages_7.llvm llvmPackages_7.libclang
                  llvmPackages_7.libclang.out ];
  cmakeFlags = [
    "-DSYSTEM_CLANG=ON"
    "-DUSE_SHARED_LLVM=ON"
  ];

  preConfigure = ''
  sed 's/\(_Clang_find_add_library(clangToolingCore)\)/_Clang_find_library(clangToolingInclusions_LIBRARY clangToolingInclusions)\nif (clangToolingInclusions_LIBRARY)\n  list(APPEND _Clang_LIBRARIES ''${clangToolingInclusions_LIBRARY})\nendif()\n\1/' -i cmake/FindClang.cmake
  '';

  meta = {
    description = "C/C++/ObjC language server";
    homepage = https://github.com/MaskRay/ccls;
    license = stdenv.lib.licenses.asl20;
    platforms = stdenv.lib.platforms.all;
  };
}
