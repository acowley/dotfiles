{ stdenv, fetchFromGitHub, cmake, llvmPackages_6 }:
llvmPackages_6.stdenv.mkDerivation rec {
  name = "ccls-${version}";
  version = "20180730";
  src = fetchFromGitHub {
    owner = "MaskRay";
    repo = "ccls";
    rev = "924fedbb02be07e19fd2a5372a09e89779973d41";
    sha256 = "01lgfk3x0n3f40h83bps8h515n4psynsvy40g1a3ry1r0lns87c6";
    fetchSubmodules = true;
  };
  enableParallelBuilding = true;
  nativeBuildInputs = [ cmake ];
  buildInputs = [ llvmPackages_6.llvm llvmPackages_6.libclang
                  llvmPackages_6.libclang.out ];
  cmakeFlags = [
    "-DSYSTEM_CLANG=ON"
    "-DUSE_SHARED_LLVM=ON"
  ];

  meta = {
    description = "C/C++/ObjC language server";
    homepage = https://github.com/MaskRay/ccls;
    license = stdenv.lib.licenses.mit;
    platforms = stdenv.lib.platforms.all;
  };
}
