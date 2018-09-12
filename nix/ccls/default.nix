{ stdenv, fetchFromGitHub, cmake, llvmPackages_6 }:
llvmPackages_6.stdenv.mkDerivation rec {
  name = "ccls-${version}";
  version = "20180912";
  src = fetchFromGitHub {
    owner = "MaskRay";
    repo = "ccls";
    rev = "536b245883663bac75922c0fe9486e15ed4a0aa9";
    sha256 = "0gqy96b8j5az848br9ip5ys699bpq9r6q15y806y4zgjjk322f1w";
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
    license = stdenv.lib.licenses.asl20;
    platforms = stdenv.lib.platforms.all;
  };
}
