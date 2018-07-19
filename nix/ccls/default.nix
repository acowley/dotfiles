{ stdenv, fetchFromGitHub, cmake, llvmPackages_6 }:
llvmPackages_6.stdenv.mkDerivation rec {
  name = "ccls-${version}";
  version = "20180718";
  src = fetchFromGitHub {
    owner = "MaskRay";
    repo = "ccls";
    rev = "0732d37817a97a53708a0e598aae3738d5ed03ec";
    sha256 = "0sks74r55x1mfcv4gqf1m6snijwkj37649wgw6hnfy912garlb0k";
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
