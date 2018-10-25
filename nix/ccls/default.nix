{ stdenv, fetchFromGitHub, cmake, llvmPackages_7 }:
llvmPackages_7.stdenv.mkDerivation rec {
  name = "ccls-${version}";
  version = "20181024";
  src = fetchFromGitHub {
    owner = "MaskRay";
    repo = "ccls";
    rev = "323d2ec8bf0df063810461199d2063832a322131";
    sha256 = "0gqcgj1qjbbcz6z8cssf0d0jx3pm3ny4p3z9gyjaaj23qi40l1yw";
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

  meta = {
    description = "C/C++/ObjC language server";
    homepage = https://github.com/MaskRay/ccls;
    license = stdenv.lib.licenses.asl20;
    platforms = stdenv.lib.platforms.all;
  };
}
