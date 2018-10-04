{ stdenv, fetchFromGitHub, cmake, llvmPackages_6 }:
llvmPackages_6.stdenv.mkDerivation rec {
  name = "ccls-${version}";
  version = "20181003";
  src = fetchFromGitHub {
    owner = "MaskRay";
    repo = "ccls";
    rev = "c7ee3d85f3fa181acb4b3f66fb9472b02b4823b6";
    sha256 = "0npsbyj0x8bn6kpy831m2ajndlmiaa2zcycagjv9cyp198xfqnp2";
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
