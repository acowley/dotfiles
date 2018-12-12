{ stdenv, fetchFromGitHub, cmake, llvmPackages_7 }:
llvmPackages_7.stdenv.mkDerivation rec {
  name = "ccls-${version}";
  version = "20181209";
  src = fetchFromGitHub {
    owner = "MaskRay";
    repo = "ccls";
    rev = "f5604525cbea83570cea39b008a05dc5e277aa2a";
    sha256 = "0rv0kiivpnlwbz07z6sl8f3fqp97h6w14fs5jqx97qf3i0k1h5sm";
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
