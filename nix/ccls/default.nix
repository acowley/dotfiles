{ stdenv, fetchFromGitHub, cmake, llvmPackages_7 }:
llvmPackages_7.stdenv.mkDerivation rec {
  name = "ccls-${version}";
  version = "20190102";
  src = fetchFromGitHub {
    owner = "MaskRay";
    repo = "ccls";
    rev = "9f3305157f0f8d64dcebcc0ad362df71c3eb91f7";
    sha256 = "024qli2dm179drl969rf8i0zy4z0gzyy7lv0ml905ylbsykgzzrv";
    fetchSubmodules = true;
    # date = 2019-01-03T00:37:10+08:00;
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
