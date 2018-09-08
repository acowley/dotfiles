{ stdenv, fetchFromGitHub, cmake, llvmPackages_6 }:
llvmPackages_6.stdenv.mkDerivation rec {
  name = "ccls-${version}";
  version = "20180904";
  src = fetchFromGitHub {
    owner = "MaskRay";
    repo = "ccls";
    rev = "0a51424c5cdcd32be128129150e34d32462f3725";
    sha256 = "16w523bdsb3i70dnn03w0ssvj3pwhpqvaf62w8rzb2735p4l971g";
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
