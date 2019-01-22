{ stdenv, fetchFromGitHub, cmake, llvmPackages_7 }:
llvmPackages_7.stdenv.mkDerivation rec {
  name = "ccls-${version}";
  version = "20190116";
  src = fetchFromGitHub {
    owner = "MaskRay";
    repo = "ccls";
    rev = "7a79ed92b25f78f402410c37e39b755c5f8a3f81";
    sha256 = "0q98wcrwrmjsg0yzgw5ak1hd6h2zkbr6wh84i2b5w2slrnxs6zpa";
    fetchSubmodules = true;
    # date = 2019-01-16T00:04:24+08:00;
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
