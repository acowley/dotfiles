{ stdenv, fetchFromGitHub, cmake, llvmPackages_8 }:
llvmPackages_8.stdenv.mkDerivation rec {
  name = "ccls-${version}";
  version = "20190513";
  src = fetchFromGitHub {
    owner = "MaskRay";
    repo = "ccls";
    rev = "d3808de26ac00b36263aac73d8f64432472dff88";
    sha256 = "0j55237bhkwddb8fa3iplhzbc9i93pxk3mxywhgfd2cwbf911dzq";
    fetchSubmodules = true;
  };
  enableParallelBuilding = true;
  nativeBuildInputs = [ cmake ];
  buildInputs = [ llvmPackages_8.llvm llvmPackages_8.libclang
                  llvmPackages_8.libclang.out ];

  preConfigure =''
    sed 's/if(NOT LLVM_ENABLE_RTTI)/if(1)/' -i CMakeLists.txt
  '';

  meta = {
    description = "C/C++/ObjC language server";
    homepage = https://github.com/MaskRay/ccls;
    license = stdenv.lib.licenses.asl20;
    platforms = stdenv.lib.platforms.all;
  };
}
