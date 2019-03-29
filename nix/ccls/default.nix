{ stdenv, fetchFromGitHub, cmake, llvmPackages_7 }:
llvmPackages_7.stdenv.mkDerivation rec {
  name = "ccls-${version}";
  version = "20190326";
  src = fetchFromGitHub {
    owner = "MaskRay";
    repo = "ccls";
    # rev = "e3c1adcf4847ebe91bbe524d5d0d1ebb0a1b14a7";
    # sha256 = "04is4bgnd4wfrpwm2d535qalrb6wmb5bf0kllhbxzzldi323bwh7";
    rev = "304f4d7f9e576529b61b1e86262221c5f195d124";
    sha256 = "1dpq210lh7i2l730dd6rid5h4drjgp2k2zgy0n26z5j7sj3w2j02";
    fetchSubmodules = true;
  };
  enableParallelBuilding = true;
  nativeBuildInputs = [ cmake ];
  buildInputs = [ llvmPackages_7.llvm llvmPackages_7.libclang
                  llvmPackages_7.libclang.out ];

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
