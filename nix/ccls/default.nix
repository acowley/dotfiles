{ stdenv, fetchFromGitHub, cmake, llvmPackages_8 }:
llvmPackages_8.stdenv.mkDerivation rec {
  name = "ccls-${version}";
  version = "20190418";
  src = fetchFromGitHub {
    owner = "MaskRay";
    repo = "ccls";
    rev = "e496d4e5db47b602b14fe53bd556f11d0b025f52";
    sha256 = "18jh9jynszqz1k7xscv80r4zg1v0a1qpkxwgd45pc8xwlvshcbbv";
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
