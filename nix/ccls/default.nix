{ stdenv, fetchFromGitHub, cmake, llvmPackages_8 }:
llvmPackages_8.stdenv.mkDerivation rec {
  name = "ccls-${version}";
  version = "20190412";
  src = fetchFromGitHub {
    owner = "MaskRay";
    repo = "ccls";
    rev = "6710dbc2e9dc4b3bff1438e5be9549d147507b5d";
    sha256 = "0i6amhgl7jjdmkisz03b3nwly5mgglhzj56kxvfyl6pnk9dzplwv";
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
