{ stdenv, fetchFromGitHub, cmake, llvmPackages_8 }:
llvmPackages_8.stdenv.mkDerivation rec {
  name = "ccls-${version}";
  version = "20190716";
  src = fetchFromGitHub {
    owner = "MaskRay";
    repo = "ccls";
    rev = "eb752497e87fd062c2ddccb7e780c8812c4a2f19";
    sha256 = "190xz3740cbw0gafskz0bq9vw4xq6hqyzwd0vzi45fcsvy68973w";
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
