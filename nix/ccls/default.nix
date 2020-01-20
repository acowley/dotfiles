{ stdenv, fetchFromGitHub, cmake, llvmPackages }:
llvmPackages.stdenv.mkDerivation rec {
  name = "ccls-${version}";
  version = "20191228";
  src = fetchFromGitHub {
    owner = "MaskRay";
    repo = "ccls";
    rev = "64e3e8f2508d141094a640f9116411a15be9cae4";
    sha256 = "08srr9kqv3kss7ry7hkmjg821r6v0kybmf9ph01b9wkfqya3vzsw";
    fetchSubmodules = true;
  };
  enableParallelBuilding = true;
  nativeBuildInputs = [ cmake ];
  buildInputs = [ llvmPackages.llvm llvmPackages.libclang
                  llvmPackages.libclang.out ];

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
