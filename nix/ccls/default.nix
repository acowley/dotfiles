{ stdenv, fetchFromGitHub, fetchpatch, cmake, llvmPackages }:
llvmPackages.stdenv.mkDerivation rec {
  name = "ccls-${version}";
  version = "20200704";
  src = fetchFromGitHub {
    owner = "MaskRay";
    repo = "ccls";
    rev = "9477d56c7b7fe27987fd3b3a925c855518f41895";
    sha256 = "0sas92bc33l4la4jy9nnsp6bab38x4n5s86cfxy8z3a5m2hj4966";
    fetchSubmodules = true;
  };
  enableParallelBuilding = true;
  nativeBuildInputs = [ cmake ];
  buildInputs = [ llvmPackages.llvm llvmPackages.libclang
                  llvmPackages.libclang.out ];

  preConfigure =''
    sed 's/if(NOT LLVM_ENABLE_RTTI)/if(1)/' -i CMakeLists.txt
  '';

  # static symbols appear as emacs imenu leaves which prevents
  # navigating to functions in which they are defined. This patch does
  # not include static duration local variables in the relevant lsp
  # query.
  patches = [
    (fetchpatch {
      name = "no-static-symbols";
      url = "https://github.com/acowley/ccls/commit/22952acd425c57de743331939f3317258978231b.patch";
      sha256 = "1zj1fl71r98cxq0zmpvj5w487wihi1xk055fw9sbz7fai0kxxgz7";
    })
  ];

  meta = {
    description = "C/C++/ObjC language server";
    homepage = https://github.com/MaskRay/ccls;
    license = stdenv.lib.licenses.asl20;
    platforms = stdenv.lib.platforms.all;
  };
}
