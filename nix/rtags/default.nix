{ stdenv, lib, fetchgit, cmake, llvmPackages, openssl, writeScript, apple_sdk, bash, emacs, pkgconfig }:

stdenv.mkDerivation rec {
  name = "rtags-${version}";
  version = "2.15p1";

  nativeBuildInputs = [ cmake pkgconfig ];
  buildInputs = [ llvmPackages.llvm openssl emacs ]
    ++ lib.optionals stdenv.cc.isGNU [ llvmPackages.clang-unwrapped ]
    ++ lib.optionals stdenv.isDarwin [ apple_sdk.libs.xpc apple_sdk.frameworks.CoreServices ];


  src = fetchgit {
    # rev = "refs/tags/v${version}";
    rev = "9e53e4621354466abbea194df8996ed0ffcf4aab";
    fetchSubmodules = true;
    url = "https://github.com/andersbakken/rtags.git";
    #sha256 = "12nnyav2q1ddkz9wm0aclhn7r74xj4ibrb0x05k7mcf694bg79c0";
    sha256 = "03s0h04cxsz1xi9a8wcmj2jvxhff740d7dd2lx2n28b0xfpcwhy0";
    # unicode file names lead to different checksums on HFS+ vs. other
    # filesystems because of unicode normalisation
    postFetch = ''
      rm $out/src/rct/tests/testfile_*.txt
    '';
  };

  preConfigure = ''
    export LIBCLANG_CXXFLAGS="-isystem ${llvmPackages.clang.cc}/include $(llvm-config --cxxflags) -fexceptions" \
           LIBCLANG_LIBDIR="${llvmPackages.clang.cc}/lib"
  '';

  enableParallelBuilding = true;

  meta = {
    description = "C/C++ client-server indexer based on clang";
    homepage = https://github.com/andersbakken/rtags;
    license = stdenv.lib.licenses.gpl3;
    platforms = stdenv.lib.platforms.allBut [ "i686-linux" ];
  };
}
