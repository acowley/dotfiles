{ stdenv, fetchFromGitHub, python, scons, git, boost, cacert, xorg
, rocr, rocm-opencl-runtime, libGLU_combined }:
let sconsVersion = stdenv.lib.concatStringsSep "."
                    (stdenv.lib.tail (builtins.splitVersion scons.name));
in stdenv.mkDerivation rec {
  name = "rcp-${version}";
  version = "2018-04-19";
  src = fetchFromGitHub {
    owner = "GPUOpen-Tools";
    repo = "RCP";
    rev = "2b1967ca22ea928ec138bbf22b773636408fa5ad";
    sha256 = "0p26129d7iawzzgs3iqsfpc61630y37p34v99sc2pb5b52kn387z";
  };
  nativeBuildInputs = [ python scons git cacert ];
  buildInputs = [ boost rocr libGLU_combined xorg.libX11 ];
  preConfigure = ''
    export GIT_SSL_CAINFO=/etc/ssl/certs/ca-certificates.crt
    export CXL_boost_lib_dir=${boost}/lib
    python Scripts/UpdateCommon.py
    sed 's,#!/bin/bash,#!${stdenv.shell},' -i Build/Linux/build_rcp.sh
    sed '/[[:space:]]*sys.path.append("\/usr\/lib\/scons\/SCons\/Variables\/")/,/[[:space:]]*sys.path.append("\/usr\/lib\/python3\/site-packages\/SCons\/Variables\/")/ c\
        sys.path.append("${scons}/lib/python2.7/site-packages/scons-${sconsVersion}/SCons/Variables")
' -i Build/Linux/CXL_init.py
    sed 's/ -Werror//' -i Build/Linux/CXL_init.py
    sed 's/#include <algorithm>/#include <algorithm>\n#include <vector>/' -i Src/CLCommon/CLDeviceReplacer.cpp
    sed -e 's,BOOST_DIR = $(COMMON_LIB_EXT)/Boost/boost_1_59_0,BOOST_DIR = ${boost},' \
        -e 's,BOOST_LIB_DIR = /usr/lib/x86_64-linux-gnu,BOOST_LIB_DIR = ${boost}/lib,' \
        -i Build/Linux/Common.mk
    sed 's,/libboost_\([^.]*\)\.a,/libboost_\1.so,g' -i Src/sprofile/makefile
    chmod u+x Build/Linux/build_rcp.sh
  '';
  CPPFLAGS = "-Wno-error";
  buildPhase = ''
    cd Build/Linux
    ./build_rcp.sh skip-32bitbuild hsadir ${rocr}
  '';
  meta = {
    description = "The Radeon Compute Profiler";
    homepage = https://github.com/GPUOpen-Tools/RCP;
  };
}
