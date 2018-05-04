{ stdenv, fetchFromGitHub, cmake, pkgconfig, libunwind, python
, rocr, rocminfo, hcc, git, hip, rocm-cmake
, doCheck ? false
# Tensile slows the build a lot, but can produce a faster rocBLAS
, useTensile ? true
, gfortran, lapack_3_8, boost, gtest }:
let pyenv = python.withPackages (ps:
               with ps; [pyyaml pip wheel setuptools virtualenv]); in
stdenv.mkDerivation rec {
  name = "rocBLAS";
  version = "12.3.1";
  src = fetchFromGitHub {
    owner = "ROCmSoftwarePlatform";
    repo = "rocBLAS";
    rev = "v${version}";
    sha256 = "1862plbygwsd1zylc7pcv5lpf3xvrvbafqkbv2gmdcxdm6rinvml";
  };
  nativeBuildInputs = [ cmake rocm-cmake pkgconfig git ];
  buildInputs = [ libunwind pyenv hcc hip rocminfo rocr ]
    ++ stdenv.lib.optionals doCheck [ gfortran boost gtest lapack_3_8 ];
  preConfigure = ''
    export GIT_SSL_CAINFO=/etc/ssl/certs/ca-certificates.crt
  '';
  cmakeFlags = [
    "-DCMAKE_CXX_COMPILER=${hcc}/bin/hcc"
    "-DCMAKE_INSTALL_INCLUDEDIR=include"
    "-DBUILD_WITH_TENSILE=${if useTensile then "ON" else "OFF"}"
  ] ++ stdenv.lib.optionals doCheck [
    "-DBUILD_CLIENTS_SAMPLES=YES"
    "-DBUILD_CLIENTS_TESTS=YES"
    "-DBUILD_CLIENTS_BENCHMARKS=YES"
  ];
}
