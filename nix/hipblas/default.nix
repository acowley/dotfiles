{ stdenv, fetchFromGitHub, cmake, pkgconfig, hcc, hip, libunwind
, rocm-cmake, rocr, rocminfo, rocblas
, doCheck ? false
, boost, gtest, liblapack_3_8, gfortran }:
stdenv.mkDerivation rec {
  name = "hipBLAS";
  version = "0.10.2.0";
  src = fetchFromGitHub {
    owner = "ROCmSoftwarePlatform";
    repo = "hipBLAS";
    rev = "v${version}";
    sha256 = "0yyndzcivi3ni85rqknjjvdw43kvj16jmdk1v68970w3sv6fzl9x";
  };
  nativeBuildInputs = [ cmake rocm-cmake pkgconfig ];
  buildInputs = [ libunwind hcc hip rocminfo rocr rocblas ]
    ++ stdenv.lib.optionals doCheck [ gfortran boost gtest liblapack_3_8 ];
  cmakeFlags = [
    "-DCMAKE_CXX_COMPILER=${hcc}/bin/hcc"
  ] ++ stdenv.lib.optionals doCheck [
   "-DBUILD_CLIENTS_SAMPLES=YES"
   "-DBUILD_CLIENTS_TESTS=YES"
  ];
}
