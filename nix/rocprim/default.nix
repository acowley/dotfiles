{ stdenv, fetchFromGitHub, cmake, git
, rocr, hcc, hip, rocm-cmake
, doCheck ? false
, doBench ? false
, gtest, gbenchmark }:
stdenv.mkDerivation rec {
  name = "rocPRIM-${version}";
  version = "0.3.0";
  src = fetchFromGitHub {
    owner = "ROCmSoftwarePlatform";
    repo = "rocPRIM";
    rev = "v${version}";
    sha256 = "13hzg27nf2r294x1pl42hjsa9pabzlcrpnx3a4wwi37dk2427n0a";
  };
  nativeBuildInputs = [ cmake rocm-cmake git ];
  buildInputs = [ rocr hcc hip ]
    ++ stdenv.lib.optionals doCheck [gtest gbenchmark];
  cmakeFlags = [
    "-DHIP_PLATFORM=hcc"
    "-DHSA_HEADER=${rocr}/include"
    "-DHSA_LIBRARY=${rocr}/lib/libhsa-runtime64.so"

    "-DBUILD_TEST=${if doCheck then "ON" else "OFF"}"
    "-DBUILD_BENCHMARK=${if doBench then "ON" else "OFF"}"
  ];
  patchPhase = ''
    sed '/project(rocprim VERSION 0.3.0.0 LANGUAGES CXX)/d' -i CMakeLists.txt
    sed 's,\(include(cmake/SetToolchain.cmake)\),project(rocprim VERSION 0.3.0.0 LANGUAGES CXX)\n\1,' -i CMakeLists.txt
    sed -e '/^[[:space:]]*find_package(hcc REQUIRED CONFIG PATHS .*$/ d' \
        -e '/^[[:space:]]*find_package(hip REQUIRED CONFIG PATHS .*$/ d' \
        -i cmake/Dependencies.cmake
  '';
}
