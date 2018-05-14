{ stdenv, fetchFromGitHub, cmake, rocr, roct }:
stdenv.mkDerivation {
  name = "rocm-bandwidth";
  version = "2018-04-19";
  src = fetchFromGitHub {
    owner = "RadeonOpenCompute";
    repo = "rocm_bandwidth_test";
    rev = "255785b9ab95caef1a66287334eeaa9022d1c174";
    sha256 = "18ddq8chwim1yzqklcqz0vnr6mxgxpibvzmaym0pxrv1v3hz0ylk";
  };
  nativeBuildInputs = [ cmake ];
  buildInputs = [ rocr roct ];
  cmakeFlags = [
    "-DROCR_INC_DIR=${rocr}/include"
    "-DROCR_LIB_DIR=${rocr}/lib"
  ];
}
