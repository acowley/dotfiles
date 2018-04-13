{ stdenv, fetchFromGitHub, cmake, pkgconfig, rocm-cmake, half, openssl, boost,
  rocm-opencl-runtime, rocr, hcc, hip, clang-ocl, miopengemm, useHip ? false }:
assert useHip -> hip != null;
stdenv.mkDerivation rec {
  name = "miopen";
  version = "1.3.0";
  src = fetchFromGitHub {
    owner = "ROCmSOftwarePlatform";
    repo = "MIOpen";
    rev = version;
    sha256 = "14ql39amk8a2i845bi0n6vriklkc4sabyv7z3kww4g8z5ypzrafm";
  };
  nativeBuildInputs = [ cmake pkgconfig rocm-cmake ];
  buildInputs = [ rocr half openssl boost miopengemm ]
    ++ (if useHip then [ hcc hip ] else [rocm-opencl-runtime clang-ocl]);

  cmakeFlags = [
    "-DCMAKE_PREFIX_PATH=${hcc};${hip};${clang-ocl}"
    "-DMIOPEN_AMDGCN_ASSEMBLER_PATH=${hcc}/bin"
    "-DCMAKE_INSTALL_INCLUDEDIR=include"
  ] ++ (if useHip
  then [ "-DCMAKE_CXX_COMPILER=${hcc}/bin/hcc"
         "-DMIOPEN_BACKEND=HIP"
         "-DENABLE_HIP_WORKAROUNDS=YES" ]
  else [ "-DMIOPEN_BACKEND=OpenCL"
         "-DOPENCL_INCLUDE_DIRS=${rocm-opencl-runtime}/include/opencl2.2"
         "-DOPENCL_LIB_DIRS=${rocm-opencl-runtime}/lib"
  ]);
  patchPhase = ''
    sed -e 's,cmake_minimum_required( VERSION 2.8.12 ),cmake_minimum_required( VERSION 3.10 ),' \
        -i CMakeLists.txt
  '';
}
