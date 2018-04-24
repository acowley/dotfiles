{ stdenv, fetchFromGitHub, cmake, doxygen
, python, future, six, python-protobuf, numpy, pydot
, rocr, rocminfo, hcc, hip, rocrand, rocblas, hipblas, rocm-cmake
, miopen-hip, miopengemm, rocm-thrust
, protobuf, eigen3, lmdb, leveldb, snappy, atlas, opencv3, rocksdb
, doCheck ? false
, glog, google-gflags, gbenchmark }:
let
  # pyenv = python.withPackages (ps:
  #              with ps; [pyyaml pip wheel setuptools virtualenv]);

  # Third party modules that caffe2 holds as git submodules.
  # Download them and create symlinks from caffe2/third_party.
  installExtraSrc = extra: ''
    rmdir "third_party/${extra.dst}"
    ln -s "${extra.src}" "third_party/${extra.dst}"
  '';
  pybind11 = {
    src = fetchFromGitHub {
      owner  = "pybind";
      repo   = "pybind11";
      rev    = "86e2ad4f77442c3350f9a2476650da6bee253c52";
      sha256 = "05gi58dirvc8fgm0avpydvidzsbh2zrzgfaq671ym09f6dz0bcgz";
    };
    dst = "pybind11";
  };
  gtest_new = stdenv.mkDerivation rec {
    name = "gtest-${version}";
    version = "2017-10-18";

    src = fetchFromGitHub {
      owner = "google";
      repo = "googletest";
      rev = "69e48e92de43960a316a826293510b7b3deb9eca";
      sha256 = "1mjad92a3kv29x9wgbix6raai71fkw3mimli6rlwkhzs7cp0vllh";
    };

    buildInputs = [ cmake ];

    configurePhase = ''
      mkdir build
      cd build
      cmake ../ -DCMAKE_INSTALL_PREFIX=$out
    '';

    installPhase = ''
      mkdir -p $out/lib
      cp -v googlemock/gtest/libgtest.a googlemock/gtest/libgtest_main.a googlemock/libgmock.a googlemock/libgmock_main.a $out/lib
      ln -s $out/lib/libgmock.a $out/lib/libgoogletest.a
      mkdir -p $out/include
      cp -v -r ../googlemock/include/gmock $out/include
      cp -v -r ../googletest/include/gtest $out/include
      mkdir -p $out/src
      cp -v -r ../googlemock/src/* ../googletest/src/* $out/src
    '';

    meta = with stdenv.lib; {
      description = "Google's framework for writing C++ tests";
      homepage = https://github.com/google/googletest;
      license = licenses.bsd3;
      platforms = platforms.all;
      maintainers = with maintainers; [ zoomulator ivan-tkatchev ];
    };
  };
in
stdenv.mkDerivation rec {
  name = "rocm-caffe2";
  version = "2018-04-16";
  src = fetchFromGitHub {
    owner = "ROCmSoftwarePlatform";
    repo = "rocm-caffe2";
    # rev = "62652543077efe27928dd6c32d797327e9fc9c21";
    # sha256 = "0hz4xbran0arxp8q71gra95g5iwziwligi5jz2fhyr5h6hkqsna2";
    rev = "e464cde8840c49c95bb093ebd79368705738e7dd";
    sha256 = "0fxjpzv67y2h3p7kxyscv4gagm8nxf88pr3cddk0bncd8a2qz304";
  };

  outputs = [ "bin" "out" ];
  propagatedBuildOutputs = [ ]; # otherwise propagates out -> bin cycle
  propagatedBuildInputs = [ numpy future six python-protobuf pydot ];

  nativeBuildInputs = [ cmake rocm-cmake doxygen ];

  buildInputs = [ glog google-gflags protobuf eigen3
    lmdb leveldb snappy atlas opencv3 hip hipblas rocblas rocrand
    miopen-hip miopengemm rocksdb
  ] ++ stdenv.lib.optionals doCheck [ gtest_new gbenchmark ];

  patchPhase = ''
    sed -e '/find_package(Eigen3)/d' \
        -e 's/if(EIGEN3_FOUND)/if(1)/' \
        -e 's,^[[:space:]]*add_subdirectory(''${PROJECT_SOURCE_DIR}/third_party/googletest),,' \
        -e 's,''${PROJECT_SOURCE_DIR}/third_party/googletest/googletest/include,${gtest_new}/include,' \
        -e 's,^[[:space:]]*add_subdirectory(''${PROJECT_SOURCE_DIR}/third_party/benchmark),,' \
        -e 's,''${PROJECT_SOURCE_DIR}/third_party/benchmark/include,${gbenchmark}/include,' \
        -i cmake/Dependencies.cmake
  '';

  preConfigure = ''
    export GIT_SSL_CAINFO=/etc/ssl/certs/ca-certificates.crt
    export HCC_HOME=${hcc}
    export HIP_PATH=${hip}
    export THRUST_ROOT=${rocm-thrust}/include
    ${installExtraSrc pybind11}
  '';

  cmakeFlags = [
    "-DUSE_GLOO=OFF"
    "-DUSE_MPI=OFF"
    "-DUSE_NNPACK=OFF"
    "-DEIGEN3_INCLUDE_DIR=${eigen3}/include/eigen3"
    "-Dpybind11_INCLUDE_DIR=${pybind11.src}/include"
    "-DBUILD_TEST=${if doCheck then "YES" else "NO"}"
    "-DPYTHON_INCLUDE_DIR=${python}/include/python${stdenv.lib.concatStringsSep "." (stdenv.lib.take 2 (builtins.splitVersion python.version))}"
    "-DPYTHON_EXECUTABLE=${python}/bin/python3"
    "-DPYTHON_LIBRARY=${python}/lib/libpython3.so"
  ];
  enableParallelBuilding = true;

  postInstall = ''
    moveToOutput "bin" "$bin"
    mkdir -p $out/lib/${python.libPrefix}
    ln -s $out/ $out/${python.sitePackages}
    echo "Patching caffe2_pybind11_state_hip.so"
    patchelf --replace-needed ../lib/libcaffe2_hip.so $out/lib/libcaffe2_hip.so $out/caffe2/python/caffe2_pybind11_state_hip.so
    for f in $(find $bin/bin -type f); do
      echo "Patching $f"
      patchelf --replace-needed ../../lib/libcaffe2_hip.so $out/lib/libcaffe2_hip.so "$f"
    done
  '';

}
