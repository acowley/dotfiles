{ stdenv, lib, fetchFromGitHub, cmake
, glfw, buildSamples ? false
, mesa_drivers, vulkan-headers, vulkan-loader }:
stdenv.mkDerivation {
  name = "vez";
  version = "2018-10-04";
  src = fetchFromGitHub {
    owner = "GPUOpen-LibrariesAndSDKs";
    repo = "V-EZ";
    rev = "29d2229e44ee692f74a33b698c80273ebcc67c3e";
    sha256 = "11pbb7xm8mdzn7w2ncywgqfvqf1dihnkqpiw1ygsqwym0x9p22m8";
    fetchSubmodules = true;
  };
  nativeBuildInputs = [ cmake ];
  # buildInputs = [ mesa_drivers.dev ];
  buildInputs = lib.optional buildSamples glfw;
  propagatedBuildInputs = [ vulkan-headers vulkan-loader ];
  patchPhase = ''
    sed 's|\(set(OUTPUT_DIRECTORY \).*|\1"''${CMAKE_BINARY_DIR}/lib")|' -i CMakeLists.txt
  '' + lib.optionalString buildSamples ''
    sed -e '/\/Libs\/glfw\/include/d' \
        -e '/\/Libs\/glfw\/lib/d' \
        -i Samples/CMakeLists.txt
    for f in $(find Samples -name '*.cpp'); do
      sed "s|../../Samples/Data|$out/Data|" -i "$f"
    done
  '';
  postBuild = ''
    mkdir include
    cp $src/Source/*.h include
    mkdir include/Core
    cp $src/Source/Core/*.h include/Core
    mkdir include/Utility
    cp $src/Source/Utility/*.h include/Utility
  '';
  cmakeFlags = [
    "-DVEZ_COMPILE_SAMPLES=${if buildSamples then "ON" else "OFF"}"
  ];
  installPhase = ''
    mkdir $out
    cp -r lib $out
    cp -r include $out
  '' + lib.optionalString buildSamples ''
    cp -r ../Samples/Data $out
    cp ../Libs/Assimp/lib/${if stdenv.isLinux then "linux64" else "osx64"}/* $out/lib
  '';
}
