{ stdenv, fetchFromGitHub, cmake, SDL2, pkgconfig, doxygen, mesa, darwin}:
stdenv.mkDerivation rec {
  name = "SDL2_gpu-${version}";
  version = "2018-01-09";
  src = fetchFromGitHub {
    owner = "grimfang4";
    repo = "sdl-gpu";
    rev = "143f767adf7d472f81ce890d4692ed29369aa8f3";
    sha256 = "1ljvyb2gmkiqyrwyrajgixi7kh4ys7rka25q2sszw1m3alzlpkyi";
  };
  nativeBuildInputs = [ cmake pkgconfig doxygen ];
  buildInputs = [ SDL2 ] ++
    [(if stdenv.isDarwin
      then darwin.apple_sdk.frameworks.OpenGL
      else mesa)];
  cmakeFlags = stdenv.lib.optionals stdenv.isDarwin ["-DSDL_gpu_BUILD_FRAMEWORK=OFF"];
  meta = {
    description = "High-performance, modern 2D graphics with SDL";
    homepage = https://github.com/grimfang4/sdl-gpu;
    license = stdenv.lib.licenses.mit;
    platforms = stdenv.lib.platforms.all;
  };
}
