{ stdenv, fetchFromGitHub, meson, ninja, pkgconfig, assimp, glm
, libxcb, xcbutilwm, vulkan-loader }:

stdenv.mkDerivation {
  name = "vkmark-2018.01.10";
  src = fetchFromGitHub {
    owner = "vkmark";
    repo = "vkmark";
    rev = "68b6f230984c13a7ed1676bc9d7e72dfd9445cfa";
    sha256 = "07nx8s25wjd2rsihbvmz9vnqsh87ji5i6qznad75y50a8k5fik5h";
  };
  nativeBuildInputs = [ meson ninja pkgconfig ];
  buildInputs = [ assimp glm libxcb xcbutilwm vulkan-loader ];
  configurePhase = "meson build --prefix $out";
  buildPhase = "ninja -C build";
  installPhase = "ninja -C build install";

  meta = {
    description = "Vulkan benchmark modeled on glmark2";
    homepage = https://github.com/vkmark/vkmark;
    license = stdenv.lib.licenses.lgpl21;
    platforms = stdenv.lib.platforms.all;
  };
}
