{ stdenv, lib, fetchFromGitHub, meson, ninja, pkgconfig, assimp, glm
, libxcb, xcbutilwm, vulkan-headers, vulkan-loader }:

stdenv.mkDerivation {
  name = "vkmark-2018.05.30";
  src = fetchFromGitHub {
    owner = "vkmark";
    repo = "vkmark";
    rev = "1ebd49364f03372a710f010c01dedd0d79456413";
    sha256 = "07nx8s25wjd2rsihbvmz9vnqsh87ji5i6qznad75y50a8k5fik5h";
  };
  nativeBuildInputs = [ meson ninja pkgconfig ];
  buildInputs = [ assimp glm libxcb xcbutilwm vulkan-headers vulkan-loader ];
  configurePhase = "meson build --prefix $out";
  buildPhase = "ninja -C build";
  installPhase = "ninja -C build install";

  meta = {
    description = "Vulkan benchmark modeled on glmark2";
    homepage = https://github.com/vkmark/vkmark;
    license = lib.licenses.lgpl21;
    platforms = lib.platforms.all;
  };
}
