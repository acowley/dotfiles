{ stdenv }:
stdenv.mkDerivation rec {
  name = "ogre-${version}";
  version = "1.7.4";
  phases = [ "installPhase" ];
  installPhase = ''
    mkdir -p $out/lib
    mkdir -p $out/lib/pkgconfig
    mkdir -p $out/share
    mkdir -p $out/include
    ln -s /usr/local/lib/libOgre* $out/lib/
    ln -s /usr/local/lib/pkgconfig/OGRE* $out/lib/pkgconfig/
    ln -s /usr/local/include/OGRE $out/include/
    ln -s /usr/local/include/OGRE/* $out/include/
    ln -s /usr/local/include/OGRE/Paging/* $out/include/
    ln -s /usr/local/Cellar/ogre/${version}/CMake/* $out/share/
  '';
}
