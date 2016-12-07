{ stdenv }:
stdenv.mkDerivation rec {
  name = "gazebo-${version}";
  version = "7";
  phases = [ "installPhase" ];
  installPhase = ''
    mkdir -p $out/lib
    mkdir -p $out/lib/pkgconfig
    mkdir -p $out/share
    mkdir -p $out/include
    ln -s /usr/local/lib/libgazebo* $out/lib/
    ln -s /usr/local/lib/gazebo-7/plugins/* $out/lib/
    ln -s /usr/local/lib/pkgconfig/gazebo* $out/lib/pkgconfig/
    # ln -s /usr/local/lib/pkgconfig/protobuf.pc $out/lib/pkgconfig/protobuf.pc
    ln -s /usr/local/lib/pkgconfig/OGRE* $out/lib/pkgconfig/
    ln -s /usr/local/include/gazebo-7/gazebo $out/include/
    ln -s /usr/local/Cellar/gazebo${version}/*/lib/cmake/gazebo $out/share/
  '';
}
