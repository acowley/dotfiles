{ stdenv, cmake, pkgconfig, pcl, eigen, boost, darwin }:
stdenv.mkDerivation {
  name = "pcl_demo";
  version = "0.0";
  src = ./.;
  nativeBuildInputs = [ cmake pkgconfig ];
  buildInputs = [ pcl eigen boost ] ++
    stdenv.lib.optional stdenv.isDarwin darwin.apple_sdk.frameworks.Cocoa;
  meta = {
    description = "Demo of PCL visualization features";
    maintainers = with stdenv.lib.maintainers; [acowley];
    platforms = stdenv.lib.platforms.all;
  };
}
