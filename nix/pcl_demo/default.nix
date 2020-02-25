{ stdenv, cmake, pkgconfig, pcl, eigen, boost, darwin }:
let srcFiles = [ "CMakeLists.txt" "pcl_visualizer_demo.cpp" ];
in stdenv.mkDerivation {
  name = "pcl_demo";
  version = "0.0";
  # src = builtins.path {
  #   name = "pcl_demo-src";
  #   path = ./.;
  #   filter = path: type: builtins.elem path srcFiles;
  # };
  # src = stdenv.lib.cleanSourceWith {
  #   name = "pcl_demo-src";
  #   filter = path: type: builtins.elem path srcFiles;
  #   src = ./.;
  # };
  src = builtins.filterSource (path: type: builtins.elem (baseNameOf (toString path)) srcFiles) ./.;
  nativeBuildInputs = [ cmake pkgconfig ];
  buildInputs = [ pcl eigen boost ] ++
    stdenv.lib.optional stdenv.isDarwin darwin.apple_sdk.frameworks.Cocoa;
  meta = {
    description = "Demo of PCL visualization features";
    maintainers = with stdenv.lib.maintainers; [acowley];
    platforms = stdenv.lib.platforms.all;
  };
}
