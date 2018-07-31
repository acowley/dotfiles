with (import <nixpkgs> {});
let drv = callPackage ./default.nix {
  inherit cmake pkgconfig pcl eigen darwin;
  inherit (llvmPackages_6) stdenv;
  boost = boost166;
};
in drv.overrideAttrs (old: {
nativeBuildInputs = (old.nativeBuildInputs or []) ++ [ (ccls-project {inherit (old) buildInputs;} ) ];
cmakeFlags = (old.cmakeFlags or []) ++ [ "-DCMAKE_EXPORT_COMPILE_COMMANDS=YES" ];
})
