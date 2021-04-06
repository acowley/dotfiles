{ stdenv, lib, fetchFromGitHub, fetchgit, cmake, python3, doxygen, graphviz
, spirv-tools, spirv-headers }:
let local-spirv-headers = spirv-headers.overrideAttrs (old: rec {
      version = "2021-03-10";
      src = fetchFromGitHub {
        owner = "KhronosGroup";
        repo = "SPIRV-Headers";
        rev = "bcf55210f13a4fa3c3d0963b509ff1070e434c79";
        sha256 = "1xykpclxywr2dm4an19wrcjjzkxnl6gg49k7rbpxkl67yvabmr9m";
      };
    });
    local-spirv-tools = (spirv-tools.override {
      spirv-headers = local-spirv-headers;
    }).overrideAttrs (old: rec {
      version = "2021-03-17";
      src = fetchFromGitHub {
        owner = "KhronosGroup";
        repo = "SPIRV-Tools";
        rev = "4f498774db5250c05fbdd8f24912ab2938401c00";
        sha256 = "1g96nfpkfhsghwrhszkpx42hknym7s2a7bh9kcyjk0ja27kgvrgm";
      };
      postInstall = ''
        mkdir -p $out/include/spirv-tools
        cp *.inc $out/include/spirv-tools
      '';
    });
in stdenv.mkDerivation {
  pname = "tint";
  version = "2021-03-03-unstable";
  src = fetchgit {
    url = "https://dawn.googlesource.com/tint";
    rev = "2f04dc94ce25e91e40dc088d787e148bd499ba65";
    sha256 = "17l2wj95s7bvwmf28xgl5kllaaxlg39xvimnzfd6qmixpsbl7caz";
  };
  preConfigure = ''
    mkdir -p third_party/spirv-tools
    mkdir -p third_party/spirv-headers
    ln -sf ${local-spirv-tools.src}/source
    ln -sf ${local-spirv-tools}/include/spirv-tools third_party/spirv-tools/include
    ln -sf ${local-spirv-headers}/include/spirv/unified1 third_party/spirv-headers/include
  '';
  postPatch = ''
    sed '/add_subdirectory(third_party)/d' -i CMakeLists.txt
    sed "s|OUTPUT_DIRECTORY       = out/docs|OUTPUT_DIRECTORY       = $out/docs|" -i Doxyfile
  '';
  preBuild = ''
    mkdir -p $out/docs
  '';
  nativeBuildInputs = [ cmake python3 doxygen graphviz ];
  buildInputs = [ local-spirv-headers local-spirv-tools ];
  cmakeFlags = [
    "-DTINT_BUILD_TESTS=OFF"
  ];
  installPhase = ''
    mkdir -p $out/bin
    cp tint $out/bin
  '';
}
