{ stdenv, lib, fetchFromGitHub, fetchgit, cmake, python3, doxygen, graphviz
, spirv-tools, spirv-headers }:
let local-spirv-headers = spirv-headers.overrideAttrs (old: rec {
      version = "1.5.4.raytracing.fixed";
      src = fetchFromGitHub {
        owner = "KhronosGroup";
        repo = "SPIRV-Headers";
        rev = version;
        sha256 = "sha256:12gp2mqcar6jj57jw9isfr62yn72kmvdcl0zga4gvrlyfhnf582q";
      };
    });
    local-spirv-tools = (spirv-tools.override {
      spirv-headers = local-spirv-headers;
    }).overrideAttrs (old: rec {
      version = "2020.6";
      src = fetchFromGitHub {
        owner = "KhronosGroup";
        repo = "SPIRV-Tools";
        rev = "v${version}";
        sha256 = "sha256:0v26ws6qx23jn4dcpsq6rqmdxgyxpl5pcvfm90wb3nz6iqbqx294";
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
    rev = "04d93c88a06b32ee2cb0f2fe4a95678659d93f3e";
    sha256 = "sha256:0cl6qqk29kbkx9sk61545w7axmw6j88j9givzw0kyssrxjkws5na";
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
