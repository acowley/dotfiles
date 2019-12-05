{ stdenv, fetchFromGitHub, lz4, pkgconfig }:
stdenv.mkDerivation {
  pname = "lz4json";
  version = "2019-06-24";
  src = fetchFromGitHub {
    owner = "andikleen";
    repo = "lz4json";
    rev = "ac5e269be0fd8469640b4a94db4f86f64d6562d1";
    sha256 = "193n6syic94vrp402shp3a70z3alagy51n762k171ank90b14bj6";
  };
  nativeBuildInputs = [ pkgconfig ];
  buildInputs = [ lz4 ];
  installPhase = ''
    mkdir -p $out/bin
    mv lz4jsoncat $out/bin
  '';
}
