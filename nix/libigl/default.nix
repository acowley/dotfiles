{ stdenv, fetchFromGitHub }:
stdenv.mkDerivation rec {
  version = "2017-03-17";
  name = "libigl-${version}";
  src = fetchFromGitHub {
    owner = "libigl";
    repo = "libigl";
    rev = "7d8b6be28605f5264387ab0a822fb142e3004b9b";
    sha256 = "1may4adsvaa9sgk0fa9q7xmnwrxqhbqvmrhm4qwwbljlf6z2rvld";
  };
  phases = [ "unpackPhase" "installPhase" ];
  installPhase = ''
    mkdir -p $out
    cp -R include $out
  '';
}
