{ stdenv, cmake }:
stdenv.mkDerivation rec {
  pname = "alglib";
  version = "3.16.0";
  src = builtins.fetchTarball {
    url = "https://www.alglib.net/translator/re/alglib-3.16.0.cpp.gpl.tgz";
    sha256 = "1dl7nj84swcwj2293am1jl0b2b1s5m7jap6sk0xjvspglrbzmnjy";
  };
  nativeBuildInputs = [ cmake ];
  cmakeFile = ./CMakeLists.txt;
  preConfigure = ''
    cp ${cmakeFile} CMakeLists.txt
    substituteInPlace CMakeLists.txt --subst-var version
  '';
}
