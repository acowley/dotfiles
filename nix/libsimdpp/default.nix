{ stdenv, lib, fetchFromGitHub, cmake }:
stdenv.mkDerivation rec {
  name = "libsimdpp-${version}";
  version = "2018-01-14";
  src = fetchFromGitHub {
    owner = "p12tic";
    repo = "libsimdpp";
    rev = "c27dfae319487596fb5c434bea64c8c2f57f447b";
    sha256 = "1h788jj4znqb55fhca9qlg0fipb6fzmv3j75ribhb2di8rw1zc66";
  };
  nativeBuildInputs = [ cmake ];
  meta = {
    description = "Portable header-only zero-overhead C++ low level SIMD library";
    homepage = https://github.com/p12tic/libsimdpp;
    license = lib.licenses.boost;
    platforms = lib.platforms.all;
  };
}
