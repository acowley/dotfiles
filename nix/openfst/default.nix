{ stdenv, lib, fetchurl }:
stdenv.mkDerivation {
  name = "openfst-1.4.1";
  src = fetchurl {
    url = "http://www.openfst.org/twiki/pub/FST/FstDownload/openfst-1.4.1.tar.gz";
    sha256 = "0xw2h71a6lqcs7wvsafhzndqcx0v40ja0hvm9vniynj2simvywg6";
  };
  meta = with lib; {
    description = "Library for working with weighted finite-state transducers";
    homepage = http://www.openfst.org/twiki/bin/view/FST/WebHome;
    license = licenses.apsl20;
    maintainers = [ maintainers.acowley ];
    platforms = platforms.unix;
  };
}
