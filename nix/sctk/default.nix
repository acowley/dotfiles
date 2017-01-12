{ stdenv, fetchurl, perl }:
stdenv.mkDerivation {
  name = "sctk-2.4.10";
  src = fetchurl {
    url = "http://www.openslr.org/resources/4/sctk-2.4.10-20151007-1312Z.tar.bz2";
    md5 = "dd01ad49a33486a4754655d06177f646";
  };
  buildInputs = [ perl ];
  buildPhase = ''
    make config
    sed 's/gcc/clang/' -i ./src/rfilter1/makefile
    make all
  '';
  meta = with stdenv.lib; {
    description = "Speech Recognition Scoring Toolkit";
    homepage = https://www.nist.gov/itl/iad/mig/tools;
    maintainers = [ maintainers.acowley ];
    platforms = platforms.unix;
  };
}
