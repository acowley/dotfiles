{ stdenv, fetchurl }:
stdenv.mkDerivation {
  name = "sph2pipe-2.5";
  src = fetchurl {
    url = "https://www.ldc.upenn.edu/sites/www.ldc.upenn.edu/files/ctools/sph2pipe_v2.5.tar.gz";
    sha256 = "1qyksk8xjs0hab0dy98gwz3rwybnky29ydlabhf302pdjkf3dqjv";
  };
  buildPhase = ''
    mkdir bin
    "$CC" -o bin/sph2pipe *.c -lm
  '';
  installPhase = ''
    mkdir -p $out/bin
    mv bin/sph2pipe $out/bin
  '';
  meta = with stdenv.lib; {
    description = "NIST SPHERE speech file format conversion utility";
    homepage = https://www.ldc.upenn.edu/language-resources/tools/sphere-conversion-tools;
    maintainers = [ maintainers.acowley ];
    platforms = platforms.unix;
  };
}
