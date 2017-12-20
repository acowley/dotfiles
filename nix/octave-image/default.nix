{ stdenv, fetchurl, hdf5, openblas, darwin, octave }:
stdenv.mkDerivation rec {
  name = "octave-image-${version}";
  version = "2.6.1";
  src = fetchurl {
  #https://downloads.sourceforge.net/project/octave/Octave%20Forge%20Packages/Individual%20Package%20Releases/image-2.6.1.tar.gz?r=https%3A%2F%2Fsourceforge.net%2Fprojects%2Foctave%2Ffiles%2FOctave%2520Forge%2520Packages%2FIndividual%2520Package%2520Releases%2Fimage-2.6.1.tar.gz%2Fdownload%3Fuse_mirror%3Dsuperb-dca2%26download%3D&ts=1494531317&use_mirror=iweb
    url = "mirror://sourceforge/octave/image-${version}.tar.gz";
    sha256 = "13bpi4g6vn44k06iilbc0bakxizb8z7wszwp9aj4x2y8jh87cxm5";
  };
  buildInputs = [ octave hdf5 openblas ]
    ++ stdenv.lib.optionals stdenv.isDarwin
         (with darwin.apple_sdk.frameworks; [ OpenGL Carbon ]);
  builder = builtins.toFile "builder.sh" ''
    source $stdenv/setup
    cp $src image-${version}.tar.gz
    mkdir -p $out/etc
    octave --no-history --norc --eval "pkg prefix $out $out; pkg local_list $out/etc/.octave_packages; pkg install ./image-${version}.tar.gz"
  '';
  meta = with stdenv.lib; {
    homepage = https://octave.sourceforge.io/image/index.html;
    description = "Octave functions for processing images";
    platforms = octave.meta.platforms;
  };
}
