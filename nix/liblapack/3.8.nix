{
  stdenv,
  fetchurl,
  gfortran,
  cmake,
  python2,
  atlas ? null,
  buildCBLAS ? true,
  shared ? false,
  doCheck ? true
}:
# The tests do not work with the shared library build
assert shared -> !doCheck;
let
  atlasMaybeShared = if atlas != null then atlas.override { inherit shared; }
                     else null;
  usedLibExtension = if shared then ".so" else ".a";
  inherit (stdenv.lib) optional optionals concatStringsSep;
  inherit (builtins) hasAttr attrNames;
  version = "3.8.0";
in

stdenv.mkDerivation rec {
  name = "liblapack-${version}";
  src = fetchurl {
    url = "http://www.netlib.org/lapack/lapack-${version}.tar.gz";
    sha256 = "1xmwi2mqmipvg950gb0rhgprcps8gy8sjm8ic9rgy2qjlv22rcny";
  };

  propagatedBuildInputs = [ atlasMaybeShared ];
  buildInputs = [ gfortran cmake ];
  nativeBuildInputs = [ python2 ];

  cmakeFlags = [
    "-DUSE_OPTIMIZED_BLAS=ON"
    "-DCMAKE_Fortran_FLAGS=-fPIC"
  ]
  ++ (optionals (atlas != null) [
    "-DBLAS_ATLAS_f77blas_LIBRARY=${atlasMaybeShared}/lib/libf77blas${usedLibExtension}"
    "-DBLAS_ATLAS_atlas_LIBRARY=${atlasMaybeShared}/lib/libatlas${usedLibExtension}"
  ])
  ++ (optional shared "-DBUILD_SHARED_LIBS=ON")
  # If we're on darwin, CMake will automatically detect impure paths. This switch
  # prevents that.
  ++ (optional stdenv.isDarwin "-DCMAKE_OSX_SYSROOT:PATH=''")
  ++ (optional buildCBLAS "-DCBLAS=ON")
  ++ (optional doCheck "-DBUILD_TESTING=ON")
  ;

  # The format-related hardening flags are not supported by the
  # Fortran compiler.
  hardeningDisable = ["format"];

  # doCheck = ! shared;

  checkPhase = "
    sed -i 's,^#!.*,#!${python2.interpreter},' lapack_testing.py
    ctest
  ";

  enableParallelBuilding = true;

  passthru = {
    blas = atlas;
  };

  meta = with stdenv.lib; {
    inherit version;
    description = "Linear Algebra PACKage";
    homepage = http://www.netlib.org/lapack/;
    license = licenses.bsd3;
    platforms = platforms.all;
  };
}
