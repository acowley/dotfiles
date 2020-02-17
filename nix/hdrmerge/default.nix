{ mkDerivation, fetchFromGitHub, cmake
, alglib, boost, exiv2, gettext, expat, libiconv, libraw, zlib
, qtbase }:
mkDerivation {
  pname = "hdrmerge";
  version = "2019-10-05";
  src = fetchFromGitHub {
    owner = "jcelaya";
    repo = "hdrmerge";
    rev = "69e3c5f85eb80a6f1229f6b789cd390c9497ada5";
    sha256 = "13gr7504imd6inpb6x3bf8xlqzn3amj7vfr0ayg2vabcx3q06f0j";
  };
  nativeBuildInputs = [ cmake ];
  buildInputs = [ alglib boost exiv2 gettext expat libiconv libraw zlib qtbase ];
  CXXFLAGS = "-Wno-deprecated-declarations";
}
