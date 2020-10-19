{ stdenv, cmake, fetchFromGitLab
, qtbase, qtcharts, qttools# , qttranslations
, libdrm, kdeFrameworks, botan2, hwdata, glxinfo, wrapQtAppsHook }:
stdenv.mkDerivation {
  pname = "corectrl";
  version = "20200910";
  src = fetchFromGitLab {
    owner = "corectrl";
    repo = "corectrl";
    rev = "bad0a0bb3a9f90d33056894fabf119bfde9ed408";
    sha256 = "1g8vxbdiy58035q0jh440wajkqc17am7pja05hh0m5yl2938rhww";
  };
  prePatch = ''
    sed 's|/usr\(/share/hwdata/pci.ids\)|${hwdata}\1|' -i src/app/appfactory.cpp
  '';
  nativeBuildInputs = [ cmake wrapQtAppsHook ];
  qtWrapperArgs = [ ''--prefix PATH : ${glxinfo}/bin'' ];
  buildInputs = [ qtbase qtcharts qttools # qttranslations
                  libdrm botan2
                  kdeFrameworks.extra-cmake-modules kdeFrameworks.kauth kdeFrameworks.karchive ];
}
