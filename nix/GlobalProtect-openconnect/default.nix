{ mkDerivation, lib, fetchFromGitHub, openconnect, qmake, qtbase, qtwebsockets, qtwebengine }:
let version = "1.2.5";
in mkDerivation {
  pname = "GlobalProtect-openconnect";
  inherit version; 
  src = fetchFromGitHub {
    owner = "yuezk";
    repo = "GlobalProtect-openconnect";
    rev = "v${version}";
    sha256 = "0an6qrx5s7krph8i88qh7jzkl54w9gl0hkvd0njh8prz531cvnc9";
    fetchSubmodules = true;
  };
  nativeBuildInputs = [ qmake ];
  buildInputs = [ openconnect qtbase qtwebsockets qtwebengine ];
  prePatch = ''
    sed -e "s|target.path = /usr/bin|target.path = $out/bin|" \
        -e 's|INSTALLS += dbus_config dbus_service systemd_service||' \
        -i GPService/GPService.pro
    sed -e "s|target.path = /usr/bin|target.path = $out/bin|" \
        -e 's|INSTALLS += desktop_entry desktop_icon||' \
        -i GPClient/GPClient.pro
    sed 's|"/usr/local/bin/openconnect",|"${openconnect}/bin/openconnect",|' -i GPService/gpservice.h
  '';
}
