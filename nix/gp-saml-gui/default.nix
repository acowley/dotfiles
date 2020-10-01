{ lib, fetchFromGitHub, buildPythonApplication
, gobject-introspection, webkitgtk, glib-networking
, requests, pygobject3, wrapGAppsHook }:
buildPythonApplication {
  pname = "gp-saml-gui";
  version = "2020-08-27";
  src = fetchFromGitHub {
    owner = "dlenski";
    repo = "gp-saml-gui";
    rev = "bb4a80e2ff2ac4dfc524549ea036d44f0e129df6";
    sha256 = "12d9zj0jm3s22986v2aav5il4qc9kb8ijr54gvms173zcshz6bqg";
  };
  nativeBuildInputs = [ wrapGAppsHook ];
  propagatedBuildInputs = [ gobject-introspection webkitgtk requests pygobject3 glib-networking ];
  phases = [ "patchPhase" "installPhase" "fixupPhase" ];
  permitUserSite = true;
  installPhase = ''
    mkdir -p $out/bin
    cp $src/gp-saml-gui.py $out/bin
  '';
}
