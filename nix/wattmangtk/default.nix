{ fetchFromGitHub, buildPythonApplication, matplotlib, pygobject3, setuptools
, cairo, gtk3, gobjectIntrospection, wrapGAppsHook, pciutils }:
buildPythonApplication {
  name = "WattmanGTK";
  version = "2019-02-15";
  buildInputs = [ cairo gtk3 setuptools ];
  propagatedBuildInputs = [ matplotlib pygobject3 pciutils ];
  src = fetchFromGitHub {
    owner = "BoukeHaarsma23";
    repo = "WattmanGTK";
    rev = "af8f7ee97fd68fe822d8c6f2e9c00fff00b70320";
    sha256 = "1jk1j8jvic5f81b0r9pg82rir1smwlcqqbqrrkf998zp77rgkb6p";
  };
  doCheck = false;
}
