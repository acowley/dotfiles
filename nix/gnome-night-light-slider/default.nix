{ stdenv, fetchFromGitHub, glib }:

stdenv.mkDerivation rec {
  name = "gnome-shell-extension-night-light-slider-${version}";
  version = "8.0";

  src = fetchFromGitHub {
    owner = "TimurKiyivinski";
    repo = "gnome-shell-night-light-slider-extension";
    rev = version;
    sha256 = "10jsb9gqk7gmc30389k482shcp43m69qahc889sxllsfm5v395z0";
  };

  uuid = "night-light-slider.timur@linux.com";

  buildInputs = [ glib ];

  buildPhase = ''
    glib-compile-schemas night-light-slider.timur@linux.com/schemas
  '';

  installPhase = ''
    mkdir -p $out/share/gnome-shell/extensions/${uuid}
    cd night-light-slider.timur@linux.com
    cp -r schemas $out/share/gnome-shell/extensions/${uuid}
    cp *.js *.json $out/share/gnome-shell/extensions/${uuid}
  '';

  meta = with stdenv.lib; {
    description = "GNOME night light temperature manager";
    license = licenses.gpl2;
    maintainers = with maintainers; [ acowley ];
    homepage = https://github.com/TimurKiyivinski/gnome-shell-night-light-slider-extension;
  };
}
