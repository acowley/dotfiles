{ stdenv, fetchFromGitHub }:

stdenv.mkDerivation rec {
  name = "gnome-shell-extension-gtile-${version}";
  version = "unstable-2018-01-29";

  src = fetchFromGitHub {
    owner = "gTile";
    repo = "gTile";
    rev = "c8c77a36b74823fd27398126a0142f6acc79b5cd";
    sha256 = "1vci1kc3d3d8xhnw78ykmmblk46ydyzjjksrs0l84rfz66wjnm57";
  };

  uuid = "gTile@vibou";

  installPhase = ''
    mkdir -p $out/share/gnome-shell/extensions/${uuid}
    cp -r schemas $out/share/gnome-shell/extensions/${uuid}
    cp -r images $out/share/gnome-shell/extensions/${uuid}
    cp *.js *.json *.css $out/share/gnome-shell/extensions/${uuid}
  '';

  meta = with stdenv.lib; {
    description = "A window tiling extension for Gnome";
    license = licenses.gpl2;
    maintainers = with maintainers; [ acowley ];
    homepage = https://github.com/gTile/gTile;
  };
}
