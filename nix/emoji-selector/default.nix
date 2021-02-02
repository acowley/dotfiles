{ stdenv, lib, fetchFromGitHub }:

stdenv.mkDerivation rec {
  name = "gnome-shell-extension-emoji-selector-${version}";
  version = "11";

  src = fetchFromGitHub {
    owner = "Maestroschan";
    repo = "emoji-selector-for-gnome";
    rev = "v${version}";
    sha256 = "0b80cxzkafaqzmc4j4qbfvv0sh89mxy9r13sq13jbgknlgc671q8";
  };

  uuid = "emoji-selector@maestroschan.fr";

  installPhase = ''
    mkdir -p $out/share/gnome-shell/extensions/${uuid}
    cp -r schemas $out/share/gnome-shell/extensions/${uuid}
    cp -r icons $out/share/gnome-shell/extensions/${uuid}
    cp -r locale $out/share/gnome-shell/extensions/${uuid}
    cp *.js *.json *.css $out/share/gnome-shell/extensions/${uuid}
  '';

  meta = with lib; {
    description = "A popup emoji menu";
    license = licenses.gpl3;
    maintainers = with maintainers; [ acowley ];
    homepage = https://github.com/Maestroschan/emoji-selector-for-gnome;
  };
}
