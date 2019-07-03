{ fetchFromGitHub, lib }:
let version = "1.2.1";
in
fetchFromGitHub {
  name = "victor-mono-${version}";
  owner = "rubjo";
  repo = "victor-mono";
  rev = "v${version}";
  sha256 = "1xvx0b6y867xbhw8jr5mjdp747154f8kyvrw16lcl3n3yyrgnlfj";
  postFetch = ''
    tar xf $downloadedFile --strip=1
    unzip public/VictorMonoAll.zip
    install -m444 -Dt $out/share/fonts/opentype/victor-mono OTF/*.otf
    install -m444 -Dt $out/share/fonts/ttf/victor-mono      TTF/*.ttf
    install -m444 -Dt $out/share/fonts/eot/victor-mono      EOT/*.eot
    install -m444 -Dt $out/share/fonts/woff/victor-mono     WOFF/*.woff       
    install -m444 -Dt $out/share/fonts/woff2/victor-mono    WOFF2/*.woff2
  '';
  meta = with lib; {
    homepage = "https://rubjo.github.io/victor-mono/";
    description = ''
      A programming font with semi-connected cursive italics and symbol ligatures.
    '';
    license = licenses.mit;
    platforms = platforms.all;
    maintainers = [ maintainers.acowley ];
  };
}
