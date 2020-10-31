{ fetchFromGitHub, lib }:
fetchFromGitHub {
  name = "nunito-2019-11-11";
  owner = "googlefonts";
  repo = "nunito";
  rev = "6d8a4e1c00df8b361e59656eee7c2b458d663191";
  sha256 = "0q0na71b90ryspn2nssfisrqrhkjd1qdjwk3z42i1s1g4gskv80b";
  postFetch = ''
    ls -a
    tar xf $downloadedFile --strip=1
    echo 'again'
    ls -a
    install -m444 -Dt $out/share/fonts/ttf/nunito fonts/TTF/*.ttf
  '';
  meta = with lib; {
    homepage = "https://github.com/googlefonts/nunito";
    description = ''
      A well balanced Sans Serif with rounded terminals designed mainly to be used as a display font.
    '';
    license = licenses.ofl;
    platforms = platforms.all;
    maintainers = [ maintainers.acowley ];
  };
}
