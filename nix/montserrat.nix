{ fetchFromGitHub, lib }:
let version = "7.200";
in
fetchFromGitHub {
  name = "montserrat-${version}";
  owner = "JulietaUla";
  repo = "Montserrat";
  rev = "v${version}";
  sha256 = "0pbkg1bqkwfhqgr9gl3jpx0pr0rc5qqg2q5nxlxsca1dzvsx6swa";
  postFetch = ''
    tar xf $downloadedFile --strip=1
    install -m444 -Dt $out/share/fonts/opentype/montserrat fonts/otf/*.otf
    install -m444 -Dt $out/share/fonts/ttf/montserrat      fonts/ttf/*.ttf
    install -m444 -Dt $out/share/fonts/webfonts/montserrat fonts/*.eot fonts/*.woff fonts/*.woff2
  '';
  meta = with lib; {
    homepage = "https://github.com/JulietaUla/Montserrat/";
    description = ''
      Inspired by posters and signs in the Montserrat neighborhood of Buenos Aires.
    '';
    license = licenses.ofl;
    platforms = platforms.all;
    maintainers = [ maintainers.acowley ];
  };
}

