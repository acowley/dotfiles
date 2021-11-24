{ stdenv, fetchurl, unzip }:
let commit = "311d7fa87bdf7cd5cc4210a91bac56d5512a3013";
in stdenv.mkDerivation {
  pname = "poppins";
  version = "4.003dev";
  srcs = [
    (fetchurl {
      url = "https://github.com/itfoundry/Poppins/raw/${commit}/products/Poppins-4.003-GoogleFonts-OTF.zip";
      hash = "sha256-OhpYz8SXs6V6im7FHWKACtQqULmCex4ylxNb9ehgsaA=";
    })
    (fetchurl {
      url = "https://github.com/itfoundry/Poppins/raw/${commit}/products/Poppins-4.003-GoogleFonts-TTF.zip";
      hash = "sha256-ac3z6LrOzJn9QgGG0m3VdpwATuIYA0RXs2kaHPQIlnw=";
    })
  ];
  nativeBuildInputs = [unzip];
  sourceRoot = "./";
  unpackCmd = "unzip $curSrc";
  installPhase = ''
    install -m444 -Dt $out/share/fonts/opentype/poppins *.otf
    install -m444 -Dt $out/share/fonts/ttf/poppins *.ttf
  '';
}

