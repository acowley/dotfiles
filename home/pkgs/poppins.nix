{ stdenv, fetchurl }:
let commit = "311d7fa87bdf7cd5cc4210a91bac56d5512a3013";
in stdenv.mkDerivation {
  pname = "poppins";
  version = "4.003dev";
  srcs = [
    (fetchurl {
      url = "https://github.com/itfoundry/Poppins/raw/${commit}/products/Poppins-4.003-GoogleFonts-OTF.zip";
      hash = "";
    })
  ];
  sourceRoot = "./";
  unpackCmd = ''
    unzip Poppins-4.003-GoogleFonts-OTF.zip
  '';
}

