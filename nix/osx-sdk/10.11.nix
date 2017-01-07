{ stdenv, fetchzip, darwin }:

let
  version = "10.11";
in stdenv.mkDerivation rec {
  name = "MacOSX10.11.sdk";

  src = fetchzip {
    url = "https://github.com/phracker/MacOSX-SDKs/releases/download/MacOSX10.11.sdk/MacOSX10.11.sdk.tar.xz";
    sha256 = "132vz288l6pk7ci49fcvkkmci47w451ggidh3sarm1f9m7sg7b1k";
  };

  unpackPhase    = "true";
  configurePhase = "true";
  buildPhase     = "true";
  setupHook = ./setup-hook.sh;

  propagatedBuildInputs = [ darwin.libobjc darwin.apple_sdk.libs.xpc ];

  installPhase = ''
    mkdir -p $out/Developer/SDKs/
    echo "Source is: $src"
    cp -r $src/* $out/Developer/SDKs/
  '';

  meta = with stdenv.lib; {
    description = "The Mac OS ${version} SDK";
    maintainers = with maintainers; [ copumpkin ];
    platforms   = platforms.darwin;
    license     = licenses.unfree;
  };
}
