{ stdenv }:

let
  version = "10.11";
in stdenv.mkDerivation rec {
  name = "MacOSX10.11.sdk";

  src = fetchzip {
    url = "https://github.com/phracker/MacOSX-SDKs/releases/download/MacOSX10.11.sdk/MacOSX10.11.sdk.tar.xz";
    sha256 = "0000f40jij2z1mia091xqyky5r11r4qyh7b8172blrmgm9q23sl9";
  };

  unpackPhase    = "true";
  configurePhase = "true";
  buildPhase     = "true";
  setupHook = ./setup-hook.sh;

  installPhase = ''
    mkdir -p $out/Developer/SDKs/
    echo "Source is: $src"
    cp -r $src $out/Developer/SDKs/
  '';

  meta = with stdenv.lib; {
    description = "The Mac OS ${version} SDK";
    maintainers = with maintainers; [ copumpkin ];
    platforms   = platforms.darwin;
    license     = licenses.unfree;
  };
}
