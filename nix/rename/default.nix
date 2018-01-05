  FileRename = buildPerlPackage rec {
    name = "File-Rename-0.20";
    src = fetchurl {
      url = "mirror://cpan/authors/id/R/RM/RMBARKER/${name}.tar.gz";
      sha256 = "5eee75ea92a987930c5bec4a631ee0201f23c77908ba322e553df80845efc6b1";
    };
    buildInputs = [ ModuleBuild ];
    meta = {
      description = "Perl extension for renaming multiple files";
      license = with stdenv.lib.licenses; [ artistic1 gpl1Plus ];
    };
  };
