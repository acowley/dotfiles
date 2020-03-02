{
  allowUnfree = true;
  allowBroken = true;
  rocmTargets = ["gfx803" "gfx900" "gfx906"];
  android_sdk.accept_license = true;
  perlPackageOverrides = pkgs: {
    FileRename = pkgs.perlPackages.buildPerlModule rec {
      pname = "File-Rename";
      version = "0.20";
      src = pkgs.fetchurl {
        url = "mirror://cpan/authors/id/R/RM/RMBARKER/${pname}-${version}.tar.gz";
        sha256 = "5eee75ea92a987930c5bec4a631ee0201f23c77908ba322e553df80845efc6b1";
      };
      # buildInputs = [ pkgs.perlPackages.ModuleBuild ];
      buildInputs = [ pkgs.makeWrapper ];
      perlPostHook = ''
        wrapProgram $out/bin/rename --prefix PERL5LIB : $out/lib/perl5/site_perl
      '';
      meta = {
        description = "Perl extension for renaming multiple files";
        license = with pkgs.stdenv.lib.licenses; [ artistic1 gpl1Plus ];
      };
    };
  };
  packageOverrides = pkgs: rec {
    mylatex = pkgs.texlive.combine {
     inherit (pkgs.texlive) scheme-small algorithms cm-super
             collection-binextra collection-context
             collection-fontsrecommended collection-fontutils
             collection-mathscience collection-formatsextra
             collection-pictures subfigure web supertabular
             wrapfig capt-of footmisc subdepth preview
             minifp lettrine titling titlesec fontspec todonotes
             doublestroke comment multirow cancel doi import ifoddpage
             subfiles biblatex logreq biber was minted fvextra xstring
             framed;
    };

    alglib = pkgs.callPackage ./nix/alglib {};
    hdrmerge = pkgs.qt5.callPackage ./nix/hdrmerge {};

    # Apply a patch that makes nix-shell take a `-o` flag with which
    # one can specify an output directory. This lets you test the
    # installPhase of a package since it does not write to the store.
    nix = pkgs.nix.overrideAttrs (old: {
      patches = (old.patches or []) ++ [(pkgs.fetchpatch {
        url = "https://patch-diff.githubusercontent.com/raw/NixOS/nix/pull/3036.patch";
        sha256 = "04kxbhxs7hb7r2psq5hgr87fnhrw9vggyb2bg8wg0yfvp538ilin";
      })];
    });

    # An monospace font with cursive italics
    victor-mono = pkgs.callPackage ./nix/victor-mono.nix {};
    montserrat = pkgs.callPackage ./nix/montserrat.nix {};
    wattmangtk = pkgs.python3Packages.callPackage ./nix/wattmangtk {
    matplotlib = pkgs.python3Packages.matplotlib.override { enableGtk3 = true; };
    };
    vez = pkgs.callPackage ./nix/vez { buildSamples = true; };
    powerline-go = pkgs.powerline-go.overrideAttrs (old: {
      postPatch = ''
        sed 's@\([[:space:]]\)dotEnv := false@\1var dotEnvEnv string\n\1dotEnvEnv, _ = os.LookupEnv("DIRENV_DIR")\n\1dotEnv := dotEnvEnv != ""@' -i segment-dotenv.go
        sed -e 's/\([[:space:]]*\)\(p.appendSegment("nix-shell", segment{\)/\1var nixPrompt string\n\1if nixShell == "impure" {\n\1    nixPrompt = "nix"\n\1} else {\n\1    nixPrompt = nixShell\n\1}\n\1\2/' \
            -e 's/\([[:space:]]*content:[[:space:]]*\)nixShell,/\1nixPrompt,/' \
            -i segment-nix-shell.go
        sed -e 's/\([[:space:]]*NixShellBg:\).*/\1 22,/' \
            -e 's/\([[:space:]]*NixShellFg:\).*/\1 254,/' \
            -i defaults.go
      '';
    });
    clang-format = pkgs.callPackage ({ stdenv, writeText, libclang }:
      stdenv.mkDerivation {
        name = "clang-format";
        buildInputs = [ libclang ];
        builder = writeText "builder.sh" ''
          source $stdenv/setup
          mkdir -p $out/bin
          ln -s ${libclang.out}/bin/clang-format $out/bin/clang-format
        '';
      }) { inherit (pkgs.llvmPackages_9) libclang; };

    lz4json = pkgs.callPackage ./nix/lz4json {};

    # handbrake = pkgs.handbrake.override { useFfmpeg = true; ffmpeg = pkgs.ffmpeg-full; };
    catch2 = pkgs.callPackage ./nix/catch2 {};
    versor = pkgs.callPackage ./nix/versor {};
    liblapack_3_8 = pkgs.callPackage ./nix/liblapack/3.8.nix {};

    cparens = pkgs.haskellPackages.callPackage ~/Projects/cparens {};

    openfoam = pkgs.callPackage ./nix/openfoam {};

    zenstates = pkgs.callPackage ./nix/zenstates {};
    global = pkgs.callPackage ./nix/global {};

    clang-rf = pkgs.callPackage ./nix/clang-rf {};

    pcl_demo = pkgs.callPackage ./nix/pcl_demo {};

    vkmark = pkgs.callPackage ./nix/vkmark {};

    # easyloggingpp = pkgs.callPackage ./nix/easyloggingpp {};

    gtsam = pkgs.callPackage ./nix/gtsam {};

    cquery = pkgs.callPackage ./nix/cquery {};
    ccls = pkgs.callPackage ./nix/ccls {
      llvmPackages = pkgs.llvmPackages_latest;
    };
    ccls-project = pkgs.callPackage ./nix/ccls/nixAware.nix {
      llvmPackages = pkgs.llvmPackages_latest;
    };
    ccls-fun = buildInputs: ccls-project { inherit buildInputs; };
    ccls-nixpkgs = pkgs.ccls;

    libsimdpp = pkgs.callPackage ./nix/libsimdpp {};

    loguru = pkgs.callPackage ./nix/loguru {};

    mygnused = pkgs.stdenv.mkDerivation {
      name = "mygnused";
      buildInputs = [pkgs.gnused];
      gnused = pkgs.gnused;
      builder = builtins.toFile "builder.sh" ''
        source $stdenv/setup
        mkdir -p $out/bin
        ln -s $gnused/bin/sed $out/bin/gnused
      '';
      meta = {
        description = "GNU sed symlinked to bin/gnused";
        longDescription = ''
          OS X ships with an older version of sed. Rather than shadow
          it in your PATH, this package simply symlinks the gnused
          executable from nixpkgs to the name "gnused".
        '';
      };
    };

    octaveFull = pkgs.octaveFull.override {
      openblas = if pkgs.stdenv.isDarwin then pkgs.openblasCompat else pkgs.openblas;
      suitesparse = pkgs.suitesparse;
      jdk = null;
    };
    # fltk = pkgs.callPackage ./nix/fltk {};

    # pip2nix = pkgs.callPackage ~/Documents/Projects/pip2nix {
    #   inherit (pkgs.pythonPackages) buildPythonApplication pip;
    #   inherit (pkgs) nix cacert;
    # };

    # busybox = null;

    # uriparser = pkgs.uriparser.overrideDerivation (_: {
    #   configureFlags = ["--disable-doc"];
    # });

    # platformio = (pkgs.python27.buildEnv.override {
    #   extraLibs = let p = pkgs.python27Packages;
    #               in [ p.setuptools p.pip p.bottle p.platformio ];
    # }).env;
  };
}
