{
  allowUnfree = true;
  allowBroken = true;
  perlPackageOverrides = pkgs: {
    FileRename = pkgs.perlPackages.buildPerlModule rec {
      name = "File-Rename-0.20";
      src = pkgs.fetchurl {
        url = "mirror://cpan/authors/id/R/RM/RMBARKER/${name}.tar.gz";
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
    global = pkgs.callPackage ./nix/global {};
    # emacs = pkgs.emacs25Macport;
    emacsMacPackagesNg = pkgs.emacsPackagesNgGen pkgs.emacs25Macport;
    emacs = emacsMacPackagesNg.emacsWithPackages (epkgs:
      with epkgs.melpaPackages; [ epkgs.pdf-tools ]
    );


    glfw31 = pkgs.callPackage ./nix/glfw/3.1.nix {};

    pcl_demo = pkgs.callPackage ./nix/pcl_demo {};

    easyloggingpp = pkgs.callPackage ./nix/easyloggingpp {};

    gtsam = pkgs.callPackage ./nix/gtsam {};

    cquery = pkgs.callPackage ./nix/cquery {};

    libsimdpp = pkgs.callPackage ./nix/libsimdpp {};

    SDL2_gpu = pkgs.callPackage ./nix/SDL_gpu {};

    python27 = pkgs.python27.override {
      packageOverrides = self: super: {
        pyserial = super.pyserial.overridePythonAttrs {
          doCheck = false;
        };
      };
    };
    python27Packages = python27.pkgs;

    loguru = pkgs.callPackage ./nix/loguru {};

    nixBufferBuilders = import (<nixpkgs> + /pkgs/build-support/emacs/buffer.nix) {
      inherit (pkgs) lib writeText;
      inherit (emacsMacPackagesNg) inherit-local;
    };

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
    # octave-image = pkgs.callPackage ./nix/octave-image {
    #   octave = octaveFull;
    # };
    fltk = pkgs.callPackage ./nix/fltk {};

    pip2nix = pkgs.callPackage ~/Documents/Projects/pip2nix {
      inherit (pkgs.pythonPackages) buildPythonApplication pip;
      inherit (pkgs) nix cacert;
    };

    # gazebo7 = pkgs.callPackage ./nix/gazebo/7.nix {};
    # sdformat4 = pkgs.callPackage (<nixpkgs> + /pkgs/development/libraries/sdformat) { };
    # sdformat = sdformat4;
    # ois = pkgs.callPackage ./nix/ois {};
    # ogre = pkgs.callPackage ./nix/ogre {};

    busybox = null;

    uriparser = pkgs.uriparser.overrideDerivation (_: {
      configureFlags = ["--disable-doc"];
    });

    platformio = (pkgs.python27.buildEnv.override {
      extraLibs = let p = pkgs.python27Packages;
                  in [ p.setuptools p.pip p.bottle p.platformio ];
    }).env;

    opencv32 = pkgs.callPackage ./nix/opencv/3.2.nix {
      enableContrib = true;
      enableEigen = true;
      enableFfmpeg = true;
      enablePython = true;
      inherit (pkgs.darwin.apple_sdk.frameworks)
        AVFoundation Cocoa QTKit VideoDecodeAcceleration;
    };

    opencv3 = pkgs.callPackage ./nix/opencv/3.x.nix {
      enableContrib = true;
      enableEigen = true;
      enableFfmpeg = true;
      enablePython = true;
      inherit (pkgs.darwin.apple_sdk.frameworks)
        AVFoundation Cocoa QTKit VideoDecodeAcceleration;
    };

    ignition = pkgs.ignition // {
      transport2 = pkgs.callPackage ./nix/ignition-transport/2.1.0.nix {
        inherit (ignition) tools messages math2;
      };
      messages = pkgs.callPackage ./nix/ignition-messages/default.nix {
        inherit (ignition) math2;
      };
      tools = pkgs.callPackage ./nix/ignition-tools/default.nix { };
    };
    kaldi = pkgs.callPackage ./nix/kaldi { };
    sctk = pkgs.callPackage ./nix/sctk { };
    openfst = pkgs.callPackage ./nix/openfst { };
    sph2pipe = pkgs.callPackage ./nix/sph2pipe { };
    atlas = pkgs.atlas.overrideDerivation (_: {
      doCheck = false;
    });

    myPythonEnv = pkgs.python3.buildEnv.override {
      extraLibs = with pkgs.python3Packages; [
        numpy scipy matplotlib networkx jupyter_console notebook
      ];};

    myREnv = pkgs.rWrapper.override {
      packages = with pkgs.rPackages;
        [ ggplot2 reshape2 ];
    };

    myNodeEnv = with pkgs;
      [ nodejs npm2nix ] ++ (with nodePackages; [ grunt-cli ]);
  };
}
