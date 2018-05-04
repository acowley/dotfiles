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
    liblapack_3_8 = pkgs.callPackage ./nix/liblapack/3.8.nix {};

    zenstates = pkgs.callPackage ./nix/zenstates {};
    global = pkgs.callPackage ./nix/global {};
    # emacs = pkgs.emacs25Macport;
    emacsMacPackagesNg = (pkgs.emacsPackagesNgGen pkgs.emacs25Macport).overrideScope (super: self: {
      use-package = self.melpaPackages.use-package;
      diminish = self.melpaPackages.diminish;
    });

    # An emacs with packages provided by nixpkgs. Pros: patches and
    # overrides have a place to live (i.e. in myEmacsPackage); Cons:
    # emacs package updates are tied to nixpkgs. Another hope was
    # faster startup times, but I found that it did not speed things
    # up much (perhaps 0.2s) and my autoloads via use-package are
    # apparently not all correct. It is easier to let package.el find
    # the autoloads and very similar in speed.
    # emacs = emacsMacPackagesNg.emacsWithPackages (epkgs:
    #   with epkgs.melpaPackages; [
    #     epkgs.pdf-tools
    #   ]
    # );

    # Overrides I usually want: local vinyl version
    haskell = pkgs.haskell // {
      packages = pkgs.haskell.packages // {
        ghc822 = pkgs.haskell.packages.ghc822.override {
          overrides = self: super: {
            vinyl = pkgs.haskell.lib.dontCheck (super.callPackage ~/Documents/Projects/VinylRecords/Vinyl {});
            intero = pkgs.haskell.lib.dontCheck (super.callPackage ~/src/intero {});
          };
        };
      };
    };

    clang-rf = pkgs.callPackage ./nix/clang-rf {};

    glfw31 = pkgs.callPackage ./nix/glfw/3.1.nix {};

    pcl_demo = pkgs.callPackage ./nix/pcl_demo {};

    vkmark = pkgs.callPackage ./nix/vkmark {};

    # easyloggingpp = pkgs.callPackage ./nix/easyloggingpp {};

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
    fltk = pkgs.callPackage ./nix/fltk {};

    mu = pkgs.mu.overrideAttrs (old: {
       # This patch causes the mu4e-view-mode-hook to be called on
       # unread messages.  See https://github.com/djcb/mu/issues/1192
       postPatch = old.postPatch or "" + ''
         sed "s/(unless mode-enabled (run-mode-hooks 'mu4e-view-mode-hook))/(run-mode-hooks 'mu4e-view-mode-hook)/" -i mu4e/mu4e-view.el
       '';
    });

    # pip2nix = pkgs.callPackage ~/Documents/Projects/pip2nix {
    #   inherit (pkgs.pythonPackages) buildPythonApplication pip;
    #   inherit (pkgs) nix cacert;
    # };

    # busybox = null;

    # uriparser = pkgs.uriparser.overrideDerivation (_: {
    #   configureFlags = ["--disable-doc"];
    # });

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

    # We can try to build only those packages whose APIs use C++
    # stdlib types against libc++. Then we can mix and match things
    # compiled against libstd++ and libc++. The downside is that
    # figuring out which libraries need special handling requires
    # getting all the way to a linker failure.
    boostclang = pkgs.boost.override {
      stdenv = pkgs.llvmPackages.libcxxStdenv;
    };
    eigenclang = pkgs.eigen.override {
      stdenv = pkgs.llvmPackages.libcxxStdenv;
    };
    gtsamclang = gtsam.override {
      stdenv = pkgs.llvmPackages.libcxxStdenv;
      eigen = eigenclang;
      boost = boostclang;
    };
    vtkclang = pkgs.vtk.override {
      stdenv = pkgs.llvmPackages.libcxxStdenv;
    };
    pclclang = pkgs.pcl.override {
      stdenv = pkgs.llvmPackages.libcxxStdenv;
      eigen = eigenclang;
      boost = boostclang;
      vtk = vtkclang;
    };
    libyamlcppclang = pkgs.libyamlcpp.override {
      stdenv = pkgs.llvmPackages.libcxxStdenv;
    };
    protobufclang = pkgs.lib.overrideDerivation (pkgs.callPackage (<nixpkgs> + /pkgs/development/libraries/protobuf/generic-v3.nix) {
      version = "3.4.0";
      sha256 = "0385j54kgr71h0cxh5vqr81qs57ack2g2k9mcdbq188v4ckjacyx";
      stdenv = pkgs.llvmPackages.libcxxStdenv;
    }) (attrs: { NIX_CFLAGS_COMPILE = "-Wno-error"; });

    opencv3clang = pkgs.callPackage ./nix/opencv/3.x.nix {
      stdenv = pkgs.llvmPackages.libcxxStdenv;
      protobuf = protobufclang;
      enableContrib = true;
      enableEigen = true;
      enableFfmpeg = true;
      enablePython = true;
      inherit (pkgs.darwin.apple_sdk.frameworks)
        AVFoundation Cocoa QTKit VideoDecodeAcceleration;
    };

    # opencv3 = pkgs.opencv3.override {
    #   enableContrib = true;
    #   enableEigen = true;
    #   enableFfmpeg = true;
    #   enablePython = true;
    # };

    opencv3nix = pkgs.opencv3;

    # opencv3x = pkgs.callPackage ./nix/opencv/331.nix {
    #   # enableContrib = true;
    #   # enableEigen = true;
    #   enableFfmpeg = true;
    #   # enablePython = true;
    #   inherit (pkgs.darwin.apple_sdk.frameworks)
    #     AVFoundation Cocoa QTKit VideoDecodeAcceleration;
    # };

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
