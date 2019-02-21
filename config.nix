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
      }) { inherit (pkgs.llvmPackages_6) libclang; };

    # handbrake = pkgs.handbrake.override { useFfmpeg = true; ffmpeg = pkgs.ffmpeg-full; };
    catch2 = pkgs.callPackage ./nix/catch2 {};
    versor = pkgs.callPackage ./nix/versor {};
    liblapack_3_8 = pkgs.callPackage ./nix/liblapack/3.8.nix {};

    cparens = pkgs.haskellPackages.callPackage ~/Projects/cparens {};

    zenstates = pkgs.callPackage ./nix/zenstates {};
    global = pkgs.callPackage ./nix/global {};

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
    ccls = pkgs.callPackage ./nix/ccls {};
    ccls-project = pkgs.callPackage ./nix/ccls/nixAware.nix {};

    libsimdpp = pkgs.callPackage ./nix/libsimdpp {};

    SDL2_gpu = pkgs.callPackage ./nix/SDL_gpu {};

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

    # platformio = (pkgs.python27.buildEnv.override {
    #   extraLibs = let p = pkgs.python27Packages;
    #               in [ p.setuptools p.pip p.bottle p.platformio ];
    # }).env;

    vtk8 = pkgs.callPackage ./nix/vtk/8.nix {
      inherit (pkgs.darwin) cf-private libobjc;
      inherit (pkgs.darwin.apple_sdk.libs) xpc;
      inherit (pkgs.darwin.apple_sdk.frameworks) Cocoa CoreServices DiskArbitration
        IOKit CFNetwork Security ApplicationServices
        CoreText IOSurface ImageIO OpenGL GLUT;
    };

    opencv32 = pkgs.callPackage ./nix/opencv/3.2.nix {
      enableContrib = true;
      enableEigen = true;
      enableFfmpeg = true;
      enablePython = true;
      inherit (pkgs.darwin.apple_sdk.frameworks)
        AVFoundation Cocoa QTKit VideoDecodeAcceleration;
    };

    # opencv3 = pkgs.callPackage ./nix/opencv/3.x.nix {
    #   enableContrib = true;
    #   enableEigen = true;
    #   enableFfmpeg = true;
    #   enablePython = true;
    #   inherit (pkgs.darwin.apple_sdk.frameworks)
    #     AVFoundation Cocoa QTKit VideoDecodeAcceleration;
    # };
    opencv3contrib = pkgs.opencv3.override {
      enableContrib = true;
      enableEigen = true;
      enableFfmpeg = true;
      enablePython = true;
      enableGtk3 = if (!pkgs.stdenv.isDarwin) then true else false;
    };
  };
}
