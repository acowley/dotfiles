{
  allowUnfree = true;
  allowBroken = true;
  packageOverrides = pkgs: rec {
    gnupg = pkgs.gnupg.override { x11Support = false; };
    pass = with pkgs;
           callPackage (<nixpkgs> + /pkgs/tools/security/pass) { x11Support = false; };
    emacs = pkgs.emacs24Macport;
    emacsMacPackagesNg = pkgs.emacsPackagesNgGen emacs;
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
    luatool = pkgs.callPackage ~/Documents/Projects/BabyTimer/luatool {
      inherit (pkgs) fetchgit licenses;
      inherit (pkgs.python27Packages) buildPythonPackage pyserial;
    };

    nodemcu-uploader = pkgs.callPackage ~/Documents/Projects/BabyTimer/nodemcu-uploader {
      inherit (pkgs) fetchgit licenses;
      inherit (pkgs.python27Packages) buildPythonPackage pyserial;
    };

    esptool = pkgs.callPackage ~/Documents/Projects/BabyTimer/esptool {
      inherit (pkgs) fetchgit licenses;
      inherit (pkgs.python27Packages) buildPythonPackage pyserial;
    };

    julia = pkgs.julia.overrideDerivation (_: {
      doCheck = false;
    });

    ffmpeg-full = with pkgs;
    callPackage (<nixpkgs> + /pkgs/development/libraries/ffmpeg-full) {
      libXv = null;
      libXfixes = null;
      openglExtlib = false;
      ffplayProgram = false;
      SDL = null;
      nonfreeLicensing = true;

      faacExtlib = false;
      faac = null;
      ladspaH = null;
      celt = null;
      gsm = null;
      openjpeg_1 = null;

      libiconv = darwin.libiconv;

      # The following need to be fixed on Darwin
      frei0r = if stdenv.isDarwin then null else frei0r;
      game-music-emu = if stdenv.isDarwin then null else game-music-emu;
      libjack2 = if stdenv.isDarwin then null else libjack2;
      libmodplug = if stdenv.isDarwin then null else libmodplug;
      libvpx = if stdenv.isDarwin then null else libvpx;
      openal = if stdenv.isDarwin then null else openal;
      libpulseaudio = if stdenv.isDarwin then null else libpulseaudio;
      samba = if stdenv.isDarwin then null else samba;
      vid-stab = if stdenv.isDarwin then null else vid-stab;
      x265 = if stdenv.isDarwin then null else x265;
      xavs = if stdenv.isDarwin then null else xavs;
      inherit (darwin) CF;
      inherit (darwin.apple_sdk.frameworks)
        Cocoa CoreServices AVFoundation MediaToolbox VideoDecodeAcceleration;
    };

    # doxygen = pkgs.doxygen.overrideDerivation (oldAttrs: {
    #   NIX_CFLAGS_COMPILE=pkgs.lib.optional pkgs.stdenv.isDarwin "-mmacosx-version-min=10.9";
    # });

    busybox = null;

    libdevil = (pkgs.libdevil.override {
      mesa=null; libX11=null;
    }).overrideDerivation (_: {
        buildInputs = with pkgs;
          [ libjpeg libpng libmng lcms1 libtiff openexr ]
            ++ (if stdenv.isDarwin
                then [darwin.apple_sdk.frameworks.OpenGL]
                else [mesa libX11]);
    });

    graphviz = (pkgs.graphviz.override {
      xorg = null;
    });

    uriparser = pkgs.uriparser.overrideDerivation (_: {
      configureFlags = ["--disable-doc"];
    });

    opencv3 =
      let mypy27 = pkgs.python27.buildEnv.override {
            extraLibs = [ pkgs.python27Packages.numpy ];
          };
          mypy3 = pkgs.python3.buildEnv.override {
            extraLibs = [ pkgs.python3Packages.numpy ];
          };
      in pkgs.opencv3.overrideDerivation
      (oldAttrs: {
        buildInputs = (with pkgs; [ unzip libjpeg libpng libtiff ])
          ++ [ pkgs.eigen pkgs.bzip2 mypy27 mypy3 ];
        nativeBuildInputs = oldAttrs.nativeBuildInputs
          ++ [ pkgs.doxygen ]
          ++ pkgs.lib.optionals pkgs.stdenv.isDarwin
               (with pkgs.darwin.apple_sdk.frameworks;
                [ AVFoundation OpenCL QuartzCore QTKit AppKit Cocoa ]);
        preConfigure = ''
          NIX_CFLAGS_COMPILE=$(echo "$NIX_CFLAGS_COMPILE" | ${pkgs.gnused}/bin/sed "s,[[:space:]]*-F$NIX_STORE/[[:alnum:]]*-CF-osx-[[:digit:].]*/Library/Frameworks,,g")
          export PYTHON2_EXECUTABLE=${mypy27}/bin/python
          export PYTHON2_LIBRARY=${mypy27}/lib/libpython2.7.dylib
          export PYTHON2_INCLUDE_DIR=${mypy27}/include/python2.7
          export PYTHON2_PACKAGES_PATH=${mypy27}/lib/python2.7/site-packages
          export PYTHON3_EXECUTABLE=${mypy3}/bin/python3
          export PYTHON3_LIBRARY=${mypy3}/lib/libpython3.4m.dylib
          export PYTHON3_INCLUDE_DIR=${mypy3}/include/python3.4
          export PYTHON3_PACKAGES_PATH=${mypy3}/lib/python3.4/site-packages
          export EIGEN_ROOT=${pkgs.eigen}
          ${pkgs.gnused}/bin/sed -i 's,\(^[[:space:]]*PATH_SUFFIXES include/eigen3 include/eigen2 Eigen/include/eigen3 Eigen/include/eigen2\),\1 eigen3,' ./cmake/OpenCVFindLibsPerf.cmake
        '';
        cmakeFlags = oldAttrs.cmakeFlags ++ [ "-DWITH_FFMPEG=OFF -DWITH_QT=OFF -DWITH_QUICKTIME=OFF -DWITH_AVFOUNDATION=ON" ];
        NIX_CFLAGS_COMPILE="-DDEPLOYMENT_TARGET_MACOSX";
      });

    myHaskellPackages =
      with import (<nixpkgs> + /pkgs/development/haskell-modules/lib.nix) {
        inherit pkgs;
      };
      pkgs.haskell.packages.lts-5_9.override {
        overrides = self: super: {
            hpp = pkgs.callPackage ~/Documents/Projects/hpp {
              inherit (pkgs) stdenv;
              inherit (super) mkDerivation base directory filepath time
                              transformers;
            };
            GLUtil = pkgs.callPackage ~/Documents/Projects/GLUtil {
              inherit (pkgs) stdenv;
              inherit (super) mkDerivation array base bytestring containers
                              directory filepath JuicyPixels linear
                              OpenGL OpenGLRaw transformers vector;
              inherit (self) hpp;
              frameworks = pkgs.darwin.apple_sdk.frameworks;
            };

            ipython-kernel = pkgs.callPackage ~/src/IHaskell/ipython-kernel {
              inherit (pkgs) stdenv;
              inherit (self) mkDerivation aeson base bytestring cereal containers
                directory filepath mtl parsec process SHA temporary
                text transformers unordered-containers uuid zeromq4-haskell;
            };

            ihaskell = pkgs.callPackage ~/src/IHaskell {
              inherit (pkgs) stdenv;
              inherit (self) mkDerivation aeson base base64-bytestring bin-package-db
                 bytestring cereal cmdargs containers directory filepath ghc
                 ghc-parser ghc-paths haskeline haskell-src-exts hlint hspec
                 http-client http-client-tls HUnit ipython-kernel mtl parsec
                 process random setenv shelly split stm strict
                 system-argv0 text transformers unix unordered-containers
                 utf8-string uuid vector;
            };

            ihaskell-diagrams = pkgs.callPackage ~/src/IHaskell/ihaskell-display/ihaskell-diagrams {
              inherit (pkgs) stdenv;
              inherit (self) mkDerivation active base bytestring diagrams
                             diagrams-cairo diagrams-lib directory ihaskell
                             text;
            };

          diagrams-builder = overrideCabal super.diagrams-builder (drv: {
            configureFlags = [ "-fps" "-frasterific" "-fogs" ];
            executableHaskellDepends = with super; [
              base bytestring cmdargs diagrams-lib
              diagrams-postscript diagrams-rasterific diagrams-svg directory
              filepath JuicyPixels lens lucid-svg
            ];
          });
          Chart-diagrams = super.Chart-diagrams_1_5_4;

          mockery = super.mockery.overrideDerivation (_: {
            doCheck = false;
          });

          OpenGLRaw = overrideCabal super.OpenGLRaw (drv: {
            librarySystemDepends =
              if pkgs.stdenv.isDarwin
              then [pkgs.darwin.apple_sdk.frameworks.OpenGL]
              else super.OpenGLRaw.librarySystemDepends;
            buildDepends = with pkgs;
              lib.optionals stdenv.isDarwin [darwin.apple_sdk.frameworks.OpenGL];
          });
          GLURaw = overrideCabal super.GLURaw (drv: {
            librarySystemDepends = with pkgs;
              if stdenv.isDarwin
              then []
              else super.GLURaw.librarySystemDepends;
            buildDepends = with pkgs;
              lib.optionals stdenv.isDarwin [darwin.apple_sdk.frameworks.OpenGL];
          });
          OpenCL = overrideCabal super.OpenCL (drv: {
            librarySystemDepends = (with pkgs;
              if stdenv.isDarwin
              then [darwin.apple_sdk.frameworks.OpenCL]
              else super.OpenCL.librarySystemDepends);
            buildDepends = with pkgs;
               lib.optionals stdenv.isDarwin [darwin.apple_sdk.frameworks.OpenCL];
          });
        };
      };

    myHaskellEnv = myHaskellPackages.ghcWithHoogle
      (haskellPackages: with haskellPackages; [
        cabal-install stack cabal2nix ghc-mod
        tasty tasty-hunit doctest
        lens linear vector containers criterion foldl
        hmatrix freer pipes
        # OpenGL GLUtil # GLFW-b
        OpenCL
        JuicyPixels Rasterific
        diagrams diagrams-rasterific diagrams-builder
        Chart Chart-diagrams
        ihaskell ihaskell-charts ihaskell-diagrams ihaskell-blaze
      ]);

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
