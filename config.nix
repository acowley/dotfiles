{
  allowUnfree = true;
  packageOverrides = pkgs: rec {
    gnupg = pkgs.gnupg.override { x11Support = false; };
    pass = with pkgs;
           callPackage (<nixpkgs> + /pkgs/tools/security/pass) { x11Support = false; };
    emacs = pkgs.emacs24Macport;
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

    myHaskellPackages =
      with import (<nixpkgs> + /pkgs/development/haskell-modules/lib.nix) {
        inherit pkgs;
      };
      pkgs.haskell.packages.lts-5_1.override {
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

          diagrams-builder = overrideCabal super.diagrams-builder (drv: {
            configureFlags = [ "-fps" "-frasterific" "-fogs" ];
            executableHaskellDepends = with super; [
              base bytestring cmdargs diagrams-lib
              diagrams-postscript diagrams-rasterific diagrams-svg directory
              filepath JuicyPixels lens lucid-svg
            ];
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
        lens linear vector containers criterion hmatrix foldl
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
  };
}
