{
  allowUnfree = true;
  packageOverrides = pkgs: rec {
    gnupg = pkgs.gnupg.override { x11Support = false; };
    pass = with pkgs;
           callPackage (<nixpkgs> + /pkgs/tools/security/pass) { x11Support = false; };
    emacs = pkgs.emacs24Macport;

    myHaskellPackages = with pkgs.haskell.lib;
      (pkgs.haskell.packages.ghc7103.override {
        packageSetConfig = self: super:
          pkgs.callPackage (<nixpkgs> + /pkgs/development/haskell-modules/configuration-lts-5.1.nix) { } self (super // rec {
            hpp = pkgs.callPackage ~/Documents/Projects/hpp {
              inherit stdenv;
              inherit (super) mkDerivation base directory filepath time
                              transformers;
            };
            GLUtil = pkgs.callPackage ~/Documents/Projects/GLUtil {
              inherit (pkgs) stdenv;
              inherit (super) mkDerivation array base bytestring containers
                              directory filepath JuicyPixels linear
                              OpenGL OpenGLRaw transformers vector;
              inherit hpp;
              frameworks = pkgs.darwin.apple_sdk.frameworks;
            };
          });
        overrides = self: super: {
          diagrams-builder = overrideCabal super.diagrams-builder (drv: {
            configureFlags = [ "-fps" "-frasterific" "-fogs" ];
            executableHaskellDepends = with super; [
              base bytestring cmdargs diagrams-lib
              diagrams-postscript diagrams-rasterific diagrams-svg directory
              filepath JuicyPixels lens lucid-svg
            ];
          });
        };
      });

    #myHaskellEnv = pkgs.haskell.packages.lts-5_1.ghcWithHoogle
    myHaskellEnv = myHaskellPackages.ghcWithHoogle
      (haskellPackages: with haskellPackages; [
        cabal-install stack cabal2nix ghc-mod
        tasty tasty-hunit doctest
        lens linear vector containers criterion hmatrix
        OpenGL GLUtil # GLFW-b
        OpenCL
        JuicyPixels Rasterific
        diagrams diagrams-rasterific diagrams-builder
        Chart Chart-diagrams
        ihaskell ihaskell-charts ihaskell-diagrams
      ]);
    myPythonEnv = pkgs.python3.buildEnv.override {
      extraLibs = with pkgs.python3Packages; [
        numpy scipy matplotlib networkx jupyter_console notebook
      ];};
  };
}
