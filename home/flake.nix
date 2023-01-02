# Usage:
# nix build .#home.activationPackage
# result/activate
#
# Or:
# home-manager switch --flake .#serve
# or home-manager switch --flake .#home
# or home-manager switch --flake .#macos
{
  inputs = {
    nixpkgs.url = "path:/home/acowley/src/nixpkgs";
    homeManager.url = "github:nix-community/home-manager";
    homeManager.inputs.nixpkgs.follows = "nixpkgs";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    my-emacs.url = "github:acowley/my-emacs";
    # my-emacs.url = "path:/Users/acowley/dotfiles/my-emacs";
    # my-emacs.url = "path:/home/acowley/dotfiles/my-emacs";
    my-latex.url = "path:/home/acowley/dotfiles/nix/mylatex.nix";
    my-latex.flake = false;
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, homeManager, emacs-overlay, my-emacs, my-latex }:
    let mkHome = { extraImports,
                   system ? "x86_64-linux",
                   homeDirectory ? "/home/acowley"
                 }:
          homeManager.lib.homeManagerConfiguration {
            pkgs = import nixpkgs {
              inherit system;
              config = {
                allowBroken = true;
                allowUnfreePredicate = (_: true);
              };
              overlays = [
                  emacs-overlay.overlay
                  my-emacs.overlay
                  (final: prev: import my-latex final prev)
                ];
            };
            modules = [./common.nix] ++ extraImports;
            # configuration = { pkgs, lib, ... }: {
            #   imports = [ ./common.nix ] ++ extraImports;
            #   # manual.manpages.enable = false;
            #   nixpkgs = {
            #     overlays = [
            #       emacs-overlay.overlay
            #       my-emacs.overlay
            #       (final: prev: import my-latex final prev)
            #     ];
            #     config = {
            #       # allowUnfree = true;
            #       allowUnfreePredicate = (_: true);
            #     };
            #   };
            # };
            # inherit system homeDirectory;
            # home.homeDirectory = homeDirectory;
            # username = "acowley";
            # stateVersion = "21.11";
          };
        mkLinux = machine: mkHome { extraImports = [ ./linux.nix machine ]; };
    in {
      homeConfigurations = {
        serve = mkLinux ./serve.nix;
        home = mkLinux ./home.nix;
        macos = mkHome {
          extraImports = [ ./macos.nix ];
          #system = "x86_64-darwin";
          system = "aarch64-darwin";
          homeDirectory = "/Users/acowley";
        };
      };
    } //
    flake-utils.lib.eachDefaultSystem (system: {
      legacyPackages = import nixpkgs {
        inherit system;
        overlays = [
          emacs-overlay.overlay
          my-emacs.overlay
          (final: prev: import my-latex final prev)
        ];
      };
    });
}
