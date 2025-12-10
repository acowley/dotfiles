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
    #nixpkgs.url = "path:/home/acowley/src/nixpkgs";
    # nixpkgs.url = "path:/Users/acowley/src/nixpkgs";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-25.11-darwin";
    #nixpkgs.url = "github:nixos/nixpkgs/release-24.11";

    # See https://github.com/nix-community/home-manager/issues/4483
    # Using nixpkgs-unstable requires home-manager master for now at least

    # homeManager.url = "github:nix-community/home-manager/release-23.05";
    # homeManager.url = "github:nix-community/home-manager/release-23.11";
    homeManager.url = "github:nix-community/home-manager/release-25.11";
    # homeManager.url = "github:nix-community/home-manager/master";
    homeManager.inputs.nixpkgs.follows = "nixpkgs";

    emacs-overlay.url = "github:nix-community/emacs-overlay";
    # my-emacs.url = "github:acowley/my-emacs";
    my-emacs.url = "path:/Users/acowley/dotfiles/my-emacs";
    # my-emacs.url = "path:/home/acowley/dotfiles/my-emacs";
    #my-latex.url = "path:/home/acowley/dotfiles/nix/mylatex.nix";
    my-latex.url = "path:/Users/acowley/dotfiles/nix/mylatex.nix";
    my-latex.flake = false;
    flake-utils.url = "github:numtide/flake-utils";
    unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    # emacs-lsp-booster.url = "github:acowley/emacs-lsp-booster-nix";
    # emacs-lsp-booster.inputs.nixpkgs.follows = "nixpkgs";
    nix-gl-host.url = "github:numtide/nix-gl-host";
  };

  outputs = { self, nixpkgs, flake-utils, homeManager, emacs-overlay, my-emacs, my-latex, unstable, nix-gl-host }:
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
                  nix-gl-host.overlays.default
                ];
            };
            modules = [./common.nix] ++ extraImports;
            extraSpecialArgs = {
              unstable = import unstable {
                inherit system;
              };
              inherit nixpkgs;
            };
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
        home-pop = mkLinux ./home-pop.nix;
        macos = mkHome rec {
          extraImports = [ (import ./macos.nix {
            unstable = import unstable { inherit system; };
          })];
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
      unstable = import unstable {
        inherit system;
        overlays = [
          emacs-overlay.overlay
          my-emacs.overlay
          (final: prev: import my-latex final prev)
        ];
      };
    });
}
