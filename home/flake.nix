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
    # my-emacs.url = "path:/home/acowley/dotfiles/my-emacs";
    my-latex.url = "path:/home/acowley/dotfiles/nix/mylatex.nix";
    my-latex.flake = false;
  };

  outputs = { self, nixpkgs, homeManager, emacs-overlay, my-emacs, my-latex }:
    let mkHome = { extraImports,
                   system ? "x86_64-linux",
                   homeDirectory ? "/home/acowley"
                 }:
          homeManager.lib.homeManagerConfiguration {
            configuration = { pkgs, lib, ... }: {
              imports = [ ./common.nix ] ++ extraImports;
              nixpkgs = {
                overlays = [
                  emacs-overlay.overlay
                  my-emacs.overlay
                  (final: prev: import my-latex final prev)
                ];
                config = { allowUnfree = true; };
              };
            };
            inherit system homeDirectory;
            username = "acowley";
            stateVersion = "21.11";
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
      legacyPackages.x86_64-linux = import nixpkgs {
        system = "x86_64-linux";
        overlays = [
          emacs-overlay.overlay
          my-emacs.overlay
          (final: prev: import my-latex final prev)
        ];
      };
    };
}
