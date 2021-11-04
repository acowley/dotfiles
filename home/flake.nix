# Usage:
# nix build .#home.activationPackage --impure
# result/activate
{
  inputs = {
    nixpkgs.url = "path:/home/acowley/src/nixpkgs";
    homeManager.url = "github:nix-community/home-manager";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    my-emacs.url = "path:/home/acowley/dotfiles/emacs.nix";
    my-emacs.flake = false;
    my-latex.url = "path:/home/acowley/dotfiles/nix/mylatex.nix";
    my-latex.flake = false;
  };

  outputs = { self, nixpkgs, homeManager, emacs-overlay, my-emacs, my-latex }: {
    home = homeManager.lib.homeManagerConfiguration {
      configuration = { pkgs, lib, ... }: {
        imports = [ ./home.nix ];
        nixpkgs = {
          overlays = [
            emacs-overlay.overlay
            (final: prev: import my-emacs final prev)
            (final: prev: import my-latex final prev)
          ];
          config = { allowUnfree = true; };
        };
      };
      system = "x86_64-linux";
      homeDirectory = "/home/acowley";
      username = "acowley";
    };
  };
}