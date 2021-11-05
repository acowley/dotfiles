# Usage:
# nix build .#home.activationPackage
# result/activate
#
# Or:
# home-manager switch --flake .#serve
# or home-manager switch --flake .#home
{
  inputs = {
    nixpkgs.url = "path:/home/acowley/src/nixpkgs";
    homeManager.url = "github:nix-community/home-manager";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    my-emacs.url = "path:/home/acowley/dotfiles/my-emacs";
    my-latex.url = "path:/home/acowley/dotfiles/nix/mylatex.nix";
    my-latex.flake = false;
  };

  outputs = { self, nixpkgs, homeManager, emacs-overlay, my-emacs, my-latex }: 
    let mkHome = extraImports:
          homeManager.lib.homeManagerConfiguration {
            configuration = { pkgs, lib, ... }: {
              imports = [ ./home.nix ] ++ extraImports;
              nixpkgs = {
                overlays = [
                  emacs-overlay.overlay
                  my-emacs.overlay
                  (final: prev: import my-latex final prev)
                ];
                config = { allowUnfree = true; };
              };
            };
            system = "x86_64-linux";
            homeDirectory = "/home/acowley";
            username = "acowley";
            stateVersion = "21.11";
          };
    in {
      homeConfigurations = {
        serve = mkHome [./serve.nix];
        home = mkHome [];
      };
    };
}
