{
  inputs = {
    nixpkgs.url = "path:/home/acowley/src/nixpkgs";
    homeManager.url = "github:nix-community/home-manager";
    emacs-overlay.url = "github.com:nix-community/emacs-overlay";
    my-emacs.url = "path:/home/acowley/dotfiles/emacs.nix";
    my-emacs.flake = false;
  };

  outputs = { self, nixpkgs, homeManager, emacs-overlay, my-emacs }: {
    homeManager.lib.homeManagerConfiguration {
      configuration = { pkgs, lib, ... }: {
        imports = [ ./home.nix ];
        nixpkgs = {
          overlays = [ emacs-overlay.overlay my-emacs ];
          config = { allowUnfree = true; }
        };
      };
      system = "x86_64-linux";
      homeDirectory = "/home/acowley";
      username = "acowley";
    };
  };
}
