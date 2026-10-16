{
  description = ''

    A configured nixpkgs collection.
    Usage: nix registry add n /path/to/mynixpkgs
           nix run n#zoom-us
  '';

  inputs = {
    # nixpkgs.url = "/home/acowley/src/nixpkgs";
    nixpkgs.url = "/Users/acowley/src/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    my-emacs.url = "github:acowley/my-emacs";
    # nixos-rocm.url = "/home/acowley/src/nixos-rocm";
  };

  outputs = { self, nixpkgs, flake-utils, emacs-overlay, my-emacs }:
    flake-utils.lib.eachDefaultSystem (system: 
      let # overlays = import ../overlays.nix;
          overlays = [
            emacs-overlay.overlay
            my-emacs.overlay
          ];
          pkgs = import nixpkgs { 
            inherit system;
            inherit overlays;
            config = {
              allowUnfree = true;
            };
          };
      in {
        legacyPackages = pkgs;
        apps.fritzing = {
          type = "app";
          program = "${pkgs.fritzing}/bin/Fritzing";
        };
      });
}
