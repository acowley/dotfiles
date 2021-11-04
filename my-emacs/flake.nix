{
  description = "My Emacs Overlay";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }: {
      overlay = final: prev: import ./emacs.nix final prev;
  };
}
