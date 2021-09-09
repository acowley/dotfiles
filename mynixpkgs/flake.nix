{
  description = ''

    A configured nixpkgs collection.
    Usage: nix registry add n /path/to/mynixpkgs
           nix run n#zoom-us
  '';

  inputs = {
    nixpkgs.url = "/home/acowley/src/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    nixos-rocm.url = "/home/acowley/src/nixos-rocm";
  };

  outputs = { self, nixpkgs, flake-utils, nixos-rocm }: 
    flake-utils.lib.eachDefaultSystem (system: 
      let # overlays = [nixos-rocm.overlay];
          pkgs = import nixpkgs { 
            inherit system;
            # inherit overlays;
            config = {
              allowUnfree = true; 
            };
          };
      in {
        legacyPackages = pkgs;
      });
}
