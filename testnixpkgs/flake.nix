{
  description = ''

    A configured nixpkgs collection.
    Usage: nix registry add test /path/to/testnixpkgs
           nix shell test#hip
  '';

  inputs = {
    nixpkgs.url = "github:Flakebi/nixpkgs/rocm";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils}:
    flake-utils.lib.eachDefaultSystem (system: 
      let # overlays = import ../overlays.nix;
          pkgs = import nixpkgs { 
            inherit system;
            # inherit overlays;
            config = {
              allowUnfree = true; 
            };
          };
      in {
        legacyPackages = pkgs;
      }
    );
}
