{
  description = ''

    A configured nixpkgs collection.
    Usage: nix registry add n /path/to/mynixpkgs
           nix run n#zoom-us
  '';

  inputs = {
    nixpkgs.url = "/home/acowley/src/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }: 
    flake-utils.lib.eachDefaultSystem (system: 
      let pkgs = import nixpkgs { 
            inherit system; 
            config = {
              allowUnfree = true; 
            };
          };
      in {
        legacyPackages = pkgs;
      });
}
