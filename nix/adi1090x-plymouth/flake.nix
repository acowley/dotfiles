{
  description = "Plymouth themes";

  outputs = { self, nixpkgs }: 
    let pkg = nixpkgs.x86_64-linux.callPackage ./default.nix {}; in
    {

    # packages.x86_64-linux.adi1090x-plymouth = nixpkgs.legacyPackages.x86_64-linux.callPackage ./default.nix {};
      packages.x86_64-linux.adi1090x-plymouth = pkg;

    defaultPackage.x86_64-linux = self.packages.x86_64-linux.adi1090x-plymouth;

  };
}
