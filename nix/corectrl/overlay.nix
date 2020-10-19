self: pkgs: {
  # hwdata = pkgs.hwdata.overrideAttrs (old: rec {
  #   version = "0.335";
  #   src = pkgs.fetchFromGitHub {
  #     owner = "vcrhonek";
  #     repo = "hwdata";
  #     rev = "v${version}";
  #     sha256 = "0f8ikwfrs6xd5sywypd9rq9cln8a0rf3vj6nm0adwzn1p8mgmrb2";
  #   };
  #   outputHash = "101lppd1805drwd038b4njr5czzjnqqxf3xlf6v3l22wfwr2cn3l";
  # });
  corectrl = self.libsForQt514.callPackage ./default.nix {};
}

